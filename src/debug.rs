use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    panic::Location,
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc::{sync_channel, Receiver, SyncSender, RecvTimeoutError},
        Arc,
    },
    thread::ThreadId,
    time::Duration,
};

use parking_lot::*;
use serde::{Deserialize, Serialize};

use crate::{time::Instant, DynStaticStr};

static OBJECT_ID: AtomicU64 = AtomicU64::new(0);

fn new_object_id() -> u64 {
    OBJECT_ID.fetch_add(1, Ordering::Relaxed)
}

#[derive(Default)]
struct ObjectLocks {
    locations: HashMap<Location<'static>, (ThreadId, Instant)>,
    shared: bool,
}

static THREAD_SENDER: RwLock<Option<Arc<SyncSender<ObjectLockAck>>>> = RwLock::new(None);
static THREAD_DEBUG_NAMES: RwLock<Option<HashMap<ThreadId, DynStaticStr>>> = RwLock::new(None); 

pub fn set_this_thread_debug_name(name: impl Into<DynStaticStr>) {
    let id = std::thread::current().id();

    let mut lock = THREAD_DEBUG_NAMES.write();
    let map = lock.get_or_insert_with(HashMap::new);
    map.insert(id, name.into());
}

struct NamedThreadId(ThreadId);

impl NamedThreadId {
    pub fn name(&self) -> Option<DynStaticStr> {
        THREAD_DEBUG_NAMES.read().as_ref().and_then(|map| map.get(&self.0).cloned())
    }
}

impl Display for NamedThreadId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name() {
            Some(name) => f.write_fmt(format_args!("\"{}\"", name.deref())),
            None => self.0.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ObjectAction {
    Request { shared: bool, timestamp: Instant },
    Acquire { shared: bool, timestamp: Instant },
    Unlock,
}

struct ObjectLockAck {
    id: u64,
    location: Location<'static>,
    action: ObjectAction,
    thread: ThreadId,
}

fn ack_object(ack: ObjectLockAck) {
    let sender_opt = THREAD_SENDER.read();

    let sender = match sender_opt.deref() {
        Some(s) => s.clone(),
        None => {
            drop(sender_opt);
            create_thread(false)
        }
    };

    let err = sender.send(ack).err();
    let ack = unwrap_option_or_return!(err).0;

    let sender = create_thread(true);
    let err = sender.send(ack).is_err(); // If it errors, whatever, something is wrong with the thread

    if err {
        println!("[deadlock_detection] epic thread fail")
    }
}

fn create_thread(force_recreation: bool) -> Arc<SyncSender<ObjectLockAck>> {
    fn create_thread_inner() -> Arc<SyncSender<ObjectLockAck>> {
        let (send, recv) = sync_channel(16);
        std::thread::Builder::new()
            .name("deadlock detection".into())
            .spawn(move || {
                println!("[deadlock_detection] thread begin");
                watcher_thread(recv);
                println!("[deadlock_detection] thread end");
            })
            .expect("failed to spawn deadlock detection thread");
        Arc::new(send)
    }

    let mut sender_opt = THREAD_SENDER.write();
    let arc = match force_recreation {
        true => sender_opt.insert(create_thread_inner()),
        false => sender_opt.get_or_insert_with(create_thread_inner),
    };

    arc.clone()
}

const LOCK_WARNING_DURATION: Duration = Duration::from_secs(1);

fn watcher_thread(recv: Receiver<ObjectLockAck>) {
    let mut object_map = HashMap::<u64, ObjectLocks>::new();

    let mut thread_locks = HashMap::<ThreadId, HashSet<u64>>::new();
    let mut thread_waits = HashMap::<ThreadId, (u64, Instant, Location<'static>)>::new();

    let mut next_timing_check = Instant::now() + Duration::from_secs(1);
    let mut timing_warned_objects = HashSet::new();

    'main: loop {
        let timing_timeout = next_timing_check.checked_duration_since(Instant::now());
        let ack_timeout = timing_timeout.map(|timeout| recv.recv_timeout(timeout));

        let ack = match ack_timeout {
            Some(Ok(ack)) => ack,
            Some(Err(RecvTimeoutError::Disconnected)) => {
                println!("[deadlock_detection] receiver broke");
                return;
            }
            Some(Err(RecvTimeoutError::Timeout)) | None => {
                let now = Instant::now();
                for (object_id, locks) in object_map.iter() {
                    for (location, (thread, timestamp)) in locks.locations.iter() {
                        let hashkey = (*object_id, *thread);
                        if timing_warned_objects.contains(&hashkey) {
                            continue;
                        }
                        if let Some(dur) = now.checked_duration_since(*timestamp) {
                            if dur >= LOCK_WARNING_DURATION {
                                println!(
                                    "[deadlock_detection] Object {object_id} being held by {} for too long at {location}! ({:.2} s.)",
                                    NamedThreadId(*thread),
                                    dur.as_secs_f32()
                                );
                                timing_warned_objects.insert(hashkey);
                            }
                        }
                    }
                }
                let now = Instant::now();
                for (thread, (object_id, timestamp, location)) in thread_waits.iter() {
                    let hashkey = (*object_id, *thread);
                        if timing_warned_objects.contains(&hashkey) {
                            continue;
                        }
                        if let Some(dur) = now.checked_duration_since(*timestamp) {
                            if dur >= LOCK_WARNING_DURATION {
                                println!(
                                    "[deadlock_detection] {} waits too long for object {object_id} at {location}! ({:.2} s.)",
                                    NamedThreadId(*thread),
                                    dur.as_secs_f32()
                                );
                                timing_warned_objects.insert(hashkey);
                            }
                        }
                }
                next_timing_check = Instant::now() + Duration::from_secs(1);

                continue;
            },
        };

        // println!(
        //     "{:?} object {} at {:?}, {}",
        //     ack.action, ack.id, ack.thread, ack.location
        // );

        let object_locks = object_map.entry(ack.id).or_default();

        match ack.action {
            ObjectAction::Request { shared, timestamp } => 'mat: {
                timing_warned_objects.remove(&(ack.id, ack.thread));
                let locks = thread_locks.entry(ack.thread).or_default();

                if locks.contains(&ack.id)
                    && (!object_locks.shared || !shared || shared != object_locks.shared)
                {
                    let thread = ack.thread;
                    let ack_id = ack.id;
                    let ack_loc = ack.location;

                    let other_lock = object_locks
                        .locations
                        .iter()
                        .find_map(|(loc, (thread, _))| (*thread == ack.thread).then_some(loc));

                    let thread = NamedThreadId(thread);
                    match other_lock {
                        Some(loc) => println!(
                            "[deadlock_detection] possible deadlock:
 {thread} tries to lock object {ack_id} at {ack_loc}, and at {loc}"
                        ),
                        None => println!(
                            "[deadlock_detection] possible deadlock:
 {thread} tries to lock object {ack_id} at {ack_loc}, and at unknown location"
                        ),
                    }
                    continue 'main;
                }

                thread_waits.insert(ack.thread, (ack.id, timestamp, ack.location));

                let this_thread_locks = thread_locks.get(&ack.thread);
                let this_thread_locks = unwrap_option_or_break!(this_thread_locks, 'mat);
                if this_thread_locks.is_empty() {
                    break 'mat;
                }

                // if any thread that locked this object
                for (lock_location, (lock_thread, _)) in object_locks.locations.iter() {
                    // Is waiting for another object
                    if let Some((id, _, _)) = thread_waits.get(lock_thread) {
                        // And that object is locked by this thread
                        if this_thread_locks.contains(id) {
                            let thread = ack.thread;
                            let ack_id = ack.id;
                            let ack_loc = ack.location;
                            let thread = NamedThreadId(thread);

                            println!(
                                "[deadlock_detection] possible deadlock:
 {thread} tries to lock object {ack_id} at {ack_loc},
 while it is being locked by a {lock_thread:?} at {lock_location},
 waiting for object {id}"
                            );
                        }
                    }
                }
            }
            ObjectAction::Acquire { shared, timestamp } => {
                timing_warned_objects.remove(&(ack.id, ack.thread));
                if !shared || object_locks.shared != shared {
                    object_locks.locations.clear();
                    object_locks.shared = shared;
                }
                object_locks.locations.insert(ack.location, (ack.thread, timestamp));
                thread_locks.entry(ack.thread).or_default().insert(ack.id);
                thread_waits.remove(&ack.thread);
            }
            ObjectAction::Unlock => {
                if let Some((thread, timestamp)) = object_locks.locations.remove(&ack.location) {
                    if let Some(dur) = Instant::now().checked_duration_since(timestamp) {
                        if dur > LOCK_WARNING_DURATION {
                            println!(
                                "[deadlock_detection] Object {} unlocked by {} after {:.2} s.",
                                ack.id,
                                NamedThreadId(thread),
                                dur.as_secs_f32()
                            );
                        }
                    }
                }
                thread_waits.remove(&ack.thread);
                timing_warned_objects.remove(&(ack.id, ack.thread));
                if let Some(locks) = thread_locks.get_mut(&ack.thread) {
                    locks.remove(&ack.id);
                }
            }
        }
    }
}

struct GuardInfo {
    object_id: u64,
    location: Location<'static>,
    thread: ThreadId,
}

pub struct DebugRwLock<T> {
    id: u64,
    inner: RwLock<T>,
}

pub struct DebugRwLockReadGuard<'a, T> {
    inner: RwLockReadGuard<'a, T>,
    info: GuardInfo,
}

pub struct DebugRwLockWriteGuard<'a, T> {
    inner: RwLockWriteGuard<'a, T>,
    info: GuardInfo,
}

pub struct DebugMutex<T> {
    id: u64,
    inner: Mutex<T>,
}

pub struct DebugMutexGuard<'a, T> {
    inner: MutexGuard<'a, T>,
    info: GuardInfo,
}

impl<T: Default> Default for DebugRwLock<T> {
    fn default() -> Self {
        Self {
            id: new_object_id(),
            inner: Default::default(),
        }
    }
}

impl<T: Default> Default for DebugMutex<T> {
    fn default() -> Self {
        Self {
            id: new_object_id(),
            inner: Default::default(),
        }
    }
}

impl<T: Sized> DebugRwLock<T> {
    pub fn new(value: T) -> Self {
        Self {
            id: new_object_id(),
            inner: RwLock::new(value),
        }
    }
}

impl<T: Sized> DebugMutex<T> {
    pub fn new(value: T) -> Self {
        Self {
            id: new_object_id(),
            inner: Mutex::new(value),
        }
    }
}

impl<T> DebugRwLock<T> {
    #[track_caller]
    pub fn read(&self) -> DebugRwLockReadGuard<'_, T> {
        let location = *Location::caller();
        let thread = std::thread::current().id();

        let guard = match self.inner.try_read() {
            Some(guard) => guard,
            None => {
                ack_object(ObjectLockAck {
                    id: self.id,
                    location,
                    action: ObjectAction::Request { shared: true, timestamp: Instant::now() },
                    thread,
                });
                self.inner.read()
            }
        };
        ack_object(ObjectLockAck {
            id: self.id,
            location,
            action: ObjectAction::Acquire { shared: true, timestamp: Instant::now() },
            thread,
        });

        let info = GuardInfo {
            object_id: self.id,
            location,
            thread,
        };
        DebugRwLockReadGuard { inner: guard, info }
    }

    #[track_caller]
    pub fn try_read(&self) -> Option<DebugRwLockReadGuard<'_, T>> {
        self.inner.try_read().map(|guard| {
            let location = *Location::caller();
            let thread = std::thread::current().id();

            ack_object(ObjectLockAck {
                id: self.id,
                location,
                thread,
                action: ObjectAction::Acquire { shared: true, timestamp: Instant::now() },
            });

            let info = GuardInfo {
                object_id: self.id,
                location,
                thread,
            };
            DebugRwLockReadGuard { inner: guard, info }
        })
    }

    #[track_caller]
    pub fn write(&self) -> DebugRwLockWriteGuard<'_, T> {
        let location = *Location::caller();
        let thread = std::thread::current().id();

        let guard = match self.inner.try_write() {
            Some(guard) => guard,
            None => {
                ack_object(ObjectLockAck {
                    id: self.id,
                    location,
                    action: ObjectAction::Request { shared: false, timestamp: Instant::now() },
                    thread,
                });
                self.inner.write()
            }
        };
        ack_object(ObjectLockAck {
            id: self.id,
            location,
            action: ObjectAction::Acquire { shared: false, timestamp: Instant::now() },
            thread,
        });

        let info = GuardInfo {
            object_id: self.id,
            location,
            thread,
        };
        DebugRwLockWriteGuard { inner: guard, info }
    }

    #[track_caller]
    pub fn try_write(&self) -> Option<DebugRwLockWriteGuard<'_, T>> {
        self.inner.try_write().map(|guard| {
            let location = *Location::caller();
            let thread = std::thread::current().id();

            ack_object(ObjectLockAck {
                id: self.id,
                location,
                thread,
                action: ObjectAction::Acquire { shared: false, timestamp: Instant::now() },
            });

            let info = GuardInfo {
                object_id: self.id,
                location,
                thread,
            };
            DebugRwLockWriteGuard { inner: guard, info }
        })
    }
}

impl<T> DebugMutex<T> {

    #[track_caller]
    pub fn lock(&self) -> DebugMutexGuard<'_, T> {
        let location = *Location::caller();
        let thread = std::thread::current().id();

        let guard = match self.inner.try_lock() {
            Some(guard) => guard,
            None => {
                ack_object(ObjectLockAck {
                    id: self.id,
                    location,
                    action: ObjectAction::Request { shared: false, timestamp: Instant::now() },
                    thread,
                });
                self.inner.lock()
            }
        };
        ack_object(ObjectLockAck {
            id: self.id,
            location,
            action: ObjectAction::Acquire { shared: false, timestamp: Instant::now() },
            thread,
        });

        let info = GuardInfo {
            object_id: self.id,
            location,
            thread,
        };
        DebugMutexGuard { inner: guard, info }
    }
}

impl<T> Deref for DebugRwLockReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T> Deref for DebugRwLockWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T> DerefMut for DebugRwLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.deref_mut()
    }
}

impl<T> Deref for DebugMutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T> DerefMut for DebugMutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.deref_mut()
    }
}

impl<T> Drop for DebugRwLockReadGuard<'_, T> {
    fn drop(&mut self) {
        ack_object(ObjectLockAck {
            id: self.info.object_id,
            location: self.info.location,
            action: ObjectAction::Unlock,
            thread: self.info.thread,
        })
    }
}

impl<T> Drop for DebugRwLockWriteGuard<'_, T> {
    fn drop(&mut self) {
        ack_object(ObjectLockAck {
            id: self.info.object_id,
            location: self.info.location,
            action: ObjectAction::Unlock,
            thread: self.info.thread,
        })
    }
}

impl<T> Drop for DebugMutexGuard<'_, T> {
    fn drop(&mut self) {
        ack_object(ObjectLockAck {
            id: self.info.object_id,
            location: self.info.location,
            action: ObjectAction::Unlock,
            thread: self.info.thread,
        })
    }
}

impl<T: Debug> Debug for DebugRwLock<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: Debug> Debug for DebugMutex<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: Serialize> Serialize for DebugRwLock<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
    }
}

impl<T: Serialize> Serialize for DebugMutex<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for DebugRwLock<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        RwLock::<T>::deserialize(deserializer).map(|inner| Self {
            id: new_object_id(),
            inner,
        })
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for DebugMutex<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Mutex::<T>::deserialize(deserializer).map(|inner| Self {
            id: new_object_id(),
            inner,
        })
    }
}
