use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut},
    panic::Location,
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc::{sync_channel, Receiver, Sender, SyncSender},
        Arc,
    },
    thread::{Thread, ThreadId},
};

use eframe::epaint::ahash::HashSet;
use parking_lot::*;
use serde::{Deserialize, Serialize};

use crate::containers::FixedVec;

static OBJECT_ID: AtomicU64 = AtomicU64::new(0);

fn new_object_id() -> u64 {
    OBJECT_ID.fetch_add(1, Ordering::Relaxed)
}

#[derive(Default)]
struct ObjectLocks {
    locations: HashMap<Location<'static>, ThreadId>,
    shared: bool,
}

static THREAD_SENDER: RwLock<Option<Arc<SyncSender<ObjectLockAck>>>> = RwLock::new(None);

#[derive(Debug, Clone, Copy)]
enum ObjectAction {
    Request { shared: bool },
    Acquire { shared: bool },
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

fn watcher_thread(recv: Receiver<ObjectLockAck>) {
    let mut object_map = HashMap::<u64, ObjectLocks>::new();

    let mut thread_locks = HashMap::<ThreadId, HashSet<u64>>::new();
    let mut thread_waits = HashMap::<ThreadId, u64>::new();

    'main: loop {
        let ack = match recv.recv() {
            Ok(a) => a,
            Err(_) => {
                println!("[deadlock_detection] receiver broke");
                return;
            }
        };

        // println!(
        //     "{:?} object {} at {:?}, {}",
        //     ack.action, ack.id, ack.thread, ack.location
        // );

        let object_locks = object_map.entry(ack.id).or_default();

        match ack.action {
            ObjectAction::Request { shared } => 'mat: {
                let locks = thread_locks.entry(ack.thread).or_default();

                if locks.contains(&ack.id)
                    && (!object_locks.shared || !shared || shared != object_locks.shared)
                {
                    let thread = ack.thread;
                    let ack_id = ack.id;
                    let ack_loc = ack.location;

                    let other_lock = object_locks.locations.iter().find_map(|(loc, thread)| (*thread == ack.thread).then_some(loc));

                    match other_lock {
                        Some(loc) => println!(
"[deadlock_detection] possible deadlock:
 {thread:?} tries to lock object {ack_id} at {ack_loc}, and at {loc}"),
                        None => println!(
"[deadlock_detection] possible deadlock:
 {thread:?} tries to lock object {ack_id} at {ack_loc}, and at unknown location"),
                    }
                    continue 'main;
                }

                thread_waits.insert(ack.thread, ack.id);

                let this_thread_locks = thread_locks.get(&ack.thread);
                let this_thread_locks = unwrap_option_or_break!(this_thread_locks, 'mat);
                if this_thread_locks.is_empty() {
                    break 'mat;
                }

                // if any thread that locked this object
                for (lock_location, lock_thread) in object_locks.locations.iter() {

                    // Is waiting for another object
                    if let Some(id) = thread_waits.get(lock_thread) {

                        // And that object is locked by this thread
                        if this_thread_locks.contains(id) {

                            let thread = ack.thread;
                            let ack_id = ack.id;
                            let ack_loc = ack.location;

                            println!(
"[deadlock_detection] possible deadlock:
 {thread:?} tries to lock object {ack_id} at {ack_loc},
 while it is being locked by a {lock_thread:?} at {lock_location},
 waiting for object {id}");
                        }
                    }
                }
            }
            ObjectAction::Acquire { shared } => {
                if !shared || object_locks.shared != shared {
                    object_locks.locations.clear();
                    object_locks.shared = shared;
                }
                object_locks.locations.insert(ack.location, ack.thread);
                thread_locks.entry(ack.thread).or_default().insert(ack.id);
                thread_waits.remove(&ack.thread);
            }
            // ObjectAction::SingleAcquire => {
            //     object_locks.locations.clear();
            //     object_locks.shared = false;
            //     object_locks.locations.insert(ack.location, ack.thread);
            //     thread_locks.entry(ack.thread).or_default().insert(ack.id, true);
            // }
            ObjectAction::Unlock => {
                object_locks.locations.remove(&ack.location);
                thread_waits.remove(&ack.thread);
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

impl<T: Default> Default for DebugRwLock<T> {
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
                    action: ObjectAction::Request { shared: true },
                    thread,
                });
                self.inner.read()
            }
        };
        ack_object(ObjectLockAck {
            id: self.id,
            location,
            action: ObjectAction::Acquire { shared: true },
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
                action: ObjectAction::Acquire { shared: true },
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
                    action: ObjectAction::Request { shared: false },
                    thread,
                });
                self.inner.write()
            }
        };
        ack_object(ObjectLockAck {
            id: self.id,
            location,
            action: ObjectAction::Acquire { shared: false },
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
                action: ObjectAction::Acquire { shared: false },
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

impl<T: Debug> Debug for DebugRwLock<T> {
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

impl<'de, T: Deserialize<'de>> Deserialize<'de> for DebugRwLock<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        RwLock::<T>::deserialize(deserializer).map(|inner| DebugRwLock {
            id: new_object_id(),
            inner,
        })
    }
}
