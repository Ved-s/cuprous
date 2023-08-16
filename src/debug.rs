use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{Deref, DerefMut},
    panic::Location,
    sync::{
        atomic::{AtomicU64, Ordering},
        LockResult, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard
    },
};

use serde::{Deserialize, Serialize};

use crate::containers::FixedVec;

static DRWLOCK_ID: AtomicU64 = AtomicU64::new(0);

enum LockType {
    None,
    Read(FixedVec<Location<'static>>),
    Write(Location<'static>),
}

thread_local! {
    static LOCK_MAP: RefCell<Option<HashMap<u64, LockType>>> = RefCell::new(None);
}

fn access_map<R>(accessor: impl FnOnce(&mut HashMap<u64, LockType>) -> R) -> R {
    LOCK_MAP.with(|opt| {
        let mut opt = opt.borrow_mut();
        accessor(opt.get_or_insert(HashMap::new()))
    })
}

pub struct DebugRwLock<T: ?Sized> {
    id: u64,

    inner: RwLock<T>,
}

impl<'de, T: ?Sized + Deserialize<'de>> Deserialize<'de> for DebugRwLock<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(Self {
            id: new_drwlock_id(),
            inner: RwLock::<T>::deserialize(deserializer)?,
        })
    }
}

impl<T: ?Sized + Serialize> Serialize for DebugRwLock<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
    }
}

fn new_drwlock_id() -> u64 {
    DRWLOCK_ID.fetch_add(1, Ordering::Relaxed)
}

// unsafe impl<T> Send for DebugRwLock<T> {}
// unsafe impl<T> Sync for DebugRwLock<T> {}

impl<T> DebugRwLock<T> {
    pub fn new(value: T) -> Self {
        let id = new_drwlock_id();
        Self {
            id,
            inner: RwLock::new(value),
        }
    }
}

impl<T: ?Sized> DebugRwLock<T> {
    #[track_caller]
    pub fn read(&self) -> LockResult<DebugRwLockReadGuard<'_, T>> {
        let location = Location::caller();
        let (id, deadlock_at) = access_map(|map| {
            if let Some(r) = map.get_mut(&self.id) {
                match r {
                    LockType::None => {
                        let mut vec: FixedVec<_> = vec![].into();
                        let id = vec.first_free_pos();
                        vec.set(*location, id);
                        *r = LockType::Read(vec);
                        (id, None)
                    }
                    LockType::Read(ref mut vec) => {
                        let id = vec.first_free_pos();
                        vec.set(*location, id);
                        (id, None)
                    }
                    LockType::Write(loc) => (0, Some(*loc)),
                }
            } else {
                let mut vec: FixedVec<_> = vec![].into();
                let id = vec.first_free_pos();
                vec.set(*location, id);
                let t = LockType::Read(vec);
                map.insert(self.id, t);
                (id, None)
            }
        });

        if let Some(loc) = deadlock_at {
            panic!("deadlock! Lock already held for write! (tried to lock read)\nwrite at: {loc}\nthis read at: {location}\n")
        }

        map_result(self.inner.read(), |guard| DebugRwLockReadGuard {
            lock_id: self.id,
            id,
            inner: guard,
        })
    }

    #[track_caller]
    pub fn write(&self) -> LockResult<DebugRwLockWriteGuard<'_, T>> {
        let location = Location::caller();
        let (deadlock_at, write_held) = access_map(|map| {
            if let Some(r) = map.get_mut(&self.id) {
                match r {
                    LockType::None => {
                        *r = LockType::Write(*location);
                        (None, false)
                    }
                    LockType::Read(ref vec) => {
                        (Some(vec.iter().cloned().collect::<Vec<_>>()), false)
                    }
                    LockType::Write(loc) => (Some(vec![*loc]), true),
                }
            } else {
                let t = LockType::Write(*location);
                map.insert(self.id, t);
                (None, false)
            }
        });

        if let Some(locations) = deadlock_at {
            let reads = locations
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join("\n  ");
            if write_held {
                panic!("deadlock! Lock already held for write! (tried to lock write)\nreads at:  {reads}\nthis write at: {location}\n")
            } else {
                panic!("deadlock! Lock already held for read! (tried to lock write)\nreads at:  {reads}\nthis write at: {location}\n")
            }
        }

        map_result(self.inner.write(), |guard| DebugRwLockWriteGuard {
            lock_id: self.id,
            inner: guard,
        })
    }

}

impl<T: ?Sized> Drop for DebugRwLock<T> {
    fn drop(&mut self) {
        access_map(|map| {
            map.remove(&self.id);
        });
    }
}

pub struct DebugRwLockReadGuard<'a, T: ?Sized> {
    lock_id: u64,
    id: usize,
    inner: RwLockReadGuard<'a, T>,
}

pub struct DebugRwLockWriteGuard<'a, T: ?Sized> {
    lock_id: u64,
    inner: RwLockWriteGuard<'a, T>,
}

impl<T: ?Sized> Drop for DebugRwLockReadGuard<'_, T> {
    fn drop(&mut self) {
        access_map(|map| match map.get_mut(&self.lock_id) {
            Some(r) => match r {
                LockType::None => panic!("invalid state: dropping non-tracked debug lock"),
                LockType::Read(ref mut vec) => {
                    vec.remove(self.id);
                    if vec.is_empty() {
                        *r = LockType::None;
                    }
                }
                LockType::Write(_) => {
                    panic!("invalid state: dropping invalid debug lock (dropping Read, had Write)")
                }
            },
            None => panic!("invalid state: dropping non-tracked debug lock"),
        });
    }
}

impl<T: ?Sized> Drop for DebugRwLockWriteGuard<'_, T> {
    fn drop(&mut self) {
        access_map(|map| match map.get_mut(&self.lock_id) {
            Some(r) => match r {
                LockType::None => panic!("invalid state: dropping non-tracked debug lock"),
                LockType::Read(_) => {
                    panic!("invalid state: dropping invalid debug lock (dropping Write, had Read)")
                }
                LockType::Write(_) => *r = LockType::None,
            },
            None => panic!("invalid state: dropping non-tracked debug lock"),
        });
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for DebugRwLock<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DebugRwLock")
            .field("id", &self.id)
            .field("inner", &self.inner)
            .finish()
    }
}

impl<T: Default> Default for DebugRwLock<T> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<T> Deref for DebugRwLockReadGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner.deref()
    }
}

impl<T> Deref for DebugRwLockWriteGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.inner.deref()
    }
}

impl<T> DerefMut for DebugRwLockWriteGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.inner.deref_mut()
    }
}

pub fn map_result<T, U, F>(result: LockResult<T>, f: F) -> LockResult<U>
where
    F: FnOnce(T) -> U,
{
    match result {
        Ok(t) => Ok(f(t)),
        Err(err) => Err(PoisonError::new(f(err.into_inner()))),
    }
}

#[cfg(test)]
mod test {
    fn sync_send<T: Sync + Send>() {}

    #[test]
    fn sync_send_rwlock() {
        sync_send::<super::DebugRwLock<()>>();
    }
}
