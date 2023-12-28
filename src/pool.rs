use std::{sync::Arc, ops::{Deref, DerefMut}};

use parking_lot::Mutex;
use serde::{Deserialize, Serialize, Serializer, Deserializer};

use crate::state::WireState;

#[derive(Debug, Clone)]
pub struct ArcPool<T: ArcPoolable>(pub Arc<Mutex<Vec<T>>>);

impl<T: ArcPoolable> ArcPool<T> {
    pub fn new() -> Self {
        Self(Arc::new(Mutex::new(Vec::new())))
    }
}

impl<T: ArcPoolable> Default for ArcPool<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArcPooled<T: ArcPoolable>(pub T);

impl<T: ArcPoolable> ArcPooled<T> {
    pub fn new() -> Self {
        let mut t = T::pool().0.lock().pop().unwrap_or_else(T::new);
        t.clear();
        Self(t)
    }
}

impl<T: ArcPoolable> Default for ArcPooled<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: ArcPoolable> Clone for ArcPooled<T> {
    fn clone(&self) -> Self {
        let mut new = Self::new();
        self.clone_into(&mut new);
        new 
    }
}

impl<T: ArcPoolable + Serialize> Serialize for ArcPooled<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer {
        self.0.serialize(serializer)
    }
}

impl<'de, T: ArcPoolable + Deserialize<'de>> Deserialize<'de> for ArcPooled<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de> {
        Ok(Self(<_>::deserialize(deserializer)?))
    }
}

impl<T: ArcPoolable> Deref for ArcPooled<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ArcPoolable> DerefMut for ArcPooled<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: ArcPoolable> Drop for ArcPooled<T> {
    fn drop(&mut self) {
        self.0.clear();
        let pool = T::pool();
        let value = std::mem::replace(&mut self.0, T::new());
        pool.0.lock().push(value);
    }
}

pub trait ArcPoolable: Sized {
    fn pool() -> ArcPool<Self>;
    fn clone_into(&self, other: &mut Self);
    fn clear(&mut self);
    fn new() -> Self;
}

thread_local! {
    static WIRE_STATE_VEC_POOL: ArcPool<Vec<WireState>> = ArcPool::new();
    static BOOL_VEC_POOL: ArcPool<Vec<bool>> = ArcPool::new();
}

pub type PooledStateVec = ArcPooled<Vec<WireState>>;
pub type PooledBoolVec = ArcPooled<Vec<bool>>;

impl ArcPoolable for Vec<WireState> {
    fn pool() -> ArcPool<Self> {
        WIRE_STATE_VEC_POOL.with(|p| p.clone())
    }

    fn clone_into(&self, other: &mut Self) {
        other.clear();
        other.extend_from_slice(self);
    }

    fn clear(&mut self) {
        Vec::clear(self)
    }

    fn new() -> Self {
        Vec::new()
    }
}

impl ArcPoolable for Vec<bool> {
    fn pool() -> ArcPool<Self> {
        BOOL_VEC_POOL.with(|p| p.clone())
    }

    fn clone_into(&self, other: &mut Self) {
        other.clear();
        other.extend_from_slice(self);
    }

    fn clear(&mut self) {
        Vec::clear(self)
    }

    fn new() -> Self {
        Vec::new()
    }
}
