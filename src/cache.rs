use std::{sync::Arc, collections::HashSet};

use parking_lot::Mutex;

pub static GLOBAL_STR_CACHE: StrCache = StrCache::new();

pub struct StrCache(Mutex<Option<HashSet<Arc<str>>>>);

impl StrCache {
    pub const fn new() -> Self {
        Self(Mutex::new(None))
    }

    pub fn cache(&self, str: &str) -> Arc<str> {
        let mut lock = self.0.lock();
        let set = lock.get_or_insert_with(HashSet::new);
        if let Some(arc) = set.get(str) {
            arc.clone()
        } else {
            let arc: Arc<str> = str.into();
            set.insert(arc.clone());
            arc
        }
    }
}
