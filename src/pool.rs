use std::ops::{Deref, DerefMut};

use parking_lot::Mutex;

use crate::vertex_renderer::ColoredTriangleBuffer;

pub struct Pool<T: Poolable>(Mutex<Vec<T>>);

impl<T: Poolable> Pool<T> {
    #[allow(clippy::new_without_default)] // No point in Default since it's not const
    pub const fn new() -> Self {
        Self(Mutex::new(vec![]))
    }
}

pub trait Poolable: Sized + 'static {
    fn pool() -> &'static Pool<Self>;
    fn clear(&mut self);
}

pub struct Pooled<T: Poolable>(Option<T>);

impl<T: Poolable> Pooled<T> {
    pub fn new(value: T) -> Self {
        Self(Some(value))
    }

    pub fn into_inner(mut self) -> T {
        self.0.take().expect("valid Pooled object")
    }
}

impl<T: Poolable> DerefMut for Pooled<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().expect("valid Pooled object")
    }
}

impl<T: Poolable> Deref for Pooled<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("valid Pooled object")
    }
}

impl<T: Poolable> Drop for Pooled<T> {
    fn drop(&mut self) {
        if let Some(inner) = self.0.take() {
            T::pool().0.lock().push(inner);
        }
    }
}

pub fn get_pooled<T: Poolable + Default>() -> Pooled<T> {
    Pooled::new(T::pool().0.lock().pop().unwrap_or_default())
}

pub fn get_pooled_with<T: Poolable>(maker: impl FnOnce() -> T) -> Pooled<T> {
    Pooled::new(T::pool().0.lock().pop().unwrap_or_else(maker))
}

macro_rules! generate_pool {
    ($ty:ty, $pname:ident, |$si:ident| $clear:expr) => {
        static $pname: Pool<$ty> = Pool::new();

        impl Poolable for $ty {
            fn pool() -> &'static Pool<Self> {
                &$pname
            }

            fn clear(&mut self) {
                let $si = self;
                $clear
            }
        }
    };
}

generate_pool! {
    ColoredTriangleBuffer,
    COLORED_TRIANGLE_BUFFER_POOL,
    |s| s.clear()
}
