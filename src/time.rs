#[cfg(not(feature = "wasm"))]
pub type Instant = std::time::Instant;

#[cfg(feature = "wasm")]
pub use time_web::Instant;

#[cfg(feature = "wasm")]
mod time_web {
    use std::{ops::{Add, Sub}, time::Duration};

    fn now() -> f64 {
        use wasm_bindgen::prelude::*;
        js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str("performance"))
            .expect("failed to get performance from global object")
            .unchecked_into::<web_sys::Performance>()
            .now()
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Instant {
        micros: u64,
    }

    impl Instant {
        pub fn now() -> Self {
            Self { micros: (now() * 1000.0) as u64 }
        }

        pub fn checked_duration_since(self, before: Self) -> Option<Duration> {
            self.micros.checked_sub(before.micros).map(Duration::from_micros)
        }
    }

    impl Add<Duration> for Instant {
        type Output = Instant;

        fn add(self, rhs: Duration) -> Self::Output {
            Self {
                micros: self.micros + rhs.as_micros() as u64,
            }
        }
    }

    impl Sub for Instant {
        type Output = Duration;

        fn sub(self, rhs: Self) -> Self::Output {
            Duration::from_micros(self.micros - rhs.micros)
        }
    }
}
