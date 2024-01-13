#[cfg(all(not(feature = "wasm"), not(feature = "emulate_web_time")))]
pub type Instant = std::time::Instant;

#[cfg(any(feature = "wasm", feature = "emulate_web_time"))]
pub use time_web::Instant;

#[cfg(any(feature = "wasm", feature = "emulate_web_time"))]
mod time_web {
    use std::{ops::{Add, Sub, AddAssign}, time::Duration};

    fn now() -> f64 {
        web_time_provider()
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

    impl AddAssign<Duration> for Instant {
        fn add_assign(&mut self, rhs: Duration) {
            self.micros += rhs.as_micros() as u64;
        }
    }

    impl Sub for Instant {
        type Output = Duration;

        fn sub(self, rhs: Self) -> Self::Output {
            Duration::from_micros(self.micros - rhs.micros)
        }
    }

    #[cfg(not(feature = "emulate_web_time"))]
    fn web_time_provider() -> f64 {
        use wasm_bindgen::prelude::*;
        js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str("performance"))
            .expect("failed to get performance from global object")
            .unchecked_into::<web_sys::Performance>()
            .now()
    }
    #[cfg(feature = "emulate_web_time")]
    fn web_time_provider() -> f64 {
        use std::sync::OnceLock;
        type StdInstant = std::time::Instant;

        static START_OF_TIME: OnceLock<StdInstant> = OnceLock::new();
        const PRECISION_MS: f64 = 0.1;

        let start = START_OF_TIME.get_or_init(StdInstant::now);
        let duration = StdInstant::now() - *start;
        let ms = duration.as_secs_f64() * 1000.0;
        
        (ms / PRECISION_MS).floor() * PRECISION_MS
    }
}

