
#[cfg(not(target_arch = "wasm32"))]
pub use std_time::*;

#[cfg(target_arch = "wasm32")]
pub use web::*;

pub trait TimeProvider {
    fn now(&self) -> Instant;
}

#[cfg(not(target_arch = "wasm32"))]
mod std_time {
    use std::time::Duration;

    pub static SYSTEM: StdTimeProvider = StdTimeProvider;

    pub struct StdTimeProvider;

    impl super::TimeProvider for StdTimeProvider {
        fn now(&self) -> Instant {
            Instant(std::time::Instant::now())
        }
    }

    #[derive(Clone, Copy)]
    pub struct Instant(std::time::Instant);

    impl Instant {
        pub fn checked_duration_since(&self, earlier: Instant) -> Option<Duration> {
            self.0.checked_duration_since(earlier.0)
        }
    }

    impl std::ops::Sub<Instant> for Instant {
        type Output = Duration;

        fn sub(self, rhs: Instant) -> Self::Output {
            self.0 - rhs.0
        }
    }

    impl std::ops::Add<Duration> for Instant {
        type Output = Instant;

        fn add(self, rhs: Duration) -> Self::Output {
            Self(self.0 + rhs)
        }
    }

    impl std::cmp::PartialEq for Instant {
        fn eq(&self, other: &Self) -> bool {
            self.0.eq(&other.0)
        }
    }

    impl std::cmp::PartialOrd for Instant {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl std::cmp::Eq for Instant {}

    impl std::cmp::Ord for Instant {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.cmp(&other.0)
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod web {
    use std::time::Duration;

    pub static SYSTEM: WebTimeProvider = WebTimeProvider;

    pub struct WebTimeProvider;

    impl super::TimeProvider for WebTimeProvider {
        fn now(&self) -> Instant {
            Instant(performance_now())
        }
    }

    #[wasm_bindgen::prelude::wasm_bindgen]
    extern "C" {

        #[wasm_bindgen(js_namespace = "performance", js_name = "now")]
        fn performance_now() -> f64;
    }

    #[derive(Clone, Copy)]
    pub struct Instant(
        #[cfg(not(target_arch = "wasm32"))] std::time::Instant, f64,
    );

    impl Instant {
        pub fn checked_duration_since(&self, earlier: Instant) -> Option<Duration> {
            if self.0 < earlier.0 {
                None
            } else {
                Some(Duration::from_secs_f64((self.0 - earlier.0) * 1000.0))
            }
        }
    }

    impl std::ops::Sub<Instant> for Instant {
        type Output = Duration;

        fn sub(self, rhs: Instant) -> Self::Output {
            Duration::from_secs_f64((self.0 - rhs.0) * 1000.0)
        }
    }

    impl std::ops::Add<Duration> for Instant {
        type Output = Instant;

        fn add(self, rhs: Duration) -> Self::Output {
            Self(self.0 + rhs.as_secs_f64() * 1000.0)
        }
    }

    impl std::cmp::PartialEq for Instant {
        fn eq(&self, other: &Self) -> bool {
            self.0.eq(&other.0)
        }
    }

    impl std::cmp::PartialOrd for Instant {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl std::cmp::Eq for Instant {}

    impl std::cmp::Ord for Instant {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0
                .partial_cmp(&other.0)
                .unwrap_or(std::cmp::Ordering::Equal)
        }
    }
}
