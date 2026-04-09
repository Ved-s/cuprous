use std::time::Duration;

pub static SYSTEM: StdTimeProvider = StdTimeProvider;

pub trait TimeProvider {
    fn now(&self) -> Instant;
}

pub struct StdTimeProvider;

impl TimeProvider for StdTimeProvider {
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