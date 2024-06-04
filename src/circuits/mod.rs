use parking_lot::RwLock;

use crate::vector::{Vec2isize, Vec2usize};


pub struct Circuit {
    pub id: usize,
    pub pos: Vec2isize,
    pub info: RwLock<CircuitInfo>,
}

pub struct CircuitInfo {
    pub size: Vec2usize,
}