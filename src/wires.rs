use std::{collections::HashMap, num::NonZeroU32, sync::Arc};

use eframe::epaint::Color32;

use crate::{
    circuits::CircuitPin,
    state::StateCollection,
    vector::{Vec2i, Vector},
    Direction2, Direction4, OptionalInt, OptionalNonzeroInt, RwLock, SizeCalc, State, DirectionPosItreator,
};

#[derive(Debug, Default)]
pub struct Wire {
    pub id: usize,
    pub points: HashMap<Vec2i, WirePoint>,
}

impl Wire {
    pub fn color(&self, state: &State) -> Color32 {
        state.read_wire(self.id).color()
    }

    pub fn remove_point(&mut self, pos: Vector<2, i32>, states: &StateCollection, update_wire: bool) {
        if let Some(point) = self.points.remove(&pos) {
            if let Some(pin) = point.pin {
                pin.write().unwrap().set_wire(states, None, false, true)
            }
        }
        if update_wire {
            states.update_wire(self.id);
        }
    }

    pub fn add_point(
        &mut self,
        pos: Vector<2, i32>,
        states: Option<StateCollection>,
        point: WirePoint,
    ) {
        self.points.insert(pos, point);

        if let Some(states) = states {
            states.update_wire(self.id);
        }
    }
}

#[derive(Debug, Default)]
pub struct WirePoint {
    pub up: bool,
    pub left: bool,
    pub pin: Option<Arc<RwLock<CircuitPin>>>,
}
pub struct WirePart {
    pub pos: Vec2i,
    pub length: NonZeroU32,
    pub dir: Direction2,
}

impl WirePart {
    pub fn iter_pos(&self, include_start: bool) -> DirectionPosItreator {
        self.dir.iter_pos_along(self.pos, self.length.get() as i32, include_start)
    }
}

#[derive(Default, Clone, Copy)]
pub struct WireNode {
    pub up: OptionalNonzeroInt<u32>,
    pub left: OptionalNonzeroInt<u32>,
    pub down: OptionalNonzeroInt<u32>,
    pub right: OptionalNonzeroInt<u32>,
    pub wire: OptionalInt<usize>,
}

impl WireNode {
    pub fn is_empty(self) -> bool {
        self.wire.is_none()
            && self.up.is_none()
            && self.left.is_none()
            && self.down.is_none()
            && self.right.is_none()
    }

    pub fn get_dir(&self, dir: Direction4) -> OptionalNonzeroInt<u32> {
        match dir {
            Direction4::Up => self.up,
            Direction4::Left => self.left,
            Direction4::Down => self.down,
            Direction4::Right => self.right,
        }
    }

    pub fn get_dir_mut(&mut self, dir: Direction4) -> &mut OptionalNonzeroInt<u32> {
        match dir {
            Direction4::Up => &mut self.up,
            Direction4::Left => &mut self.left,
            Direction4::Down => &mut self.down,
            Direction4::Right => &mut self.right,
        }
    }
}

impl SizeCalc for WireNode {
    fn calc_size_inner(&self) -> usize {
        0
    }
}

#[allow(unused)]
pub struct FoundWireNode {
    pub node: WireNode,
    pub wire: usize,
    pub pos: Vec2i,
    pub distance: NonZeroU32,
}

pub enum TileWires {
    None,
    One {
        wire: usize,
        vertical: bool,
    },
    Two {
        horizontal: usize,
        vertical: usize,
    },
    Point {
        wire: usize,
        left: bool,
        up: bool,
        right: bool,
        down: bool,
    },
}

impl TileWires {
    pub fn dir(&self, dir: Direction4) -> Option<usize> {
        match self {
            TileWires::None => None,
            TileWires::One { wire, vertical } => {
                if dir.is_vertical() == *vertical {
                    Some(*wire)
                } else {
                    None
                }
            }
            TileWires::Two {
                horizontal,
                vertical,
            } => Some(if dir.is_vertical() {
                *vertical
            } else {
                *horizontal
            }),
            TileWires::Point {
                wire,
                left,
                up,
                right,
                down,
            } => {
                let dir = match dir {
                    Direction4::Up => *up,
                    Direction4::Left => *left,
                    Direction4::Down => *down,
                    Direction4::Right => *right,
                };
                if dir {
                    Some(*wire)
                } else {
                    None
                }
            }
        }
    }
}
