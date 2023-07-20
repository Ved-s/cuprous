use std::{
    collections::HashMap,
    sync::Arc, num::NonZeroU32,
};

use eframe::epaint::Color32;

use crate::{
    circuits::CircuitPin,
    vector::{Vec2i, Vector},
    OptionalInt, SizeCalc, State, state::StateCollection, RwLock,
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

    pub fn remove_point(&mut self, pos: Vector<2, i32>, states: Option<StateCollection>) {
        if let Some(point) = self.points.remove(&pos) {
            if let Some(pin) = point.pin {
                pin.write().unwrap().set_wire(None, None)
            }
        }
        if let Some(states) = states {
            states.update_wire(self.id);
        }
    }

    pub fn add_point(&mut self, pos: Vector<2, i32>, states: Option<StateCollection>, left: bool, up: bool, pin: Option<Arc<RwLock<CircuitPin>>>) {
        self.points.insert(pos, WirePoint { left, up, pin });

        if let Some(states) = states {
            states.update_wire(self.id);
        }
    }
}

#[derive(Debug, Default)]
pub struct WirePoint {
    pub left: bool,
    pub up: bool,
    pub pin: Option<Arc<RwLock<CircuitPin>>>,
}
pub struct WirePart {
    pub pos: Vec2i,
    pub length: NonZeroU32,

    /// Down and right
    pub vertical: bool,
}

#[derive(Default, Clone, Copy)]
pub struct WireNode {
    pub up: u32,
    pub left: u32,
    pub wire: OptionalInt<usize>,
}

impl WireNode {
    pub fn is_empty(self) -> bool {
        self.wire.is_none() && self.up == 0 && self.left == 0
    }
}

impl SizeCalc for WireNode {
    fn calc_size_inner(&self) -> usize {
        0
    }
}

// pub struct Wires {
//     pub wire_drag_pos: Option<Vec2i>,
//     pub nodes: Chunks2D<16, WireNode>,
//     pub wires: FixedVec<Wire>,

//     pub parts_drawn: RwLock<u32>,
// }

#[allow(unused)]
pub struct FoundWireNode {
    pub node: WireNode,
    pub wire: usize,
    pub pos: Vec2i,
    pub distance: NonZeroU32,
}

pub enum WireDirection {
    None,
    Up,
    Left,
}

pub enum TileWires {
    None,
    One { wire: usize, dir: WireDirection },
    Two { left: usize, up: usize },
}

impl TileWires {
    pub fn up(&self) -> Option<usize> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Up) || matches!(dir, WireDirection::None) =>
            {
                Some(*wire)
            }
            TileWires::Two { left: _, up } => Some(*up),
            _ => None,
        }
    }

    pub fn left(&self) -> Option<usize> {
        match self {
            TileWires::One { wire, dir }
                if matches!(dir, WireDirection::Left) || matches!(dir, WireDirection::None) =>
            {
                Some(*wire)
            }
            TileWires::Two { left, up: _ } => Some(*left),
            _ => None,
        }
    }
}
