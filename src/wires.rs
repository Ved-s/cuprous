use std::{collections::HashMap, num::NonZeroU32, sync::Arc};

use eframe::epaint::Color32;


use crate::{
    circuits::{Circuit, CircuitPin},
    containers::FixedVec,
    state::StateCollection,
    vector::{Vec2i, Vector},
    Direction2, Direction4, DirectionPosItreator, OptionalInt, OptionalNonzeroInt, RwLock,
    SizeCalc, State,
};

#[derive(Debug, Clone, Copy)]
pub struct FoundWirePoint {
    pub dist: NonZeroU32,
    pub pos: Vec2i,
}

#[derive(Debug, Default)]
pub struct Wire {
    pub id: usize,
    pub points: HashMap<Vec2i, WirePoint>,
}

impl Wire {
    pub fn color(&self, state: &State) -> Color32 {
        state.read_wire(self.id).color()
    }

    pub fn set_point(
        &mut self,
        pos: Vector<2, i32>,
        states: &StateCollection,
        point: Option<WirePoint>,
        update_wire: bool,
    ) {
        match point {
            Some(point) => {
                self.points.insert(pos, point);
            }
            None => {
                if let Some(point) = self.points.remove(&pos) {
                    if let Some(pin) = point.pin {
                        pin.write().unwrap().set_wire(states, None, false, true)
                    }
                }
            }
        }

        if update_wire {
            states.update_wire(self.id);
        }
    }

    pub fn search_wire_point(
        &self,
        pos: Vector<2, i32>,
        dir: Direction2,
    ) -> Option<FoundWirePoint> {
        let current_diff_pos = dir.choose_axis_component(pos.x(), pos.y());
        let current_eq_pos = dir.choose_axis_component(pos.y(), pos.x());
        self.points
            .keys()
            .filter_map(|pos| {
                let target_diff_pos = dir.choose_axis_component(pos.x(), pos.y());
                let target_eq_pos = dir.choose_axis_component(pos.y(), pos.x());
                let pos_diff = current_diff_pos - target_diff_pos;
                (target_eq_pos == current_eq_pos && pos_diff > 0).then_some((pos, pos_diff as u32))
            })
            .min_by(|a, b| a.1.cmp(&b.1))
            .map(|(pos, dist)| FoundWirePoint {
                dist: NonZeroU32::new(dist).expect("unreachable"),
                pos: *pos,
            })
    }

    pub fn save(&self) -> crate::io::WireData {
        crate::io::WireData {
            points: self.points.iter().map(|(k, v)| (*k, v.save())).collect(),
        }
    }

    pub fn load(data: &crate::io::WireData, id: usize, circuits: &FixedVec<Circuit>) -> Self {
        Self {
            id,
            points: data
                .points
                .iter()
                .map(|(pos, data)| (*pos, WirePoint::load(data, circuits)))
                .collect(),
        }
    }
}

#[derive(Debug, Default)]
pub struct WirePoint {
    pub up: bool,
    pub left: bool,
    pub pin: Option<Arc<RwLock<CircuitPin>>>,
}

impl WirePoint {
    pub fn get_dir(&self, dir: Direction2) -> bool {
        match dir {
            Direction2::Up => self.up,
            Direction2::Left => self.left,
        }
    }

    fn save(&self) -> crate::io::WirePointData {
        crate::io::WirePointData {
            up: self.up,
            left: self.left,
            pin: self.pin.as_ref().map(|pin| {
                let pin = pin.read().unwrap();
                crate::io::NamedCircuitPinIdData {
                    name: pin.name(),
                    circuit: pin.id.circuit_id,
                }
            }),
        }
    }

    fn load(data: &crate::io::WirePointData, circuits: &FixedVec<Circuit>) -> Self {
        let pin = data.pin.as_ref().and_then(|data| {
            circuits
                .get(data.circuit)
                .and_then(|circ| {
                    circ.info
                        .read()
                        .unwrap()
                        .pins
                        .iter()
                        .find(|i| i.name == *data.name)
                        .map(|info| info.pin.clone())
                })
        });
        Self {
            up: data.up,
            left: data.left,
            pin,
        }
    }
}

pub struct WirePart {
    pub pos: Vec2i,
    pub length: NonZeroU32,
    pub dir: Direction2,
}

impl WirePart {
    pub fn iter_pos(&self, include_start: bool) -> DirectionPosItreator {
        self.dir
            .iter_pos_along(self.pos, self.length.get() as i32, include_start)
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
