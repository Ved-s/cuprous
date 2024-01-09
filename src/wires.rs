use std::{collections::HashMap, num::NonZeroU32, ops::Deref, sync::Arc};

use eframe::{
    egui::{Grid, Ui},
    epaint::Color32,
};
use serde::{Deserialize, Serialize};

use crate::{
    app::Style,
    circuits::{Circuit, CircuitPin},
    containers::FixedVec,
    error::{ErrorList, OptionReport},
    state::StateCollection,
    vector::Vec2i,
    Direction2, Direction4, DirectionPosItreator, OptionalInt, OptionalNonzeroInt, RwLock, State,
};

#[derive(Debug, Clone, Copy)]
pub struct FoundWirePoint {
    pub dist: NonZeroU32,
    pub pos: Vec2i,
}
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
pub struct WireColors {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub none: Option<Color32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub r#false: Option<Color32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub r#true: Option<Color32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub error: Option<Color32>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub bundle: Option<Color32>,
}

impl WireColors {
    pub const NONE: Color32 = Color32::from_rgb(0, 0, 200);
    pub const FALSE: Color32 = Color32::from_rgb(0, 255, 0);
    pub const TRUE: Color32 = Color32::from_rgb(0, 127, 0);
    pub const ERROR: Color32 = Color32::from_rgb(200, 0, 0);
    pub const BUNDLE: Color32 = Color32::BLACK;

    pub fn is_empty(&self) -> bool {
        self.none.is_none()
            && self.r#true.is_none()
            && self.r#false.is_none()
            && self.error.is_none()
            && self.bundle.is_none()
    }

    pub fn or(&self, other: WireColors) -> WireColors {
        WireColors {
            none: self.none.or(other.none),
            r#false: self.r#false.or(other.r#false),
            r#true: self.r#true.or(other.r#true),
            error: self.error.or(other.error),
            bundle: self.bundle.or(other.bundle),
        }
    }

    pub fn none_color(&self) -> Color32 {
        self.none.unwrap_or(Self::NONE)
    }
    pub fn true_color(&self) -> Color32 {
        self.r#true.unwrap_or(Self::FALSE)
    }
    pub fn false_color(&self) -> Color32 {
        self.r#false.unwrap_or(Self::TRUE)
    }
    pub fn error_color(&self) -> Color32 {
        self.error.unwrap_or(Self::ERROR)
    }
    pub fn bundle_color(&self) -> Color32 {
        self.bundle.unwrap_or(Self::BUNDLE)
    }

    /// Returns changes
    pub fn ui(&mut self, default: Option<WireColors>, ui: &mut Ui) -> WireColorChanges {
        Grid::new("wire_colors_ui")
            .num_columns(2)
            .show(ui, |ui| {
                macro_rules! color_row {
                ($name:literal, $field:ident) => {
                    {
                        ui.label($name);
                        let mut color = match self.$field {
                            None => match default {
                                None => paste::paste!(self. [<$field _color>]()),
                                Some(d) => paste::paste!(d. [<$field _color>]()),
                            },
                            Some(c) => c
                        };
                        let mut changed = false;
                        if ui.color_edit_button_srgba(&mut color).changed() {
                            self.$field = Some(color);
                            changed = true;
                        }
                        if self.$field.is_some() {
                            if ui.button("x").clicked() {
                                self.$field = None;
                                changed = true;
                            }
                        }
                        ui.end_row();
                        changed
                    }
                };
            }
                WireColorChanges {
                    none: color_row!("None", none),
                    r#false: color_row!("False", r#false),
                    r#true: color_row!("True", r#true),
                    error: color_row!("Error", error),
                    bundle: color_row!("Bundle", bundle),
                }
            })
            .inner
    }
}

#[derive(Clone, Copy)]
pub struct WireColorChanges {
    pub none: bool,
    pub r#false: bool,
    pub r#true: bool,
    pub error: bool,
    pub bundle: bool,
}

impl WireColorChanges {
    pub fn is_empty(self) -> bool {
        !(self.none || self.r#false || self.r#true || self.error || self.bundle)
    }
}
#[derive(Debug, Default)]
pub struct Wire {
    pub id: usize,
    pub colors: WireColors,
    pub points: HashMap<Vec2i, WirePoint>,
}

impl Wire {
    pub fn color(&self, state: &State, style: &Style) -> Color32 {
        state.get_wire(self.id).color(style, Some(&self.colors))
    }

    pub fn set_point(
        &mut self,
        pos: Vec2i,
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
                        pin.write().set_wire(states, None, false, true)
                    }
                }
            }
        }

        if update_wire {
            states.update_wire(self.id, true);
        }
    }

    pub fn search_wire_point(&self, pos: Vec2i, dir: Direction4) -> Option<FoundWirePoint> {
        let (dir, forward) = dir.into_dir2();
        let current_diff_pos = dir.choose_axis_component(pos.x, pos.y);
        let current_eq_pos = dir.choose_axis_component(pos.y, pos.x);
        self.points
            .keys()
            .filter_map(|pos| {
                let target_diff_pos = dir.choose_axis_component(pos.x, pos.y);
                let target_eq_pos = dir.choose_axis_component(pos.y, pos.x);
                let pos_diff = current_diff_pos - target_diff_pos;

                if forward {
                    (target_eq_pos == current_eq_pos && pos_diff > 0)
                        .then_some((pos, pos_diff as u32))
                } else {
                    (target_eq_pos == current_eq_pos && pos_diff < 0)
                        .then_some((pos, -pos_diff as u32))
                }
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
            colors: self.colors,
        }
    }

    pub fn load(
        data: &crate::io::WireData,
        id: usize,
        circuits: &FixedVec<Arc<Circuit>>,
        errors: &mut ErrorList,
    ) -> Self {
        let mut errors = errors.enter_context(|| format!("loading wire {}", id));
        Self {
            id,
            colors: data.colors,
            points: data
                .points
                .iter()
                .map(|(pos, data)| {
                    let mut errors = errors
                        .enter_context(|| format!("loading wire point at {}, {}", pos.x, pos.y));
                    (*pos, WirePoint::load(data, circuits, &mut errors))
                })
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

    pub fn get_dir_mut(&mut self, dir: Direction2) -> &mut bool {
        match dir {
            Direction2::Up => &mut self.up,
            Direction2::Left => &mut self.left,
        }
    }

    fn save(&self) -> crate::io::WirePointData {
        crate::io::WirePointData {
            up: self.up,
            left: self.left,
            pin: self.pin.as_ref().map(|pin| {
                let pin = pin.read();
                crate::io::NamedCircuitPinIdData {
                    name: pin.name(),
                    circuit: pin.id.circuit_id,
                }
            }),
        }
    }

    fn load(
        data: &crate::io::WirePointData,
        circuits: &FixedVec<Arc<Circuit>>,
        errors: &mut ErrorList,
    ) -> Self {
        let mut errors = errors.enter_context(|| "connecting pin");
        let pin = data.pin.as_ref().and_then(|data| {
            circuits
                .get(data.circuit)
                .report_none(&mut errors, || {
                    format!("circuit {} did not exist", data.circuit)
                })
                .and_then(|circ| {
                    circ.info
                        .read()
                        .pins
                        .iter()
                        .find(|i| i.name == *data.name)
                        .report_none(&mut errors, || {
                            format!(
                                "pin {} did not exist on circuit {} {}",
                                data.name.deref(),
                                circ.id,
                                circ.ty.deref()
                            )
                        })
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
