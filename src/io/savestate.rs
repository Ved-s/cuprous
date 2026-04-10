use std::collections::{BTreeMap, HashMap};

use smoldata::{SmolReadWrite, raw::RawValue};

use crate::{
    Direction4,
    state::{sim::UpdateTask, wires::WireState},
    str::ArcStaticStr,
    vector::Vec2isize,
};

#[derive(SmolReadWrite)]
pub struct Board {
    pub uid: u128,
    pub name: String,
    pub wires: Vec<Option<Wire>>,
    pub components: Vec<Option<Component>>,
    pub states: Vec<u128>,
}

#[derive(SmolReadWrite)]
pub struct Wire {
    pub points: Vec<(Vec2isize, [bool; 4])>,
    pub connected_pins: Vec<PinId>,
}

#[derive(SmolReadWrite)]
pub struct PinId {
    pub component: usize,
    pub name: ArcStaticStr,
}

#[derive(SmolReadWrite)]
pub struct Component {
    pub id: ArcStaticStr,
    pub pos: Vec2isize,
    pub dir: Direction4,
    pub flip: bool,
    pub config: Option<RawValue>,
    pub instance: Option<RawValue>,
}

#[derive(SmolReadWrite)]
pub struct BoardState {
    pub uid: u128,
    pub wires: Vec<WireState>,
    pub components: Vec<Option<ComponentState>>,
    pub sim: BoardStateSimulation,
}

#[derive(SmolReadWrite)]
pub struct ComponentState {
    pub pins: HashMap<ArcStaticStr, WireState>,
    pub internal: Option<RawValue>,
}

#[derive(Default, SmolReadWrite)]
pub struct BoardStateSimulation {
    pub tasks: Vec<Option<UpdateTask>>,
    pub updates: BTreeMap<usize, (u128, Option<u128>)>,
}
