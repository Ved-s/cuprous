use std::collections::HashMap;

use smoldata::{raw::RawValue, SmolReadWrite};

use crate::{state::{sim::{CircuitUpdateTask, WireUpdateTask}, wires::WireState}, str::ArcStaticStr, vector::Vec2isize, Direction4};

#[derive(SmolReadWrite)]
pub struct Simulation {
    pub boards: Vec<Board>,
    pub states: Vec<BoardState>,
}

#[derive(SmolReadWrite)]
pub struct Board {
    pub uid: u128,
    pub wires: Vec<Option<Wire>>,
    pub circuits: Vec<Option<Circuit>>,
    pub states: Vec<u128>,
}

#[derive(SmolReadWrite)]
pub struct Wire {
    pub points: Vec<(Vec2isize, [bool; 4])>,
    pub connected_pins: Vec<PinId>,
}

#[derive(SmolReadWrite)]
pub struct PinId {
    pub circuit: usize,
    pub name: ArcStaticStr,
}

#[derive(SmolReadWrite)]
pub struct Circuit {
    pub id: ArcStaticStr,
    pub pos: Vec2isize,
    pub dir: Direction4,
    pub flip: bool,
    pub config: Option<RawValue>,
    pub instance: Option<RawValue>,
}

#[derive(SmolReadWrite)]
pub struct BoardState {
    pub wires: Vec<WireState>,
    pub circuits: Vec<Option<CircuitState>>,
    pub sim: BoardStateSimulation,
}

#[derive(SmolReadWrite)]
pub struct CircuitState {
    pub pins: HashMap<ArcStaticStr, WireState>,
    pub internal: Option<RawValue>,
}

#[derive(SmolReadWrite)]
pub struct BoardStateSimulation {
    pub wires: Vec<WireUpdateTask>,
    pub circuits: Vec<CircuitUpdateTask>,
}