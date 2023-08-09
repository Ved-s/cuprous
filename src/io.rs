use std::time::Duration;

use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    circuits::{DynStaticStr, PinDirection},
    state::{UpdateTask, WireState},
    vector::Vec2i,
};

#[derive(Serialize, Deserialize)]
pub struct NamedCircuitPinIdData {
    pub name: DynStaticStr,
    pub circuit: usize,
}

#[derive(Serialize, Deserialize)]
pub struct WirePointData {
    #[serde(skip_serializing_if = "is_false")]
    pub up: bool,
    #[serde(skip_serializing_if = "is_false")]
    pub left: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pin: Option<NamedCircuitPinIdData>,
}

#[derive(Serialize, Deserialize)]
pub struct WireData {
    pub points: Vec<(Vec2i, WirePointData)>,
}

#[derive(Serialize, Deserialize)]
pub struct CircuitData {
    pub ty: DynStaticStr,
    pub pos: Vec2i,
    pub pin_wires: Vec<(DynStaticStr, usize)>,
    #[serde(skip_serializing_if = "is_unit")]
    pub imp: Intermediate,
}

#[derive(Serialize, Deserialize)]
pub struct CircuitStateData {
    pub pins: Vec<Option<WireState>>,
    pub pin_dirs: Vec<Option<PinDirection>>,
    #[serde(skip_serializing_if = "is_unit")]
    pub internal: Intermediate,
}

#[derive(Serialize, Deserialize)]
pub struct StateData {
    pub wires: Vec<Option<WireState>>,
    pub circuits: Vec<Option<CircuitStateData>>,
    pub queue: Vec<UpdateTask>,
    pub updates: Vec<(usize, Option<Duration>)>,
}

#[derive(Serialize, Deserialize)]
pub struct CircuitBoardData {
    pub wires: Vec<Option<WireData>>,
    pub circuits: Vec<Option<CircuitData>>,
    pub states: Vec<Option<StateData>>,
}

fn is_unit(v: &Intermediate) -> bool {
    matches!(v, Intermediate::Unit)
}

fn is_false(v: &bool) -> bool {
    !v
}
