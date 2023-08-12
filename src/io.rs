use std::time::Duration;

use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    circuits::{DynStaticStr, PinDirection, CircuitPreview},
    state::{UpdateTask, WireState},
    vector::Vec2i,
};

pub trait LoadingContext {
    fn get_circuit_preview<'a>(&'a self, ty: &DynStaticStr) -> Option<&'a dyn CircuitPreview>;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct NamedCircuitPinIdData {
    pub name: DynStaticStr,
    pub circuit: usize,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WirePointData {
    #[serde(skip_serializing_if = "is_false")]
    #[serde(default)]
    pub up: bool,
    #[serde(skip_serializing_if = "is_false")]
    #[serde(default)]
    pub left: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub pin: Option<NamedCircuitPinIdData>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WireData {
    pub points: Vec<(Vec2i, WirePointData)>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitData {
    pub ty: DynStaticStr,
    pub pos: Vec2i,
    pub pin_wires: Vec<(DynStaticStr, usize)>,
    #[serde(skip_serializing_if = "is_unit")]
    #[serde(default)]
    pub imp: Intermediate,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitStateData {
    pub pins: Vec<Option<WireState>>,
    pub pin_dirs: Vec<Option<PinDirection>>,
    #[serde(skip_serializing_if = "is_unit")]
    #[serde(default)]
    pub internal: Intermediate,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct StateData {
    pub wires: Vec<Option<WireState>>,
    pub circuits: Vec<Option<CircuitStateData>>,
    pub queue: Vec<UpdateTask>,
    pub updates: Vec<(usize, Option<Duration>)>,
}

#[derive(Serialize, Deserialize, Debug)]
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
