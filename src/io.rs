use std::{time::Duration, collections::HashMap};

use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    circuits::{PinDirection, CircuitPreview},
    state::{UpdateTask, WireState},
    vector::{Vec2i, Vec2u}, DynStaticStr, Direction2,
};

#[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))]
pub static GLOBAL_CLIPBOARD: crate::Mutex<Option<crate::io::CopyPasteData>> = crate::Mutex::new(None); 

pub trait LoadingContext {
    fn get_circuit_preview<'a>(&'a self, ty: &str) -> Option<&'a CircuitPreview>;
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
    #[serde(skip_serializing_if = "is_prop_store_empty")]
    #[serde(default)]
    pub props: CircuitPropertyStoreData,
    #[serde(skip_serializing_if = "is_unit")]
    #[serde(default)]
    pub imp: Intermediate,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitStateData {
    pub pins: Vec<Option<WireState>>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
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

    #[serde(default)]
    pub name: String,

    #[serde(default = "rand::random")]
    pub uid: u128,

    pub wires: Vec<Option<WireData>>,
    pub circuits: Vec<Option<CircuitData>>,
    pub states: Vec<Option<StateData>>,

    #[serde(default)]
    pub ordered: bool
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CircuitCopyData {
    pub ty: DynStaticStr,
    pub pos: Vec2u,
    #[serde(skip_serializing_if = "is_unit")]
    #[serde(default)]
    pub imp: Intermediate,
    #[serde(skip_serializing_if = "is_prop_store_empty")]
    #[serde(default)]
    pub props: CircuitPropertyStoreData,
    #[serde(skip_serializing_if = "is_unit")]
    #[serde(default)]
    pub internal: Intermediate,
    pub update: Option<Duration>
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct WirePartCopyData {
    // Bottom-right position
    pub pos: Vec2u,
    pub length: u32,
    pub dir: Direction2
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CopyPasteData {
    pub wires: Vec<WirePartCopyData>,
    pub circuits: Vec<CircuitCopyData>
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CircuitPropertyStoreData(pub HashMap<DynStaticStr, Intermediate>);

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CircuitPreviewData {
    #[serde(skip_serializing_if = "is_prop_store_empty")]
    #[serde(default)]
    pub props: CircuitPropertyStoreData
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CircuitPreviewCollectionData(pub HashMap<DynStaticStr, CircuitPreviewData>);

fn is_prop_store_empty(props: &CircuitPropertyStoreData) -> bool {
    props.0.is_empty()
}

fn is_unit(v: &Intermediate) -> bool {
    matches!(v, Intermediate::Unit)
}

fn is_false(v: &bool) -> bool {
    !v
}
