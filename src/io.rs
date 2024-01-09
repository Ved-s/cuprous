use std::{time::Duration, collections::HashMap};

use emath::Rect;
use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    circuits::PinDirection,
    state::{UpdateTask, WireState},
    vector::{Vec2i, Vec2u}, DynStaticStr, Direction2, board::{Decoration, CircuitDesignPin, CircuitDesignStorage, CircuitDesign, CircuitDesignControl}, random_u128, wires::WireColors,
};

#[cfg(all(not(web_sys_unstable_apis), feature = "wasm"))]
pub static GLOBAL_CLIPBOARD: crate::Mutex<Option<crate::io::CopyPasteData>> = crate::Mutex::new(None); 

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
    #[serde(default = "Vec::new")]
    pub points: Vec<(Vec2i, WirePointData)>,
    #[serde(skip_serializing_if = "WireColors::is_empty", default)]
    pub colors: WireColors,
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
    #[serde(default = "Vec::new")]
    pub wires: Vec<Option<WireState>>,
    #[serde(default = "Vec::new")]
    pub circuits: Vec<Option<CircuitStateData>>,
    #[serde(default = "Vec::new")]
    pub queue: Vec<UpdateTask>,
    #[serde(default = "Vec::new")]
    pub updates: Vec<(usize, Option<Duration>)>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitDesignStoreData {
    pub current: usize,
    pub designs: Vec<Option<CircuitDesignData>>
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitDesignData {
    #[serde(default = "default_design_size")]
    pub size: Vec2u,

    #[serde(default = "Vec::new")]
    pub pins: Vec<CircuitDesignPin>,
    #[serde(default = "Vec::new")]
    pub decorations: Vec<Decoration>,
    #[serde(default = "HashMap::new")]
    pub controls: HashMap<(usize, usize), CircuitDesignControl>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct CircuitBoardData {

    #[serde(default)]
    pub name: String,

    #[serde(default = "random_u128")]
    pub uid: u128,

    #[serde(default)]
    pub wires: Vec<Option<WireData>>,

    #[serde(default)]
    pub circuits: Vec<Option<CircuitData>>,

    #[serde(default)]
    pub states: Vec<Option<StateData>>,

    #[serde(default = "default_design")]
    pub designs: CircuitDesignStoreData,

    #[serde(default)]
    pub ordered: bool,
    
    #[serde(default)]
    pub single_outer_control: bool,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct SaveStateData {
    pub boards: Vec<CircuitBoardData>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitDesignControlCopy {
    pub rect: Rect,
    pub display_name: String,
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
    pub update: Option<Duration>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(default)]
    pub design_controls: Vec<Option<CircuitDesignControlCopy>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct WirePartCopyData {
    // Bottom-right position
    pub pos: Vec2u,
    pub length: u32,
    #[serde(default)]
    pub colors: WireColors,
    pub dir: Direction2
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CopyPasteData {
    pub wires: Vec<WirePartCopyData>,
    pub circuits: Vec<CircuitCopyData>
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CircuitPropertyStoreData(pub HashMap<DynStaticStr, Intermediate>);

// TODO: circuit previews don't have proper custom data saving
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

fn default_design() -> CircuitDesignStoreData {
    CircuitDesignStorage::new(CircuitDesign::default_board_design()).save()
}

fn default_design_size() -> Vec2u {
    2.into()
}