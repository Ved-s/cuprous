use std::{hash::Hash, ops::Deref, sync::Arc, time::Duration};

use serde::{Deserialize, Serialize, Serializer};

use crate::{
    cache::GLOBAL_STR_CACHE,
    state::{CircuitState, InternalCircuitState, State, StateCollection, WireState},
    vector::{Vec2i, Vec2u, Vector},
    OptionalInt, PaintContext, RwLock,
};

pub mod button;
pub mod gates;
pub mod pullup;
pub mod test;

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[CircuitPinInfo]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CustomPinHandler {
    pub mutate_state: fn(ctx: &State, pin: &CircuitPin, state: &mut WireState),
    pub write_state: fn(ctx: &State, pin: &CircuitPin, state: WireState),
}

#[derive(Debug, Clone, Copy)]
pub enum InternalPinDirection {
    StateDependent { default: PinDirection },
    Inside,
    Outside,
    Custom(CustomPinHandler),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum PinDirection {
    Inside,

    #[default]
    Outside,
    
    // TODO: move handler to circuit impl
    #[serde(skip)]
    Custom(CustomPinHandler),
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct CircuitPinId {
    pub id: usize,
    pub circuit_id: usize,
}

impl CircuitPinId {
    pub fn new(id: usize, circuit_id: usize) -> Self {
        Self { id, circuit_id }
    }
}

#[derive(Debug)]
pub struct CircuitPin {
    name: DynStaticStr,
    pub id: CircuitPinId,
    wire: Option<usize>,
    dir: InternalPinDirection,
}

impl CircuitPin {
    pub fn direction(&self, state: &State) -> PinDirection {
        match self.dir {
            InternalPinDirection::Inside => PinDirection::Inside,
            InternalPinDirection::Outside => PinDirection::Outside,
            InternalPinDirection::Custom(h) => PinDirection::Custom(h),
            InternalPinDirection::StateDependent { default } => state
                .read_circuit(self.id.circuit_id)
                .map(|cs| {
                    cs.read()
                        .unwrap()
                        .pin_dirs
                        .get(self.id.id)
                        .cloned()
                        .unwrap_or(default)
                })
                .unwrap_or(default),
        }
    }

    pub fn get_state(&self, state: &State) -> WireState {
        state
            .read_circuit(self.id.circuit_id)
            .map(|cs| {
                cs.read()
                    .unwrap()
                    .pins
                    .get(self.id.id)
                    .copied()
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    pub fn set_input(&self, state: &State, value: WireState, update_state: bool) {
        let circuit = state.get_circuit(self.id.circuit_id);
        let mut circuit = circuit.write().unwrap();

        let current = circuit.pins.get_clone(self.id.id).unwrap_or_default();
        if current == value {
            return;
        }

        circuit.pins.set(value, self.id.id);
        if update_state {
            match self.dir {
                InternalPinDirection::Custom(_) => {
                    state.update_pin_input(self.id.circuit_id, self.id.id)
                }

                _ => state.update_circuit_signals(self.id.circuit_id, Some(self.id.id)),
            }
        }
    }

    pub fn name(&self) -> DynStaticStr {
        self.name.clone()
    }

    pub fn connected_wire(&self) -> Option<usize> {
        self.wire
    }

    pub fn set_wire(
        &mut self,
        states: &StateCollection,
        wire: Option<usize>,
        update_wire: bool,
        update_input: bool,
    ) {
        if self.wire == wire {
            return;
        }

        let prev = self.wire;

        self.wire = wire;

        if update_wire {
            if let Some(prev) = prev {
                states.update_wire(prev);
            }
            if let Some(wire) = wire {
                states.update_wire(wire);
            }
        }
        if update_input {
            match self.dir {
                InternalPinDirection::StateDependent { default: _ } => {
                    states.update_pin_input(self.id.circuit_id, self.id.id);
                }
                InternalPinDirection::Outside => {}
                InternalPinDirection::Inside => {
                    states.update_pin_input(self.id.circuit_id, self.id.id);
                }
                InternalPinDirection::Custom(_) => {
                    states.update_pin_input(self.id.circuit_id, self.id.id);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CircuitPinInfo {
    pub name: DynStaticStr,
    pub pos: Vec2u,
    pub pin: Arc<RwLock<CircuitPin>>,
}

impl CircuitPinInfo {
    pub fn get_input(&self, state_ctx: &CircuitStateContext) -> WireState {
        state_ctx
            .read_circuit_state()
            .map(|cs| {
                cs.read()
                    .unwrap()
                    .pins
                    .get_clone(self.pin.read().unwrap().id.id)
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    pub fn set_output(&self, state_ctx: &CircuitStateContext, value: WireState) {
        let pin = self.pin.read().unwrap();

        let current = state_ctx
            .read_circuit_state()
            .map(|arc| {
                arc.read()
                    .unwrap()
                    .pins
                    .get_clone(pin.id.id)
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        if current == value {
            return;
        }

        state_ctx
            .get_circuit_state()
            .write()
            .unwrap()
            .pins
            .set(value, pin.id.id);
        if let Some(wire) = pin.wire {
            state_ctx.global_state.update_wire(wire)
        }
    }

    pub fn set_direction(&self, state_ctx: &CircuitStateContext, dir: PinDirection) {
        let (pin_id, wire) = {
            let pin = self.pin.read().unwrap();
            (pin.id, pin.wire)
        };
        {
            let state = state_ctx.get_circuit_state();
            let mut state = state.write().unwrap();

            if state.pin_dirs.get(pin_id.id).is_some_and(|d| *d == dir) {
                return;
            }

            state.pin_dirs.set(dir, pin_id.id);
        }

        match dir {
            PinDirection::Inside => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire),
                None => self.pin.read().unwrap().set_input(
                    state_ctx.global_state,
                    Default::default(),
                    true,
                ),
            },
            PinDirection::Outside => state_ctx
                .global_state
                .update_circuit_signals(pin_id.circuit_id, Some(pin_id.id)),
            PinDirection::Custom(_) => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire),
                None => self.pin.read().unwrap().set_input(
                    state_ctx.global_state,
                    Default::default(),
                    true,
                ),
            },
        }
    }
}

impl CircuitPinInfo {
    fn new(
        pos: impl Into<Vec2u>,
        dir: InternalPinDirection,
        name: impl Into<DynStaticStr>,
    ) -> Self {
        let name = name.into();
        Self {
            pos: pos.into(),
            pin: Arc::new(RwLock::new(CircuitPin {
                id: Default::default(),
                dir,
                wire: None,
                name: name.clone(),
            })),
            name,
        }
    }
}

#[derive(Clone, Debug)]
pub enum DynStaticStr {
    Static(&'static str),
    Dynamic(Arc<str>),
}

impl Serialize for DynStaticStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.deref())
    }
}

impl<'de> Deserialize<'de> for DynStaticStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let str = <_>::deserialize(deserializer)?;
        Ok(Self::Dynamic(GLOBAL_STR_CACHE.cache(str)))
    }
}

impl Hash for DynStaticStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state)
    }
}

impl Eq for DynStaticStr {}

impl PartialEq<str> for DynStaticStr {
    fn eq(&self, other: &str) -> bool {
        self.deref() == other
    }
}

impl<T: PartialEq<str>> PartialEq<T> for DynStaticStr {
    fn eq(&self, other: &T) -> bool {
        other.eq(self.deref())
    }
}

impl Deref for DynStaticStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            DynStaticStr::Static(str) => str,
            DynStaticStr::Dynamic(arc) => arc.deref(),
        }
    }
}

impl From<&'static str> for DynStaticStr {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl From<Arc<str>> for DynStaticStr {
    fn from(value: Arc<str>) -> Self {
        Self::Dynamic(value)
    }
}

#[derive(Serialize)]
pub struct Circuit {
    pub ty: DynStaticStr,
    #[serde(skip)]
    pub id: usize,
    pub pos: Vec2i,

    #[serde(rename = "pin_wires")]
    #[serde(serialize_with = "serialize_circuit_info")]
    pub info: Arc<RwLock<CircuitInfo>>,

    #[serde(serialize_with = "serialize_circuit_impl")]
    pub imp: Arc<RwLock<Box<dyn CircuitImpl>>>,
}

fn serialize_circuit_info<S: Serializer>(
    info: &Arc<RwLock<CircuitInfo>>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let info = info.read().unwrap();

    let map: Vec<_> = info
        .pins
        .iter()
        .filter_map(|info| {
            let pin = info.pin.read().unwrap();
            pin.connected_wire().map(|wire| (info.name.clone(), wire))
        })
        .collect();
    serializer.collect_map(map)
}
fn serialize_circuit_impl<S: Serializer>(
    imp: &Arc<RwLock<Box<dyn CircuitImpl>>>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    imp.read().unwrap().serialize().serialize(serializer)
}

impl Circuit {
    pub fn create(id: usize, pos: Vec2i, preview: &dyn CircuitPreview) -> Self {
        let imp = preview.create_impl();
        let mut pins = imp.create_pins();
        for pin in pins.iter_mut().enumerate() {
            pin.1.pin.write().unwrap().id = CircuitPinId::new(pin.0, id);
        }

        Self {
            id,
            pos,
            info: Arc::new(RwLock::new(CircuitInfo {
                size: preview.size(),
                pins,
            })),
            imp: Arc::new(RwLock::new(imp)),
            ty: preview.type_name(),
        }
    }

    pub fn save(&self) -> crate::io::CircuitData {
        let info = self.info.read().unwrap();
        crate::io::CircuitData {
            ty: self.ty.clone(),
            pos: self.pos,
            pin_wires: info.pins.iter().filter_map(|info| {
                let pin = info.pin.read().unwrap();
                pin.connected_wire().map(|w| (pin.name(), w))
            }).collect(),
            imp: self.imp.read().unwrap().serialize(),
        }
    }
}

pub struct CircuitStateContext<'a> {
    pub global_state: &'a State,
    pub circuit: &'a Circuit,
}

impl<'a> CircuitStateContext<'a> {
    pub fn new(state: &'a State, circuit: &'a Circuit) -> Self {
        Self {
            global_state: state,
            circuit,
        }
    }

    pub fn read_circuit_state(&self) -> Option<Arc<RwLock<CircuitState>>> {
        self.global_state.read_circuit(self.circuit.id)
    }

    pub fn get_circuit_state(&self) -> Arc<RwLock<CircuitState>> {
        self.global_state.get_circuit(self.circuit.id)
    }

    pub fn read_circuit_internal_state<T: InternalCircuitState, R>(
        &self,
        reader: impl FnOnce(&T) -> R,
    ) -> Option<R> {
        Some(reader(
            self.global_state
                .read_circuit(self.circuit.id)?
                .read()
                .unwrap()
                .get_internal()?,
        ))
    }

    pub fn write_circuit_internal_state<T: InternalCircuitState + Default, R>(
        &self,
        writer: impl FnOnce(&mut T) -> R,
    ) -> R {
        writer(
            self.global_state
                .get_circuit(self.circuit.id)
                .write()
                .unwrap()
                .get_internal_mut(),
        )
    }

    pub fn set_update_interval(&self, interval: Option<Duration>) {
        self.global_state
            .set_circuit_update_interval(self.circuit.id, interval);
    }
}

#[allow(unused_variables)]
pub trait CircuitImpl: Send + Sync {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext);

    fn create_pins(&self) -> Box<[CircuitPinInfo]>;

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>);

    /// Called once every period determined by `Self::update_interval`
    fn update(&mut self, state_ctx: &CircuitStateContext) {}

    /// Called once on circuit creation, use for update interval setup
    fn init_state(&self, state_ctx: &CircuitStateContext) {}

    /// Called after `Self::update` to determine next update timestamp
    fn update_interval(&self, state_ctx: &CircuitStateContext) -> Option<Duration> {
        None
    }

    fn draw_pin_points(&self) -> bool {
        true
    }

    fn serialize(&self) -> serde_intermediate::Intermediate {
        ().into()
    }
}
pub trait CircuitPreview {
    fn type_name(&self) -> DynStaticStr;
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool);
    fn size(&self) -> Vec2u;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;
}

#[derive(Default)]
pub struct CircuitNode {
    pub origin_dist: Vector<2, u32>,
    pub circuit: OptionalInt<usize>,
}
