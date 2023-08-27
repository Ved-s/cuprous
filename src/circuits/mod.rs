use std::{sync::Arc, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    state::{CircuitState, InternalCircuitState, State, StateCollection, WireState},
    time::Instant,
    vector::{Vec2i, Vec2u, Vector},
    DynStaticStr, OptionalInt, PaintContext, RwLock, board::ActiveCircuitBoard,
};

use self::props::CircuitPropertyStore;

pub mod button;
pub mod gates;
pub mod props;
pub mod pullup;
pub mod test;
pub mod freq_meter;

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[CircuitPinInfo]>,
}

#[derive(Debug, Clone, Copy)]
pub enum InternalPinDirection {
    StateDependent { default: PinDirection },
    Inside,
    Outside,
    Custom,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum PinDirection {
    Inside,

    #[default]
    Outside,
    Custom,
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
    pub(crate) wire: Option<usize>,
    pub(crate) dir: InternalPinDirection,
}

impl CircuitPin {
    pub fn direction(&self, state: &State) -> PinDirection {
        match self.dir {
            InternalPinDirection::Inside => PinDirection::Inside,
            InternalPinDirection::Outside => PinDirection::Outside,
            InternalPinDirection::Custom => PinDirection::Custom,
            InternalPinDirection::StateDependent { default } => state
                .read_circuit(self.id.circuit_id)
                .map(|cs| {
                    cs.read()
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
                    .pins
                    .get(self.id.id)
                    .copied()
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    pub fn set_input(&self, state: &State, value: WireState, update_state: bool) {
        let circuit = state.get_circuit(self.id.circuit_id);
        let mut circuit = circuit.write();

        let current = circuit.pins.get_clone(self.id.id).unwrap_or_default();
        if current == value {
            return;
        }

        circuit.pins.set(value, self.id.id);
        if update_state {
            match self.dir {
                InternalPinDirection::Custom => {
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
                states.update_wire(prev, true);
            }
            if let Some(wire) = wire {
                states.update_wire(wire, true);
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
                InternalPinDirection::Custom => {
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
                    .pins
                    .get_clone(self.pin.read().id.id)
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }

    pub fn set_output(&self, state_ctx: &CircuitStateContext, value: WireState) {
        let pin = self.pin.read();

        let current = state_ctx
            .read_circuit_state()
            .map(|arc| {
                arc.read()
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
            .pins
            .set(value, pin.id.id);
        if let Some(wire) = pin.wire {
            state_ctx.global_state.update_wire(wire, false)
        }
    }

    pub fn set_direction(&self, state_ctx: &CircuitStateContext, dir: PinDirection) {
        let (pin_id, wire) = {
            let pin = self.pin.read();
            (pin.id, pin.wire)
        };
        {
            let state = state_ctx.get_circuit_state();
            let mut state = state.write();

            if state.pin_dirs.get(pin_id.id).is_some_and(|d| *d == dir) {
                return;
            }

            state.pin_dirs.set(dir, pin_id.id);
        }

        match dir {
            PinDirection::Inside => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire, true),
                None => self.pin.read().set_input(
                    state_ctx.global_state,
                    Default::default(),
                    true,
                ),
            },
            PinDirection::Outside => state_ctx
                .global_state
                .update_circuit_signals(pin_id.circuit_id, Some(pin_id.id)),
            PinDirection::Custom => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire, true),
                None => self.pin.read().set_input(
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

pub struct Circuit {
    pub ty: DynStaticStr,
    pub id: usize,
    pub pos: Vec2i,

    pub info: Arc<RwLock<CircuitInfo>>,
    pub imp: Arc<RwLock<Box<dyn CircuitImpl>>>,
    pub props: CircuitPropertyStore,
}

impl Circuit {
    pub fn create(
        id: usize,
        pos: Vec2i,
        preview: &CircuitPreview,
        props_override: Option<CircuitPropertyStore>,
    ) -> Self {
        let mut imp = preview.imp.create_impl();
        let props = props_override.unwrap_or_else(|| preview.props.clone());
        imp.apply_props(&props, None);
        let mut pins = imp.create_pins(&props);
        for pin in pins.iter_mut().enumerate() {
            pin.1.pin.write().id = CircuitPinId::new(pin.0, id);
        }
        let info = Arc::new(RwLock::new(CircuitInfo {
            size: imp.size(&props),
            pins,
        }));

        Self {
            ty: preview.imp.type_name(),
            id,
            pos,
            info,
            imp: Arc::new(RwLock::new(imp)),
            props,
        }
    }

    pub fn save(&self) -> crate::io::CircuitData {
        let info = self.info.read();
        crate::io::CircuitData {
            ty: self.ty.clone(),
            pos: self.pos,
            pin_wires: info
                .pins
                .iter()
                .filter_map(|info| {
                    let pin = info.pin.read();
                    pin.connected_wire().map(|w| (pin.name(), w))
                })
                .collect(),
            imp: self.imp.read().save(),
            props: self.props.save(),
        }
    }

    pub fn copy(&self, pos: Vec2u, state: &State) -> crate::io::CircuitCopyData {
        let internal = state
            .read_circuit(self.id)
            .map(|c| {
                let circuit = c.read();
                circuit
                    .internal
                    .as_ref()
                    .map(|i| i.serialize())
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        crate::io::CircuitCopyData {
            ty: self.ty.clone(),
            pos,
            imp: self.imp.read().save(),
            internal,
            update: state
                .updates
                .lock()
                .iter()
                .find_map(|(id, time)| {
                    (*id == self.id).then(|| time.checked_duration_since(Instant::now()))
                })
                .flatten(),
            props: self.props.save(),
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
                .get_internal_mut(),
        )
    }

    pub fn set_update_interval(&self, interval: Option<Duration>) {
        self.global_state
            .set_circuit_update_interval(self.circuit.id, interval);
    }

    pub fn props(&self) -> &CircuitPropertyStore {
        &self.circuit.props
    }
}

#[allow(unused_variables)]
pub trait CircuitImpl: Send + Sync {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext);

    /// After calling this, consider all connected pins invalid
    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]>;

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>);

    /// Called once every period determined by `Self::update_interval`
    fn update(&self, state_ctx: &CircuitStateContext) {}

    /// Called once on circuit creation, use for update interval setup
    fn init_state(&self, state_ctx: &CircuitStateContext) {}

    /// Called after `Self::update` to determine next update timestamp
    fn update_interval(&self, state_ctx: &CircuitStateContext) -> Option<Duration> {
        None
    }

    /// Whether to automatically draw pins as small circuits
    fn draw_pin_points(&self) -> bool {
        true
    }

    /// Serialize circuit parameters. NOT for circuit state
    fn save(&self) -> serde_intermediate::Intermediate {
        ().into()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {}

    fn load_internal(
        &self,
        data: &serde_intermediate::Intermediate,
    ) -> Option<Box<dyn InternalCircuitState>> {
        None
    }

    /// Custom handler for [`PinDirection::Custom`]
    fn custom_pin_mutate_state(
        &self,
        state_ctx: &CircuitStateContext,
        pin: usize,
        state: &mut WireState,
    ) {
    }

    /// Called to determine which circuit parameters need to be recalculated
    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {}

    /// Called after all circuit parameters were successfully updated
    fn apply_props(&mut self, props: &CircuitPropertyStore, changed: Option<&str>) {}

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u;
}

pub struct CircuitPreview {
    pub imp: Box<dyn CircuitPreviewImpl>,
    pub props: CircuitPropertyStore,
}

impl CircuitPreview {
    pub fn new(imp: Box<dyn CircuitPreviewImpl>, props: CircuitPropertyStore) -> Self {
        Self { imp, props }
    }

    pub fn load_with_data(
        imp: Box<dyn CircuitPreviewImpl>,
        data: &crate::io::CircuitPreviewData,
    ) -> Self {
        let props = imp.default_props();
        props.load(&data.props);
        Self::new(imp, props)
    }

    pub fn load_new(
        &self,
        imp: &serde_intermediate::Intermediate,
        props_data: &crate::io::CircuitPropertyStoreData,
    ) -> Option<Self> {
        let imp = self.imp.load_impl_data(imp)?;
        let props = imp.default_props();
        props.load(props_data);
        Some(Self { imp, props })
    }

    pub fn from_impl(p: Box<dyn CircuitPreviewImpl>) -> Self {
        let props = p.default_props();
        Self { imp: p, props }
    }

    pub fn draw(&self, ctx: &PaintContext, in_world: bool) {
        self.imp.draw_preview(&self.props, ctx, in_world)
    }

    /// Returns None if data is equal to default
    pub fn save(&self) -> Option<crate::io::CircuitPreviewData> {
        if self.props.is_empty() {
            None
        } else {
            Some(crate::io::CircuitPreviewData {
                props: self.props.save(),
            })
        }
    }

    pub fn size(&self) -> Vec2u {
        self.imp.size(&self.props)
    }
}

pub trait CircuitPreviewImpl {
    fn type_name(&self) -> DynStaticStr;
    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool);
    fn size(&self, props: &CircuitPropertyStore) -> Vec2u;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;
    fn load_impl_data(
        &self,
        data: &serde_intermediate::Intermediate,
    ) -> Option<Box<dyn CircuitPreviewImpl>>;
    fn default_props(&self) -> CircuitPropertyStore;
}

#[derive(Default)]
pub struct CircuitNode {
    pub origin_dist: Vector<2, u32>,
    pub circuit: OptionalInt<usize>,
}

pub fn draw_pins_preview<T: Into<Vec2u>>(ctx: &PaintContext, size: impl Into<Vec2u>, pins: impl IntoIterator<Item = T>) {
    let size = size.into().convert(|v| v as f32);
    for pin in pins.into_iter() {
        let pos = ctx.rect.lerp_inside((pin.into().convert(|v| v as f32 + 0.5) / size).into());
        ctx.paint.circle_filled(pos, ActiveCircuitBoard::WIRE_THICKNESS * 0.5 * ctx.screen.scale, WireState::False.color());
    }
}
