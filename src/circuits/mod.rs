use std::{
    any::{Any, TypeId},
    ops::{Deref, DerefMut, Not},
    sync::Arc,
    time::Duration,
};

use eframe::{egui::Id, epaint::Color32};
use emath::Rect;
use serde::{Deserialize, Serialize};
use serde_intermediate::Intermediate;

use crate::{
    app::{SimulationContext, Style},
    board::CircuitBoard,
    containers::FixedVec,
    error::ErrorList,
    io::{CircuitCopyData, CircuitDesignControlCopy},
    state::{CircuitState, InternalCircuitState, State, StateCollection, VisitedItem, VisitedList, WireState},
    string::StringFormatterState,
    time::Instant,
    unwrap_option_or_continue,
    vector::{Vec2i, Vec2u},
    ArcString, Direction4, DynStaticStr, OptionalInt, PaintContext, RwLock,
};

use self::props::CircuitPropertyStore;

pub mod board;
pub mod bundler;
pub mod button;
pub mod freq_meter;
pub mod gates;
pub mod led;
pub mod pin;
#[macro_use]
pub mod props;
pub mod clock;
pub mod pullup;
pub mod relay;
pub mod transistor;

// so templates are always valid

#[cfg(test)]
#[path = "../../templates/circuit_template.rs"]
mod circuit_template;

#[cfg(test)]
#[path = "../../templates/directional_circuit_template.rs"]
mod directional_circuit_template;

#[derive(Default)]
pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[CircuitPinInfo]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq)]
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
                .read_circuit(self.id.circuit_id, |cs| {
                    cs.pin_dirs.get(self.id.id).cloned().unwrap_or(default)
                })
                .unwrap_or(default),
        }
    }

    pub fn get_state(&self, state: &State) -> WireState {
        state
            .read_circuit(self.id.circuit_id, |cs| {
                cs.pins.get(self.id.id).cloned().unwrap_or_default()
            })
            .unwrap_or_default()
    }

    pub fn set_input(
        &self,
        state: &Arc<State>,
        value: &WireState,
        update_state: bool,
        visited_items: Option<&mut VisitedList>,
    ) {
        let skip = state.write_circuit(self.id.circuit_id, |circuit| {
            let current = circuit.pins.get(self.id.id);
            if !matches!(self.dir, InternalPinDirection::Custom)
                && current.is_some_and(|c| *c == *value)
            {
                return true;
            }

            let state = circuit
                .pins
                .get_or_create_mut(self.id.id, || WireState::None);
            *state = value.clone();
            false
        });
        if !skip && update_state {
            match self.dir {
                InternalPinDirection::Custom => {
                    state.update_pin_input(self.id.circuit_id, self.id.id, true, visited_items)
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
    pub display_name: DynStaticStr,
    pub display_dir: Option<Direction4>,
    pub name: DynStaticStr,
    pub pos: Vec2u,
    pub pin: Arc<RwLock<CircuitPin>>,
}

impl CircuitPinInfo {
    pub fn get_state(&self, state_ctx: &CircuitStateContext) -> WireState {
        state_ctx
            .read_circuit_state(|cs| cs.pins.get_clone(self.pin.read().id.id).unwrap_or_default())
            .unwrap_or_default()
    }

    pub fn connected_wire(&self) -> Option<usize> {
        self.pin.read().connected_wire()
    }

    pub fn connected_wire_color(
        &self,
        state_ctx: &CircuitStateContext,
        style: &Style,
    ) -> Option<Color32> {
        self.pin
            .read()
            .connected_wire()
            .map(|w| state_ctx.global_state.get_wire_color(w, style))
    }

    pub fn wire_or_self_color(&self, state_ctx: &CircuitStateContext, style: &Style) -> Color32 {
        self.connected_wire_color(state_ctx, style)
            .unwrap_or_else(|| self.get_state(state_ctx).color(style, None))
    }

    pub fn get_wire_state(&self, state_ctx: &CircuitStateContext) -> Option<WireState> {
        self.pin
            .read()
            .connected_wire()
            .map(|wire| state_ctx.global_state.get_wire(wire))
    }

    pub fn set_state(&self, state_ctx: &CircuitStateContext, value: WireState) {
        let pin = self.pin.read();

        let current = state_ctx
            .read_circuit_state(|cs| cs.pins.get_clone(pin.id.id).unwrap_or_default())
            .unwrap_or_default();
        if current == value {
            return;
        }

        state_ctx.write_circuit_state(|cs| {
            cs.pins.set(pin.id.id, value);
        });
        if let Some(wire) = pin.wire {
            state_ctx.global_state.update_wire(wire, false)
        }
    }

    /// Writes this state into pin's state, without triggering any updates
    pub fn set_raw_state(&self, state_ctx: &CircuitStateContext, value: WireState) {
        state_ctx.write_circuit_state(|cs| {
            cs.pins.set(self.pin.read().id.id, value);
        });
    }

    pub fn get_direction(&self, state_ctx: &CircuitStateContext) -> PinDirection {
        self.pin.read().direction(&state_ctx.global_state)
    }

    pub fn set_direction(&self, state_ctx: &CircuitStateContext, dir: PinDirection) {
        let (pin_id, wire) = {
            let pin = self.pin.read();
            (pin.id, pin.wire)
        };
        {
            state_ctx.write_circuit_state(|cs| {
                if cs.pin_dirs.get(pin_id.id).is_some_and(|d| *d == dir) {
                    return;
                }

                cs.pin_dirs.set(pin_id.id, dir);
            });
        }

        match dir {
            PinDirection::Inside => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire, true),
                None => self.pin.read().set_input(
                    &state_ctx.global_state,
                    &Default::default(),
                    true,
                    None,
                ),
            },
            PinDirection::Outside => state_ctx
                .global_state
                .update_circuit_signals(pin_id.circuit_id, Some(pin_id.id)),
            PinDirection::Custom => match wire {
                Some(wire) => state_ctx.global_state.update_wire(wire, true),
                None => self.pin.read().set_input(
                    &state_ctx.global_state,
                    &Default::default(),
                    true,
                    None,
                ),
            },
        }
    }

    fn mutate_wire_state(
        &self,
        ctx: &CircuitStateContext,
        state: &mut WireState,
        visited_items: &mut VisitedList,
    ) {
        let Some(wire) = self.connected_wire() else { return; };

        visited_items.push(ctx.circuit.board.uid, VisitedItem::Pin(self.pin.read().id));
        ctx.global_state.compute_wire_state(wire, state, visited_items);
        visited_items.pop(ctx.circuit.board.uid);
    }

    fn apply_wire_state(
        &self,
        ctx: &CircuitStateContext,
        state: &WireState,
        skip_state_ckeck: bool,
        visited_items: &mut VisitedList,
    ) {
        self.set_raw_state(ctx, state.clone());
        let Some(wire) = self.connected_wire() else { return; };

        visited_items.push(ctx.circuit.board.uid, VisitedItem::Pin(self.pin.read().id));
        ctx.global_state.apply_wire_state(wire, state, skip_state_ckeck, visited_items);
        visited_items.pop(ctx.circuit.board.uid);
    }
}

impl CircuitPinInfo {
    fn new(
        pos: impl Into<Vec2u>,
        dir: InternalPinDirection,
        name: impl Into<DynStaticStr>,
        display_name: impl Into<DynStaticStr>,
        display_dir: impl Into<Option<Direction4>>,
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
            display_name: display_name.into(),
            display_dir: display_dir.into(),
        }
    }
}

pub struct Circuit {
    pub ty: DynStaticStr,
    pub id: usize,
    pub pos: Vec2i,

    pub board: Arc<CircuitBoard>,
    pub info: Arc<RwLock<CircuitInfo>>,
    pub imp: Arc<RwLock<Box<dyn CircuitImpl>>>,
    pub props: CircuitPropertyStore,
}

impl Circuit {
    #[allow(clippy::too_many_arguments)] // TODO: cleanup
    pub fn create(
        id: usize,
        pos: Vec2i,
        preview: &CircuitPreview,
        board: Arc<CircuitBoard>,
        props_override: Option<CircuitPropertyStore>,
        paste: bool,
        imp_data: Option<&Intermediate>,
        process_formatting: bool,
        errors: &mut ErrorList,
    ) -> Arc<Self> {
        let imp = preview.imp.create_impl();
        let props = props_override.unwrap_or_else(|| preview.props.clone());

        let mut props_map = props.inner().write();

        if process_formatting {
            for (prop_id, prop) in props_map.iter_mut() {
                let string_prop = prop.imp_mut().downcast_mut::<ArcString>();
                let string_prop = unwrap_option_or_continue!(string_prop);
                let string_arc = string_prop.get_arc();

                let mut formatter = StringFormatterState::new(&string_arc);
                if !formatter.has_formatting() {
                    continue;
                }
                let circuits = board.circuits.read();
                for circuit in circuits.iter() {
                    circuit.props.read(prop_id, |s: &ArcString| {
                        formatter.add_evironment_string(&s.get_str());
                    });
                }

                formatter.process_string(string_prop.get_mut());
            }
        }
        drop(props_map);

        let circuit = Arc::new(Self {
            ty: preview.imp.type_name(),
            id,
            pos,
            board,
            info: Default::default(),
            imp: Arc::new(RwLock::new(imp)),
            props,
        });

        let mut imp = circuit.imp.write();
        if let Some(data) = imp_data {
            let mut errors = errors.enter_context(|| "loading data");
            imp.load(&circuit, data, paste, &mut errors);
        }

        imp.apply_props(&circuit, None);
        let mut pins = imp.create_pins(&circuit);
        for pin in pins.iter_mut().enumerate() {
            pin.1.pin.write().id = CircuitPinId::new(pin.0, id);
        }
        *circuit.info.write() = CircuitInfo {
            size: imp.size(&circuit),
            pins,
        };
        drop(imp);
        circuit
    }

    pub fn save(self: &Arc<Self>) -> crate::io::CircuitData {
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
            imp: self.imp.read().save(self, false),
            props: self.props.save(),
        }
    }

    pub fn copy(self: &Arc<Self>, pos: Vec2u, state: &State) -> crate::io::CircuitCopyData {
        let internal = state
            .read_circuit(self.id, |circuit| {
                circuit
                    .internal
                    .as_ref()
                    .map(|i| i.serialize(true))
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        let design = self.board.designs.read().current().clone();
        let mut controls = FixedVec::new();
        for (&(circuit, control), design) in design.controls.iter() {
            if circuit != self.id {
                continue;
            }

            controls.set(
                control,
                CircuitDesignControlCopy {
                    rect: design.rect,
                    display_name: design.display_name.get_str().to_owned(),
                },
            );
        }

        crate::io::CircuitCopyData {
            ty: self.ty.clone(),
            pos,
            imp: self.imp.read().save(self, true),
            internal,
            update: state
                .updates
                .lock()
                .iter()
                .find_map(|info| {
                    (info.id == self.id)
                        .then(|| info.next_time.checked_duration_since(Instant::now()))
                })
                .flatten(),
            props: self.props.save(),
            design_controls: controls.inner,
        }
    }

    pub fn read_imp<T: CircuitImpl, R>(&self, reader: impl FnOnce(&T) -> R) -> Option<R> {
        let imp = self.imp.read();
        let imp = imp.deref();
        if TypeId::of::<T>() != imp.deref().type_id() {
            None
        } else {
            let imp = unsafe { &*(imp.deref() as *const dyn CircuitImpl as *const T) };
            let res = reader(imp);
            Some(res)
        }
    }

    pub fn write_imp<T: CircuitImpl, R>(&self, writer: impl FnOnce(&mut T) -> R) -> Option<R> {
        let mut imp = self.imp.write();
        let ty = imp.deref().deref().type_id();
        if TypeId::of::<T>() != ty {
            None
        } else {
            let imp =
                unsafe { &mut *(imp.deref_mut().deref_mut() as *mut dyn CircuitImpl as *mut T) };
            let res = writer(imp);
            Some(res)
        }
    }

    pub fn initialize(self: &Arc<Self>, first_init: bool) {
        self.imp.write().circuit_init(self, first_init);

        let any_controls = self.imp.read().control_count(self).is_some();
        if any_controls {
            self.board.controls.write().insert(self.id);
        }
    }

    pub fn remove(self: &Arc<Self>) {
        self.imp.write().circuit_remove(self);
        self.board.controls.write().remove(&self.id);

        let mut designs = self.board.designs.write();
        let current = designs.current_mut();

        let max_id = current
            .controls
            .keys()
            .filter_map(|(c, i)| (*c == self.id).then_some(*i))
            .max();
        if let Some(max_id) = max_id {
            for i in 0..=max_id {
                current.controls.remove(&(self.id, i));
            }
        }
    }

    pub fn name(&self) -> Option<Arc<str>> {
        self.props
            .read("name", |s: &ArcString| {
                s.is_empty().not().then(|| s.get_arc())
            })
            .flatten()
    }
}

#[derive(Clone)]
pub struct CircuitStateContext {
    pub global_state: Arc<State>,
    pub circuit: Arc<Circuit>,
}

impl CircuitStateContext {
    pub fn new(state: Arc<State>, circuit: Arc<Circuit>) -> Self {
        Self {
            global_state: state,
            circuit,
        }
    }

    pub fn read_circuit_state<R>(&self, reader: impl FnOnce(&CircuitState) -> R) -> Option<R> {
        self.global_state.read_circuit(self.circuit.id, reader)
    }

    pub fn write_circuit_state<R>(&self, writer: impl FnOnce(&mut CircuitState) -> R) -> R {
        self.global_state.write_circuit(self.circuit.id, writer)
    }

    pub fn clone_circuit_internal_state<T: InternalCircuitState + Clone>(&self) -> Option<T> {
        self.global_state
            .read_circuit(self.circuit.id, |c| c.get_internal::<T>().cloned())
            .flatten()
    }

    pub fn read_circuit_internal_state<T: InternalCircuitState, R>(
        &self,
        reader: impl FnOnce(&T) -> R,
    ) -> Option<R> {
        self.global_state
            .read_circuit(self.circuit.id, |c| c.get_internal().map(reader))
            .flatten()
    }

    pub fn write_circuit_internal_state<T: InternalCircuitState + Default, R>(
        &self,
        writer: impl FnOnce(&mut T) -> R,
    ) -> R {
        self.global_state
            .write_circuit(self.circuit.id, |c| writer(c.get_internal_mut()))
    }

    pub fn set_circuit_internal_state<T: InternalCircuitState>(&self, value: Option<T>) {
        self.global_state
            .write_circuit(self.circuit.id, |c| c.set_internal(value));
    }

    pub fn set_update_interval(&self, interval: Option<Duration>) {
        match interval {
            Some(dur) => self
                .global_state
                .set_circuit_update_interval(self.circuit.id, dur),
            None => self
                .global_state
                .reset_circuit_update_interval(self.circuit.id),
        }
    }

    pub fn props(&self) -> &CircuitPropertyStore {
        &self.circuit.props
    }
}

pub struct CircuitControlInfo {
    /// Rect for the control, in absolute circuit coordinates
    pub rect: Rect,
    pub display_name: DynStaticStr,
}

#[allow(unused_variables)]
pub trait CircuitImpl: Any + Send + Sync {
    fn draw(&self, ctx: &CircuitStateContext, paint_ctx: &PaintContext);

    /// After calling this, consider all connected pins invalid
    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]>;

    fn update_signals(&self, ctx: &CircuitStateContext, changed_pin: Option<usize>);

    fn update(&self, ctx: &CircuitStateContext, interval: &mut Option<Duration>) {}

    /// Called once on circuit state creation or load<br>
    /// Called after `circuit_init`<br>
    /// Use `CircuitStateContext::set_update_interval` to set how often `Self::update` is going to be called<br>
    /// `first_init`: `true` on new state creation, `false` on loaded state
    fn state_init(&self, ctx: &CircuitStateContext, first_init: bool) {}

    /// Called once on circuit placement or load<br>
    /// Called before all calls to `state_init`<br>
    /// `first_init`: `true` on placement, `false` on load
    fn circuit_init(&mut self, circ: &Arc<Circuit>, first_init: bool) {}

    /// Called once on circuit removal for each active state<br>
    /// Called before `remove_circuit`<br>
    /// if `reset_state` will be true, internal state will be removed
    fn state_remove(&self, ctx: &CircuitStateContext, reset_state: &mut bool) {}

    /// Called once on circuit removal<br>
    /// Called after all calls to `remove_state`<br>
    fn circuit_remove(&mut self, circ: &Arc<Circuit>) {}

    /// Return `None` if this circuit will never have controls,
    /// `Some(0)` means that this circuit doesn't have controls yet, but may have in the future
    fn control_count(&self, circ: &Arc<Circuit>) -> Option<usize> {
        None
    }

    fn control_info(&self, circ: &Arc<Circuit>, id: usize) -> Option<CircuitControlInfo> {
        None
    }

    fn update_control(
        &self,
        id: usize,
        circ: &Arc<Circuit>,
        state: Option<&CircuitStateContext>,
        ctx: &PaintContext,
        interactive: bool,
        uid: Id,
    ) {
    }

    /// Whether to automatically draw pins before call to `Self::draw`
    fn draw_pin_points(&self) -> bool {
        true
    }

    /// Serialize circuit parameters. NOT for circuit state<br>
    /// `copy`: `true` if data is saved for copying
    fn save(&self, circ: &Arc<Circuit>, copy: bool) -> serde_intermediate::Intermediate {
        ().into()
    }

    /// Note: will be called with `()` (`Intermediate::Unit`) if no data was saved before
    fn load(
        &mut self,
        circ: &Arc<Circuit>,
        data: &serde_intermediate::Intermediate,
        paste: bool,
        errors: &mut ErrorList,
    ) {
    }

    /// Load data for internal state. Global states may not be loaded yet.
    fn load_internal(
        &self,
        ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        paste: bool,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        None
    }

    /// Custom read for [`PinDirection::Custom`]
    fn custom_pin_mutate_state(
        &self,
        ctx: &CircuitStateContext,
        pin: usize,
        state: &mut WireState,
        visited_items: &mut VisitedList,
    ) {
    }

    /// Custom write for [`PinDirection::Custom`]
    fn custom_pin_apply_state(
        &self,
        ctx: &CircuitStateContext,
        pin: usize,
        state: &WireState,
        visited_items: &mut VisitedList,
    ) {
    }

    /// Called to determine which circuit parameters need to be recalculated
    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {}

    /// Called after all circuit parameters were successfully updated
    fn apply_props(&mut self, circ: &Arc<Circuit>, changed: Option<&str>) {}

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u;
}

pub struct CircuitPreview {
    pub imp: Box<dyn CircuitPreviewImpl>,
    pub props: CircuitPropertyStore,
    pub description: RwLock<DynCircuitDescription>,
}

impl CircuitPreview {
    pub fn new(imp: Box<dyn CircuitPreviewImpl>, props: CircuitPropertyStore) -> Self {
        let description = RwLock::new(imp.describe(&props));
        Self {
            imp,
            props,
            description,
        }
    }

    pub fn load_with_data(
        imp: Box<dyn CircuitPreviewImpl>,
        data: &crate::io::CircuitPreviewData,
    ) -> Self {
        let props = imp.default_props();
        props.load(&data.props);
        Self::new(imp, props)
    }

    pub fn load_copy(
        &self,
        data: &CircuitCopyData,
        ctx: &Arc<SimulationContext>,
        errors: &mut ErrorList,
    ) -> Option<Self> {
        let mut errors =
            errors.enter_context(|| format!("loading preview for {}", data.ty.deref()));
        let imp = self
            .imp
            .load_copy_data(&data.imp, &data.internal, ctx, &mut errors)?;
        let props = imp.default_props();
        props.load(&data.props);
        let description = RwLock::new(imp.describe(&props));
        Some(Self {
            imp,
            props,
            description,
        })
    }

    pub fn from_impl(imp: Box<dyn CircuitPreviewImpl>) -> Self {
        let props = imp.default_props();
        let description = RwLock::new(imp.describe(&props));
        Self {
            imp,
            props,
            description,
        }
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

    pub fn redescribe(&self) {
        *self.description.write() = self.imp.describe(&self.props);
    }

    pub fn describe(&self) -> DynCircuitDescription {
        self.description.read().clone()
    }
}

pub trait CircuitPreviewImpl: Send + Sync {
    fn type_name(&self) -> DynStaticStr;
    fn display_name(&self) -> DynStaticStr;
    fn description(&self) -> DynStaticStr;
    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool);
    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;

    /// For loading data created with `CircuidImpl::save(circuit, true)` and `InternalCircuitState::serialize(true)`
    fn load_copy_data(
        &self,
        imp: &serde_intermediate::Intermediate,
        internal: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>>;
    fn default_props(&self) -> CircuitPropertyStore;
}

#[derive(Default)]
pub struct CircuitNode {
    pub origin_dist: Vec2u,
    pub circuit: OptionalInt<usize>,
}

#[derive(Clone)]
pub struct CircuitPinDescription {
    pub active: bool,
    pub display_name: DynStaticStr,
    pub display_dir: Option<Direction4>,
    pub dir: InternalPinDirection,
    pub name: DynStaticStr,
    pub pos: Vec2u,
}

#[derive(Clone)]
pub struct CircuitDescription<const P: usize> {
    pub size: Vec2u,
    pub pins: [CircuitPinDescription; P],
}

#[derive(Clone)]
pub struct DynCircuitDescription {
    pub size: Vec2u,
    pub pins: Arc<[CircuitPinDescription]>,
}

impl CircuitPinDescription {
    pub fn to_info(&self) -> CircuitPinInfo {
        CircuitPinInfo::new(
            self.pos,
            self.dir,
            self.name.clone(),
            self.display_name.clone(),
            self.display_dir,
        )
    }

    pub fn to_active_info(&self) -> Option<CircuitPinInfo> {
        match self.active {
            false => None,
            true => Some(CircuitPinInfo::new(
                self.pos,
                self.dir,
                self.name.clone(),
                self.display_name.clone(),
                self.display_dir,
            )),
        }
    }

    pub fn new_unused() -> Self {
        Self {
            active: false,
            display_name: "".into(),
            display_dir: None,
            dir: InternalPinDirection::Inside,
            name: "".into(),
            pos: [0, 0].into(),
        }
    }
}

impl<const P: usize> CircuitDescription<P> {
    pub fn to_dyn(&self) -> DynCircuitDescription {
        DynCircuitDescription {
            size: self.size,
            pins: Arc::new(self.pins.clone()),
        }
    }
}

//  # - - - +  + - - +  + - - - +  + - - #
//  | * *   |  |     |  |       |  |   * |
//  |       |  | *   |  |   * * |  |   * |
//  + - - - +  | *   |  + - - - #  |     |
//             # - - +             + - - +
//   Up         Left     Down       Right

const fn rotate_pos(pos: [u32; 2], size: [u32; 2], dir: Direction4) -> [u32; 2] {
    match dir {
        Direction4::Up => pos,
        Direction4::Left => [pos[1], size[1] - pos[0] - 1],
        Direction4::Down => [size[0] - pos[0] - 1, size[1] - pos[1] - 1],
        Direction4::Right => [size[0] - pos[1] - 1, pos[0]],
    }
}

#[macro_export]
macro_rules! expr_or_default {
    ($e:expr, $def:expr) => {
        $e
    };
    (, $def:expr) => {
        $def
    };
}

#[macro_export]
macro_rules! describe_directional_circuit {
    (
        default_dir: $default_dir:expr,
        dir: $dir:expr,
        size: [$width:expr, $height: expr],

        $(
            $pin_name:literal:
                $pin_dir:expr,
                $pin_dname:expr,
                $pin_ddir:expr,
                [$pin_x:expr, $pin_y: expr]
                $(, active: $active:expr)?
        ),*
        $(,)?
    ) => {
        {
            use Direction4::*;

            let dir = $dir;
            let default_dir = $default_dir;
            let dir_normalized = dir.rotate_counterclockwise_by(default_dir);
            let size_rotated = if default_dir.is_horizontal() == dir.is_horizontal() {
                [$width, $height]
            } else {
                [$height, $width]
            };

            {
                use InternalPinDirection::*;
                use $crate::expr_or_default;

                $crate::circuits::CircuitDescription {
                    size: size_rotated.into(),
                    pins: [
                        $(
                            $crate::circuits::CircuitPinDescription {
                                active: expr_or_default!($($active)?, true),
                                name: $pin_name.into(),
                                dir: $pin_dir,
                                display_name: $pin_dname.into(),
                                display_dir: Option::<Direction4>::from($pin_ddir)
                                    .map(|d| d.rotate_clockwise_by(dir_normalized)),
                                pos: if expr_or_default!($($active)?, true) {
                                    $crate::circuits::rotate_pos([$pin_x, $pin_y], size_rotated, dir_normalized).into()
                                } else {
                                    [0, 0].into()
                                }
                            },
                        )*
                    ]
                }
            }
        }
    };
}

#[macro_export]
macro_rules! describe_directional_custom_circuit {
    (
        default_dir: $default_dir:expr,
        dir: $dir:expr,
        size: [$width:expr, $height: expr],

        $(
            $pin_name:literal:
                $pin_dir:expr,
                $pin_dname:literal,
                $pin_ddir:expr,
                [$pin_x:expr, $pin_y: expr],
                $(active: $active:expr,)?
        )*

        dir_proc: |$dir_proc_param:ident| $dir_proc_body:expr,
        pos_proc: |$pos_proc_param:ident| $pos_proc_body:expr
        $(,)?
    ) => {
        {
            use Direction4::*;

            let dir = $dir;
            let default_dir = $default_dir;
            let dir_normalized = dir.rotate_counterclockwise_by(default_dir);
            let size_rotated = if default_dir.is_horizontal() == dir.is_horizontal() {
                [$width, $height]
            } else {
                [$height, $width]
            };

            {
                use InternalPinDirection::*;
                use $crate::expr_or_default;

                $crate::circuits::CircuitDescription {
                    size: size_rotated.into(),
                    pins: [
                        $(
                            $crate::circuits::CircuitPinDescription {
                                active: expr_or_default!($($active)?, true),
                                name: $pin_name.into(),
                                dir: $pin_dir,
                                display_name: $pin_dname.into(),
                                display_dir: Option::<Direction4>::from($pin_ddir)
                                    .map(|$dir_proc_param| $dir_proc_body.rotate_clockwise_by(dir_normalized)),
                                pos: $crate::circuits::rotate_pos( { let $pos_proc_param = [$pin_x, $pin_y]; $pos_proc_body }, size_rotated, dir_normalized).into(),
                            },
                        )*
                    ]
                }
            }
        }
    };
}
