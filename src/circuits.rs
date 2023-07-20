use std::{sync::Arc, time::Duration};

use eframe::epaint::{Color32, Rounding};
use emath::Align2;

use crate::{
    board::CircuitBoard,
    state::{CircuitState, InternalCircuitState, State, WireState},
    vector::{Vec2i, Vec2u, Vector},
    OptionalInt, PaintContext, RwLock,
};

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[CircuitPinInfo]>,
}

#[derive(Debug, Clone, Copy)]
pub enum InternalPinDirection {
    StateDependent { default: PinDirection },
    Inside,
    Outside,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum PinDirection {
    Inside,

    #[default]
    Outside,
}

#[derive(Debug, Clone, Copy, Default)]
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
    pub id: CircuitPinId,
    wire: Option<usize>,
    dir: InternalPinDirection,
}

impl CircuitPin {
    pub fn direction(&self, state: &State) -> PinDirection {
        match self.dir {
            InternalPinDirection::Inside => PinDirection::Inside,
            InternalPinDirection::Outside => PinDirection::Outside,
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

    pub fn set_input(&self, state: &State, value: WireState) {
        let circuit = state.get_circuit(self.id.circuit_id);
        let mut circuit = circuit.write().unwrap();

        let current = circuit.pins.get_clone(self.id.id).unwrap_or_default();
        if current == value {
            return;
        }

        circuit.pins.set(value, self.id.id);
        state.update_circuit_signals(self.id.circuit_id, Some(self.id.id));
    }

    pub fn connected_wire(&self) -> Option<usize> {
        self.wire
    }

    pub fn set_wire(&mut self, board: Option<&CircuitBoard>, wire: Option<usize>) {
        if self.wire == wire {
            return;
        }

        let prev = self.wire;

        self.wire = wire;

        if let Some(board) = board {
            if let Some(prev) = prev {
                board.states.update_wire(prev);
            }
            if let Some(wire) = wire {
                board.states.update_wire(wire);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CircuitPinInfo {
    pub pos: Vec2u,
    pub pin: Arc<RwLock<CircuitPin>>,
}

impl CircuitPinInfo {
    fn get_input(&self, state_ctx: &CircuitStateContext) -> WireState {
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

    fn set_output(&self, state_ctx: &CircuitStateContext, value: WireState) {
        let CircuitPin {
            id: CircuitPinId { circuit_id: _, id },
            wire,
            dir: _,
        } = *self.pin.read().unwrap();

        let current = state_ctx
            .read_circuit_state()
            .map(|arc| arc.read().unwrap().pins.get_clone(id).unwrap_or_default())
            .unwrap_or_default();
        if current == value {
            return;
        }

        state_ctx
            .get_circuit_state()
            .write()
            .unwrap()
            .pins
            .set(value, id);
        if let Some(wire) = wire {
            state_ctx.global_state.update_wire(wire)
        }
    }

    fn set_direction(&self, state_ctx: &CircuitStateContext, dir: PinDirection) {
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
                Some(wire) => state_ctx
                    .global_state.update_wire(wire),
                None => self
                    .pin
                    .read()
                    .unwrap()
                    .set_input(state_ctx.global_state, Default::default()),
            },
            PinDirection::Outside => state_ctx.global_state.update_circuit_signals(pin_id.circuit_id, Some(pin_id.id)),
        }
    }
}

impl CircuitPinInfo {
    fn new(pos: impl Into<Vec2u>, dir: InternalPinDirection) -> Self {
        Self {
            pos: pos.into(),
            pin: Arc::new(RwLock::new(CircuitPin {
                id: Default::default(),
                dir,
                wire: None,
            })),
        }
    }
}

pub struct Circuit {
    pub id: usize,
    pub pos: Vec2i,
    pub info: Arc<RwLock<CircuitInfo>>,
    pub imp: Arc<RwLock<Box<dyn CircuitImpl>>>,
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

    pub fn write_circuit_internal_state<T: InternalCircuitState, R>(
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
        self.global_state.set_circuit_update_interval(self.circuit.id, interval);
    }
}

#[allow(unused_variables)]
pub trait CircuitImpl {
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
}
pub trait CircuitPreview: std::fmt::Debug {
    fn draw_preview(&self, ctx: &PaintContext);
    fn size(&self) -> Vec2u;
    fn create_impl(&self) -> Box<dyn CircuitImpl>;
}

#[derive(Default)]
pub struct CircuitNode {
    pub origin_dist: Vector<2, u32>,
    pub circuit: OptionalInt<usize>,
}

#[derive(Default)]
pub struct TestCircuitState {
    state: bool,
    dir_in: bool,
}

impl InternalCircuitState for TestCircuitState {}

pub struct TestCircuitImpl {
    clock_pin: CircuitPinInfo,
    dir_pin: CircuitPinInfo,
    io_pin: CircuitPinInfo,
}

impl TestCircuitImpl {
    fn new() -> Self {
        Self {
            clock_pin: CircuitPinInfo::new([0, 0], InternalPinDirection::Outside),
            dir_pin: CircuitPinInfo::new([0, 1], InternalPinDirection::Inside),
            io_pin: CircuitPinInfo::new(
                [1, 1],
                InternalPinDirection::StateDependent {
                    default: PinDirection::Outside,
                },
            ),
        }
    }
}

impl CircuitImpl for TestCircuitImpl {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        for pin in state_ctx.circuit.info.read().unwrap().pins.iter() {
            let pos = state_ctx.circuit.pos + pin.pos.convert_values(|v| v as i32);
            let pin = pin.pin.read().unwrap();
            if pin.wire.is_some() {
                continue;
            }
            let color = pin.get_state(state_ctx.global_state).color();
            let pos = paint_ctx.screen.world_to_screen_tile(pos) + paint_ctx.screen.scale / 2.0;
            paint_ctx
                .paint
                .circle_filled(pos.into(), paint_ctx.screen.scale * 0.1, color);
        }

        let font_id = eframe::egui::TextStyle::Monospace.resolve(paint_ctx.ui.style());

        let rect = {
            let mut r = paint_ctx.rect;
            *r.left_mut() += paint_ctx.screen.scale / 2.0;
            *r.right_mut() -= paint_ctx.screen.scale / 2.0;
            r
        };
        paint_ctx
            .paint
            .rect_filled(rect, Rounding::none(), Color32::from_gray(100));

        let wire = self.clock_pin.pin.read().unwrap().wire.map(|v| v as i32);

        paint_ctx.paint.text(
            rect.center_bottom(),
            Align2::CENTER_BOTTOM,
            state_ctx.circuit.id.to_string(),
            font_id.clone(),
            Color32::WHITE,
        );

        if let Some(wire) = wire {
            let pos = state_ctx.circuit.pos + self.clock_pin.pos.convert_values(|v| v as i32);
            let pos = paint_ctx.screen.world_to_screen_tile(pos) + paint_ctx.screen.scale / 2.0;
            let pos = pos + [paint_ctx.screen.scale / 4.0, 0.0];

            paint_ctx.paint.text(
                pos.into(),
                Align2::LEFT_CENTER,
                wire.to_string(),
                font_id,
                Color32::WHITE,
            );
        }
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![
            self.clock_pin.clone(),
            self.dir_pin.clone(),
            self.io_pin.clone(),
        ]
        .into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, pin: Option<usize>) {
        match pin {
            Some(1) => {
                let new_dir_in = matches!(self.dir_pin.get_input(state_ctx), WireState::True);
                let dir = state_ctx.write_circuit_internal_state::<TestCircuitState, _>(|cs| {
                    if cs.dir_in == new_dir_in {
                        None
                    } else {
                        cs.dir_in = new_dir_in;
                        Some(if new_dir_in {
                            PinDirection::Inside
                        } else {
                            PinDirection::Outside
                        })
                    }
                });
                if let Some(dir) = dir {
                    self.io_pin.set_direction(state_ctx, dir);
                }
            }
            None | Some(2) => {
                let dir_in =
                    state_ctx.read_circuit_internal_state::<TestCircuitState, _>(|cs| cs.dir_in);
                if !dir_in.is_some_and(|i| i) {
                    self.io_pin.set_output(state_ctx, WireState::True);
                }
            }
            _ => {}
        }

        if pin.is_none() {
            self.clock_pin.set_output(
                state_ctx,
                state_ctx
                    .read_circuit_internal_state::<TestCircuitState, _>(|s| s.state)
                    .unwrap_or_default()
                    .into(),
            );
        }
    }

    fn update(&mut self, state_ctx: &CircuitStateContext) {
        let new_state = state_ctx.write_circuit_internal_state::<TestCircuitState, _>(|s| {
            s.state = !s.state;
            s.state
        });

        self.clock_pin.set_output(state_ctx, new_state.into());
    }

    fn update_interval(&self, state_ctx: &CircuitStateContext) -> Option<Duration> {
        match state_ctx
            .read_circuit_internal_state::<TestCircuitState, _>(|s| s.state)
            .unwrap_or_default()
        {
            true => Some(Duration::from_millis(200)),
            false => Some(Duration::from_secs(2)),
        }
    }

    fn init_state(&self, state_ctx: &CircuitStateContext) {
        state_ctx.set_update_interval(self.update_interval(state_ctx));
    }
}

#[derive(Debug)]
pub struct TestCircuitPreview {
    pub a: usize,
    pub b: usize,
}

impl CircuitPreview for TestCircuitPreview {
    fn draw_preview(&self, ctx: &PaintContext) {
        ctx.paint.rect_filled(
            ctx.rect,
            Rounding::none(),
            Color32::from_rgba_unmultiplied(0, 255, 0, 100),
        );
    }

    fn size(&self) -> Vec2u {
        [2, 2].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(TestCircuitImpl::new())
    }
}
