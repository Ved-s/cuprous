use std::{
    collections::HashSet,
    sync::{Arc, RwLock, RwLockReadGuard},
    time::Duration,
};

use eframe::{
    egui::Sense,
    epaint::{Color32, Rounding},
};
use emath::{Align2, Rect};

use crate::{
    containers::{Chunks2D, FixedVec},
    state::{CircuitState, InternalCircuitState, State, UpdateTask, WireState},
    vector::{IsZero, Vec2f, Vec2i, Vec2u, Vector},
    wires::Wires,
    OptionalInt, PaintContext,
};

pub struct CircuitInfo {
    pub size: Vec2u,
    pub pins: Box<[CircuitPinInfo]>,
}

#[derive(Debug, Clone, Copy)]
pub enum PinDirection {
    Inside,
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
    pub wire: Option<usize>,
    pub dir: PinDirection,
}

impl CircuitPin {
    pub fn get_state(&self, state: &State) -> WireState {
        match self.dir {
            PinDirection::Inside => self
                .wire
                .map(|wire| state.read_wire(wire))
                .unwrap_or_default(),
            PinDirection::Outside => state
                .read_circuit(self.id.circuit_id)
                .map(|cs| {
                    cs.read()
                        .unwrap()
                        .pins
                        .get(self.id.id)
                        .map(|s| *s)
                        .unwrap_or_default()
                })
                .unwrap_or_default(),
        }
    }

    pub fn set_input(&self, state: &State, value: WireState) {
        let circuit = state.get_circuit(self.id.circuit_id);
        let mut circuit = circuit.write().unwrap();

        let current = circuit.pins.get_clone(self.id.id).unwrap_or_default();
        if current == value {
            return;
        }

        circuit.pins.set(value, self.id.id);
        state
            .queue
            .lock()
            .unwrap()
            .enqueue(UpdateTask::UpdateCircuitSignals {
                id: self.id.circuit_id,
                pin: Some(self.id.id),
            });
    }
}

#[derive(Debug, Clone)]
pub struct CircuitPinInfo {
    pos: Vec2u,
    dir: PinDirection,
    pin: Arc<RwLock<CircuitPin>>,
}

impl CircuitPinInfo {
    fn get_input(&self, state: &CircuitState) -> WireState {
        state
            .pins
            .get_clone(self.pin.read().unwrap().id.id)
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
            state_ctx
                .global_state
                .queue
                .lock()
                .unwrap()
                .enqueue(UpdateTask::UpdateWireState(wire))
        }
    }
}

impl CircuitPinInfo {
    fn new(pos: impl Into<Vec2u>, dir: PinDirection) -> Self {
        Self {
            pos: pos.into(),
            dir,
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
    pub info: RwLock<CircuitInfo>,
    pub imp: Arc<RwLock<Box<dyn CircuitImpl>>>,
}

impl Circuit {
    fn create(id: usize, pos: Vec2i, preview: &dyn CircuitPreview) -> Self {
        let imp = preview.create_impl();
        let mut pins = imp.create_pins();
        for pin in pins.iter_mut().enumerate() {
            pin.1.pin.write().unwrap().id = CircuitPinId::new(pin.0, id);
        }

        Self {
            id,
            pos,
            info: RwLock::new(CircuitInfo {
                size: preview.size(),
                pins,
            }),
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
}

pub trait CircuitImpl {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext);

    fn create_pins(&self) -> Box<[CircuitPinInfo]>;

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>);

    fn update(&mut self, state_ctx: &CircuitStateContext);

    /// Warning! Must always return either [`Some`] or [`None`] (same type)
    #[allow(unused_variables)]
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
    origin_dist: Vector<2, u32>,
    circuit: OptionalInt<usize>,
}

pub struct Circuits {
    pub nodes: Chunks2D<16, CircuitNode>,
    pub cirtuits: FixedVec<Circuit>,

    pub updates: HashSet<usize>,
}

impl Circuits {
    pub fn new() -> Self {
        Self {
            nodes: Default::default(),
            cirtuits: vec![].into(),
            updates: HashSet::new(),
        }
    }

    pub fn update(
        &mut self,
        state: &State,
        wires: &mut Wires,
        ctx: &PaintContext,
        selected: Option<&Box<dyn CircuitPreview>>,
    ) {
        ctx.draw_chunks(
            &self.nodes,
            self,
            |n| n.circuit.is_some(),
            |node, pos, ctx, this, _| {
                if !node.origin_dist.is_zero()
                    && pos.x() != ctx.bounds.tiles_tl.x()
                    && pos.y() != ctx.bounds.tiles_tl.y()
                {
                    return;
                }
                let circ_id = match node.circuit.get() {
                    None => return,
                    Some(c) => c,
                };

                let circuit = match this.cirtuits.get(circ_id) {
                    None => return,
                    Some(c) => c,
                };

                let circ_info = circuit.info.read().unwrap();

                let pos = pos - node.origin_dist.convert_values(|v| v as i32);
                let screen_pos = ctx.screen.world_to_screen_tile(pos);
                let screen_size = circ_info.size.convert_values(|v| v as f32) * ctx.screen.scale;
                let rect = Rect::from_min_size(screen_pos.into(), screen_size.into());
                let circ_ctx = ctx.with_rect(rect);

                let state_ctx = CircuitStateContext::new(state, circuit);

                circuit.imp.read().unwrap().draw(&state_ctx, &circ_ctx);
            },
        );

        match selected {
            None => return,
            Some(p) => self.handle_preview(state, wires, ctx, p),
        };
    }

    fn handle_preview(
        &mut self,
        state: &State,
        wires: &mut Wires,
        ctx: &PaintContext<'_>,
        preview: &Box<dyn CircuitPreview>,
    ) {
        let mouse_tile_pos = ctx
            .egui_ctx
            .input(|input| input.pointer.interact_pos())
            .map(|p| ctx.screen.screen_to_world(Vec2f::from(p)));
        let mouse_tile_pos_i = match mouse_tile_pos {
            None => return,
            Some(v) => v.convert_values(|v| v.floor() as i32),
        };
        let size = preview.size();
        if size.x() == 0 || size.y() == 0 {
            return;
        }
        let place_pos = mouse_tile_pos_i - size.convert_values(|v| v as i32) / 2;
        let rect = Rect::from_min_size(
            ctx.screen.world_to_screen_tile(place_pos).into(),
            (size.convert_values(|v| v as f32) * ctx.screen.scale).into(),
        );
        preview.draw_preview(&ctx.with_rect(rect));
        let interaction = ctx.ui.interact(ctx.rect, ctx.ui.id(), Sense::click());

        if interaction.clicked_by(eframe::egui::PointerButton::Primary) {
            for j in 0..size.y() {
                for i in 0..size.x() {
                    let x = place_pos.x() + i as i32;
                    let y = place_pos.y() + j as i32;
                    if self
                        .nodes
                        .get(x as isize, y as isize)
                        .is_some_and(|n| n.circuit.is_some())
                    {
                        return;
                    }
                }
            }

            let cid = self.cirtuits.first_free_pos();
            let circ = Circuit::create(cid, place_pos, preview.as_ref());
            let circ = self.cirtuits.set(circ, cid).value_ref;

            for j in 0..size.y() {
                for i in 0..size.x() {
                    let x = place_pos.x() + i as i32;
                    let y = place_pos.y() + j as i32;
                    let node = self.nodes.get_or_create_mut(x as isize, y as isize);

                    node.circuit.set(Some(cid));
                    node.origin_dist = [i, j].into();
                }
            }

            for pin in circ.info.read().unwrap().pins.iter() {
                let pos = place_pos + pin.pos.convert_values(|v| v as i32);
                if let Some(wire) = wires.create_intersection(pos, None) {
                    pin.pin.write().unwrap().wire = Some(wire.id);
                    if let Some(p) = wire.nodes.get_mut(&pos) {
                        p.pin = Some(pin.pin.clone());
                    }
                }
            }

            let state_ctx = CircuitStateContext::new(state, circ);
            state.update_circuit_signals(
                &state_ctx,
                None,
            );

            if let Some(duration) = circ.imp.read().unwrap().update_interval(&state_ctx) {
                self.updates.insert(cid);
                state.update_circuit_interval(cid, duration);
            }
        }
    }

    pub fn pin_at(&self, pos: Vec2i) -> Option<Arc<RwLock<CircuitPin>>> {
        let circ = self.nodes.get(pos.x() as isize, pos.y() as isize)?;
        let pos = circ.origin_dist;
        let circ = circ.circuit.get()?;
        let circ = self.cirtuits.get(circ)?;

        circ.info
            .read()
            .unwrap()
            .pins
            .iter()
            .find(|p| p.pos == pos)
            .map(|p| p.pin.clone())
    }
}

impl Default for Circuits {
    fn default() -> Self {
        Self {
            cirtuits: vec![].into(),
            nodes: Default::default(),
            updates: HashSet::default(),
        }
    }
}

#[derive(Default)]
pub struct TestCircuitState {
    state: bool,
}

impl InternalCircuitState for TestCircuitState {}

pub struct TestCircuitImpl {
    pin: CircuitPinInfo,
}

impl TestCircuitImpl {
    fn new() -> Self {
        Self {
            pin: CircuitPinInfo::new([0, 0], PinDirection::Outside),
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

        let wire = self.pin.pin.read().unwrap().wire.map(|v| v as i32);

        paint_ctx.paint.text(
            rect.center_bottom(),
            Align2::CENTER_BOTTOM,
            state_ctx.circuit.id.to_string(),
            font_id.clone(),
            Color32::WHITE,
        );

        if let Some(wire) = wire {
            let pos = state_ctx.circuit.pos + self.pin.pos.convert_values(|v| v as i32);
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
        vec![self.pin.clone()].into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        self.pin.set_output(
            state_ctx,
            state_ctx
                .read_circuit_internal_state::<TestCircuitState, _>(|s| s.state)
                .unwrap_or_default()
                .into(),
        );
    }

    fn update(&mut self, state_ctx: &CircuitStateContext) {
        let new_state = state_ctx.write_circuit_internal_state::<TestCircuitState, _>(|s| {
            s.state = !s.state;
            s.state
        });

        self.pin.set_output(state_ctx, new_state.into());
    }

    fn update_interval(&self, state_ctx: &CircuitStateContext) -> Option<Duration> {
        match state_ctx
            .read_circuit_internal_state::<TestCircuitState, _>(|s| s.state)
            .unwrap_or_default()
        {
            true => Some(Duration::from_millis(200)),
            false => Some(Duration::from_secs(1)),
        }
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
