
use std::{any::Any, f32::consts::TAU, ops::Deref, sync::Arc};

use eframe::egui::Rect;
use parking_lot::{Mutex, RwLock};

use crate::{
    board::{Board, Wire},
    selection::SelectionRenderer,
    state::{BoardState, UpdateTaskPool, WireState},
    str::ArcStaticStr,
    vector::{Vec2f, Vec2isize, Vec2usize},
    Direction4, Direction8, PaintContext,
};

pub mod test;
pub mod button;

pub struct Circuit {
    pub id: usize,
    pub board: Arc<Board>,
    pub info: RwLock<CircuitInfo>,
    pub imp: RwLock<CircuitImplData>,
    pub pins: RwLock<Box<[RealizedPin]>>,
}

#[derive(Clone)]
pub struct CircuitInfo {
    pub pos: Vec2isize,
    pub render_size: Vec2usize,
    pub size: Vec2usize,
    pub transform: CircuitTransform,
}

pub struct CircuitImplData {
    pub imp: CircuitImplBox,
    pub instance: Box<dyn Any + Send + Sync>,
}

#[derive(Clone, Copy)]
pub enum PinType {
    Custom,
    Inside,
    Outside,
}

#[derive(Clone)]
pub struct PinDescription {
    pub pos: Vec2usize,
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub dir: Option<Direction8>,
    pub ty: PinType,
}
impl PinDescription {
    pub fn pos_dir_mut(&mut self) -> PosDirMut {
        PosDirMut {
            pos: &mut self.pos,
            dir: self.dir.as_mut(),
        }
    }
}

pub struct CircuitPin {
    pub id: usize,
    pub ty: PinType,
    pub circuit: Arc<Circuit>,
    pub wire: RwLock<Option<Arc<Wire>>>,
}
impl CircuitPin {
    pub fn set_output(
        &self,
        board_state: &BoardState,
        tasks: &mut UpdateTaskPool,
        state: WireState,
    ) {
        let changed = board_state.set_pin(self.circuit.id, self.id, state);
        if changed {
            if let Some(wire) = self.wire.read().as_ref().map(|w| w.id) {
                tasks.add_wire_task(wire, false);
            }
        }
    }

    pub fn get_state(&self, state: &BoardState) -> WireState {
        state.get_pin(self.circuit.id, self.id)
    }

    pub(crate) fn disconnect(&self, tasks: &mut UpdateTaskPool) {
        let mut pin_wire = self.wire.write();
        let Some(wire) = pin_wire.deref() else {
            return;
        };

        wire.remove_pin(self.circuit.id, self.id);
        let id = wire.id;

        *pin_wire = None;

        self.handle_disconnect(id, tasks);
    }

    pub(crate) fn connect(self: &Arc<Self>, wire: Arc<Wire>, tasks: &mut UpdateTaskPool) {
        let mut pin_wire = self.wire.write();
        if let Some(wire) = pin_wire.deref() {
            wire.remove_pin(self.circuit.id, self.id);
            self.handle_disconnect(wire.id, tasks);
        };

        wire.add_pin(self.circuit.clone(), self.clone());
        let id = wire.id;
        *pin_wire = Some(wire);
        self.handle_connect(id, tasks);
    }

    fn handle_disconnect(&self, old_wire: usize, tasks: &mut UpdateTaskPool) {
        match self.ty {
            PinType::Custom => todo!(),
            PinType::Inside => {
                tasks.add_circuit_task(self.circuit.id, Some(self.id));
                for state in self.circuit.board.states().read().iter() {
                    state.set_pin(self.circuit.id, self.id, WireState::default());
                }
            }
            PinType::Outside => {
                tasks.add_wire_task(old_wire, true);
            }
        }
    }

    fn handle_connect(&self, wire: usize, tasks: &mut UpdateTaskPool) {
        match self.ty {
            PinType::Custom | PinType::Outside => {
                tasks.add_wire_task(wire, true);
            }
            PinType::Inside => {
                tasks.add_circuit_task(self.circuit.id, Some(self.id));
                for state in self.circuit.board.states().read().iter() {
                    state.set_pin(self.circuit.id, self.id, state.get_wire(wire));
                }
            }
        }
    }
}

pub struct RealizedPin {
    pub desc: PinDescription,
    pub pin: Arc<CircuitPin>,
}

pub struct CircuitSelectionRenderingContext<'a> {
    pub renderer: Arc<Mutex<SelectionRenderer>>,
    pub custom_selection: &'a mut bool,
}

pub struct CircuitRenderingContext<'a> {
    pub paint: &'a PaintContext<'a>,
    pub screen_rect: Rect,
    pub selection: Option<CircuitSelectionRenderingContext<'a>>,
    pub transform: CircuitTransform,

    // internal for transform_pos
    render_size: Vec2usize,
    angle: Option<f32>,
    flip: Option<FlipType>,
}

impl<'a> CircuitRenderingContext<'a> {
    pub fn new(
        ctx: &'a PaintContext,
        screen_rect: Rect,
        render_size: Vec2usize,
        selection: Option<CircuitSelectionRenderingContext<'a>>,
        transform: CircuitTransform,
    ) -> Self {
        let flip = transform
            .flip
            .then(|| transform.support.flip_type(None))
            .flatten();
        let angle = transform.support.rotation.and_then(|r| {
            if transform.dir == r.default_dir {
                None
            } else {
                Some(
                    transform
                        .dir
                        .rotated_counterclockwise_by(r.default_dir)
                        .into_angle_xp_cw()
                        + TAU / 4.0,
                )
            }
        });

        Self {
            paint: ctx,
            screen_rect,
            render_size,
            selection,
            transform,
            angle,
            flip,
        }
    }

    /// Transform circuit coordinate [0..size] to screen coordinate
    pub fn transform_pos(&self, pos: Vec2f) -> Vec2f {
        let norm = pos / self.render_size.convert(|v| v as f32);

        let norm = match self.flip {
            None => norm,
            Some(FlipType::Vertical) => [norm.x, 1.0 - norm.y].into(),
            Some(FlipType::Horizontal) => [1.0 - norm.x, norm.y].into(),
            Some(FlipType::Both) => [1.0 - norm.x, 1.0 - norm.y].into(),
        };

        let norm = match self.angle {
            None => norm,
            Some(a) => norm.rotated(a, 0.5),
        };

        self.screen_rect.lerp_inside(norm.into()).into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransformSupport {
    Automatic,
    Manual,
}

#[derive(Debug, Clone, Copy)]
pub enum FlipType {
    Vertical,
    Horizontal,
    Both,
}

#[derive(Debug, Clone, Copy)]
pub struct CircuitRotationSupport {
    pub support: TransformSupport,
    pub default_dir: Direction4,
}

#[derive(Debug, Clone, Copy)]
pub struct CircuitFlipSupport {
    pub support: TransformSupport,
    pub ty: FlipType,
}

#[derive(Debug, Clone, Copy)]
pub struct CircuitTransformSupport {
    pub rotation: Option<CircuitRotationSupport>,
    pub flip: Option<CircuitFlipSupport>,
}

impl CircuitTransformSupport {
    pub fn rotation_default_dir(&self, support: Option<TransformSupport>) -> Option<Direction4> {
        let rot = self.rotation?;
        if support.is_some_and(|s| rot.support != s) {
            return None;
        }
        Some(rot.default_dir)
    }

    pub fn flip_type(&self, support: Option<TransformSupport>) -> Option<FlipType> {
        let flip = self.flip?;
        if support.is_some_and(|s| flip.support != s) {
            return None;
        }
        Some(flip.ty)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CircuitTransform {
    pub support: CircuitTransformSupport,
    pub dir: Direction4,
    pub flip: bool,
}
impl CircuitTransform {
    pub fn transform_size(&self, size: Vec2usize, support: Option<TransformSupport>) -> Vec2usize {
        let Some(default_dir) = self.support.rotation_default_dir(support) else {
            return size;
        };

        if default_dir.is_vertical() == self.dir.is_vertical() {
            size
        } else {
            size.swapped()
        }
    }

    pub fn transform_pos(
        &self,
        size: Vec2usize,
        pos: Vec2usize,
        support: Option<TransformSupport>,
    ) -> Vec2usize {
        let flip = self.flip.then(|| self.support.flip_type(support)).flatten();

        let flipped_pos = match flip {
            None => pos,
            Some(FlipType::Vertical) => [pos.x, size.y - pos.y - 1].into(),
            Some(FlipType::Horizontal) => [size.x - pos.x - 1, pos.y].into(),
            Some(FlipType::Both) => [size.x - pos.x - 1, size.y - pos.y - 1].into(),
        };

        let default_dir = self.support.rotation_default_dir(support);

        match default_dir {
            None => flipped_pos,
            Some(default_dir) => {
                let dir = self.dir.rotated_counterclockwise_by(default_dir);
                let transformed_size = if default_dir.is_vertical() == self.dir.is_vertical() {
                    size
                } else {
                    size.swapped()
                };

                rotate_pos(flipped_pos, transformed_size, dir)
            }
        }
    }

    pub fn backtransform_pos(
        &self,
        size: Vec2usize,
        pos: Vec2usize,
        support: Option<TransformSupport>,
    ) -> Vec2usize {
        let default_dir = self.support.rotation_default_dir(support);

        let rotated_pos = match default_dir {
            None => pos,
            Some(default_dir) => {
                let dir = default_dir.rotated_counterclockwise_by(self.dir);
                rotate_pos(pos, size, dir)
            }
        };

        let flip = self.flip.then(|| self.support.flip_type(support)).flatten();

        match flip {
            None => rotated_pos,
            Some(FlipType::Vertical) => [rotated_pos.x, size.y - rotated_pos.y - 1].into(),
            Some(FlipType::Horizontal) => [size.x - rotated_pos.x - 1, rotated_pos.y].into(),
            Some(FlipType::Both) => [size.x - rotated_pos.x - 1, size.y - pos.y - 1].into(),
        }
    }

    pub fn transform_dir(&self, dir: Direction8, support: Option<TransformSupport>) -> Direction8 {
        let flip = self.flip.then(|| self.support.flip_type(support)).flatten();

        let flipped = match flip {
            None => dir,
            Some(FlipType::Vertical) => dir.flip_by(Direction8::Left),
            Some(FlipType::Horizontal) => dir.flip_by(Direction8::Up),
            Some(FlipType::Both) => dir.inverted(),
        };

        let default_dir = self.support.rotation_default_dir(support);

        match default_dir {
            None => flipped,
            Some(default_dir) => {
                let dir = self.dir.rotated_counterclockwise_by(default_dir);
                flipped.rotated_clockwise_by(dir.into())
            }
        }
    }

    fn transform_pins(
        &self,
        size: Vec2usize,
        pins: &mut dyn Iterator<Item = PosDirMut>,
        support: Option<TransformSupport>,
    ) {
        for pin in pins {
            *pin.pos = self.transform_pos(size, *pin.pos, support);

            if let Some(dir) = pin.dir {
                *dir = self.transform_dir(*dir, support);
            }
        }
    }
}

pub struct UntypedCircuitCtx<'a> {
    pub state: &'a Arc<BoardState>,
    pub circuit: &'a Arc<Circuit>,
    pub tasks: &'a mut UpdateTaskPool,
    pub instance: &'a dyn Any,
}

impl<'a> UntypedCircuitCtx<'a> {
    pub fn make_typed<C: CircuitImpl>(self) -> CircuitCtx<'a, C> {
        CircuitCtx {
            state: self.state,
            circuit: self.circuit,
            tasks: self.tasks,
            instance: self
                .instance
                .downcast_ref::<C::Instance>()
                .expect("correct instance for a circuit"),
        }
    }
}

pub struct CircuitCtx<'a, C: CircuitImpl> {
    pub state: &'a Arc<BoardState>,
    pub circuit: &'a Arc<Circuit>,
    pub tasks: &'a mut UpdateTaskPool,
    pub instance: &'a C::Instance,
}

impl<'a, C: CircuitImpl> CircuitCtx<'a, C> {
    fn set_pin_output(&mut self, pin: &CircuitPin, state: WireState) {
        pin.set_output(self.state, self.tasks, state);
    }

    fn get_pin_input(&mut self, pin: &CircuitPin) -> WireState {
        pin.get_state(self.state)
    }

    fn read_internal_state<R>(&self, reader: impl FnOnce(&C::State) -> R) -> Option<R> {
        self.state
            .read_internal_circuit_state(self.circuit.id, reader)
    }

    fn write_internal_state<R>(&self, writer: impl FnOnce(&mut C::State) -> R) -> R {
        self.state
            .write_internal_circuit_state(self.circuit.id, writer)
    }
}

pub trait CircuitImpl: Clone + Send + Sync {
    type State: Default + Send + Sync + 'static;
    type Instance: Send + Sync + 'static;

    fn id(&self) -> ArcStaticStr;
    fn display_name(&self) -> ArcStaticStr;

    /// Ignore `transform` if circuit doesn't support transforms, or supports auto transforms
    fn size(&self, transform: CircuitTransform) -> Vec2usize;

    /// Ignore `transform` if circuit doesn't support transforms, or supports auto transforms
    fn occupies_quarter(&self, transform: CircuitTransform, qpos: Vec2usize) -> bool {
        let _ = (transform, qpos);
        true
    }

    /// Ignore `transform` if circuit doesn't support transforms, or supports auto transforms
    fn describe_pins(&self, transform: CircuitTransform) -> Box<[PinDescription]>;

    fn transform_support(&self) -> CircuitTransformSupport {
        CircuitTransformSupport {
            rotation: Some(CircuitRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Up,
            }),
            flip: None,
        }
    }

    fn draw(&self, circuit: Option<CircuitCtx<Self>>, render: &CircuitRenderingContext);

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance;

    fn update_signals(&self, ctx: CircuitCtx<Self>, changed_pin: Option<usize>);
}

traitbox::traitbox! {
    pub box CircuitImplBox;

    #[as_impl]
    trait CircuitImpl {
        fn id(&self) -> ArcStaticStr;
        fn display_name(&self) -> ArcStaticStr;
        fn size(&self, transform: CircuitTransform) -> Vec2usize;
        fn occupies_quarter(&self, transform: CircuitTransform, qpos: Vec2usize) -> bool;
        fn describe_pins(&self, transform: CircuitTransform) -> Box<[PinDescription]>;
        fn transform_support(&self) -> CircuitTransformSupport;
    }

    impl {
        fn create_instance<C: CircuitImpl>(this: &C, circuit: &Arc<Circuit>) -> Box<dyn Any + Send + Sync> {
            Box::new(this.create_instance(circuit))
        }

        fn update_signals<C: CircuitImpl>(this: &C, ctx: UntypedCircuitCtx, changed_pin: Option<usize>) {
            this.update_signals(ctx.make_typed(), changed_pin);
        }

        fn draw<C: CircuitImpl>(this: &C, circuit: Option<UntypedCircuitCtx>, render: &CircuitRenderingContext) {
            this.draw(circuit.map(|c| c.make_typed()), render);
        }
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

pub struct CircuitBlueprint {
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub imp: CircuitImplBox,
    pub inner_size: Vec2usize,
    pub transformed_size: Vec2usize,
    pub pins: Box<[PinDescription]>,
    pub transform: CircuitTransform,
}

impl CircuitBlueprint {
    pub fn new(imp: CircuitImplBox) -> Self {
        let trans_support = imp.transform_support();
        let dir = trans_support
            .rotation
            .map(|r| r.default_dir)
            .unwrap_or(Direction4::Up);
        let transform = CircuitTransform {
            support: trans_support,
            dir,
            flip: false,
        };

        let size = imp.size(transform);

        Self {
            id: imp.id(),
            display_name: imp.display_name(),
            inner_size: size,
            transformed_size: size,
            pins: imp.describe_pins(transform),
            imp,
            transform,
        }
    }

    pub fn recalculate(&mut self) {
        self.transform.support = self.imp.transform_support();
        self.inner_size = self.imp.size(self.transform);
        self.transformed_size = self
            .transform
            .transform_size(self.inner_size, Some(TransformSupport::Automatic));
        self.pins = self.imp.describe_pins(self.transform);
        self.transform.transform_pins(
            self.inner_size,
            &mut self.pins.iter_mut().map(|p| p.pos_dir_mut()),
            Some(TransformSupport::Automatic),
        );
    }
}

impl<T: CircuitImpl + 'static> From<T> for CircuitBlueprint {
    fn from(value: T) -> Self {
        Self::new(CircuitImplBox::new(value))
    }
}

pub struct PosDirMut<'a> {
    pub pos: &'a mut Vec2usize,
    pub dir: Option<&'a mut Direction8>,
}

pub const fn rotate_pos(pos: Vec2usize, target_size: Vec2usize, dir: Direction4) -> Vec2usize {
    match dir {
        Direction4::Up => pos,
        Direction4::Left => Vec2usize::new(pos.y, target_size.y - pos.x - 1),
        Direction4::Down => Vec2usize::new(target_size.x - pos.x - 1, target_size.y - pos.y - 1),
        Direction4::Right => Vec2usize::new(target_size.x - pos.y - 1, pos.x),
    }
}