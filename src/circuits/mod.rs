use std::{
    any::Any,
    collections::HashMap,
    f32::consts::TAU,
    ops::Deref,
    sync::{Arc, Weak},
};

use eframe::egui::Rect;
use parking_lot::{Mutex, RwLock};
use smoldata::raw::RawValue;

use crate::{
    Direction4, Direction8, PaintContext,
    board::{Board, Wire},
    circuits::props::{PropertyInfo, PropertyValue},
    io::savestate,
    selection::SelectionRenderer,
    state::{circuits::BoardCircuitsState, sim::UpdateTaskPool, wires::WireState},
    str::ArcStaticStr,
    vector::{Vec2f, Vec2isize, Vec2usize},
};

pub mod buffer;
pub mod button;
pub mod constant;
pub mod error_filter;
pub mod gates;
pub mod test;

pub mod props;

pub struct Circuit {
    pub id: usize,
    pub board: Weak<Board>,
    pub info: RwLock<CircuitInfo>,
    pub imp: RwLock<CircuitImplData>,
    pub pins: RwLock<Box<[RealizedPin]>>,
}

impl Circuit {
    pub fn save(&self) -> savestate::Circuit {
        let imp = self.imp.read();
        let info = self.info.read();

        savestate::Circuit {
            id: imp.imp.id(),
            pos: info.pos,
            dir: info.transform.dir,
            flip: info.transform.flip,
            config: imp.imp.save_config(),
            instance: imp.imp.save_instance(self, &imp.instance),
        }
    }

    pub fn preload(
        id: usize,
        board: &Arc<Board>,
        circuit_data: &savestate::Circuit,
        blueprints: &HashMap<ArcStaticStr, Arc<RwLock<CircuitBlueprint>>>,
    ) -> Circuit {
        let Some(blueprint) = blueprints.get(&circuit_data.id) else {
            todo!("unloaded circuit");
        };

        let blueprint = blueprint.read();

        let info = CircuitInfo {
            pos: circuit_data.pos,
            render_size: 0.into(), // calculated later
            size: 0.into(),        // calculated later
            transform: CircuitTransform {
                support: blueprint.transform.support,
                dir: circuit_data.dir,
                flip: circuit_data.flip,
            },
        };

        let imp = CircuitImplData {
            imp: blueprint.imp.clone(),
            instance: Box::new(()), // loaded later
        };

        Self {
            id,
            board: Arc::downgrade(board),
            info: RwLock::new(info),
            imp: RwLock::new(imp),
            pins: RwLock::new(Box::new([])), // loaded later
        }
    }

    pub fn load_finish(self: &Arc<Self>, data: &savestate::Circuit) {
        let mut imp = self.imp.write();
        let mut info = self.info.write();

        if let Some(config) = &data.config {
            // todo: error handling
            imp.imp.load_config(config).ok();
        }

        info.transform.support = imp.imp.transform_support();

        info.render_size = imp.imp.size(info.transform);
        info.size = info
            .transform
            .transform_size(info.render_size, Some(TransformSupport::Automatic));

        let mut pins = imp.imp.describe_pins(info.transform);
        info.transform.transform_pins(
            info.render_size,
            &mut pins.iter_mut().map(|p| p.pos_dir_mut()),
            Some(TransformSupport::Automatic),
        );

        *self.pins.write() = pins
            .into_vec()
            .into_iter()
            .enumerate()
            .map(|(id, desc)| RealizedPin {
                pin: Arc::new(CircuitPin {
                    id,
                    ty: desc.ty,
                    circuit: self.clone(),
                    wire: RwLock::new(None),
                }),
                desc,
            })
            .collect();

        // todo: error reporting
        imp.instance = data
            .instance
            .as_ref()
            .and_then(|d| imp.imp.load_instance(self, d).ok())
            .unwrap_or_else(|| imp.imp.create_instance(self));
    }
}

#[derive(Clone)]
pub struct CircuitInfo {
    pub pos: Vec2isize,

    /// Size before transformations
    pub render_size: Vec2usize,
    pub size: Vec2usize,
    pub transform: CircuitTransform,
}

pub struct CircuitImplData {
    pub imp: CircuitImplBox,
    pub instance: Box<dyn Any + Send + Sync>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PinType {
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
    pub fn pos_dir_mut(&mut self) -> PosDirMut<'_> {
        PosDirMut {
            pos: &mut self.pos,
            dir: self.dir.as_mut(),
        }
    }

    /// True if pin descriptions are equal, ignoring visual differences
    pub fn functionally_equals(&self, other: &Self) -> bool {
        self.id == other.id && self.pos == other.pos && self.ty == other.ty
    }

    pub fn into_realized(self, circuit: Arc<Circuit>, id: usize) -> RealizedPin {
        RealizedPin {
            pin: Arc::new(CircuitPin {
                id,
                wire: RwLock::new(None),
                ty: self.ty,
                circuit,
            }),
            desc: self,
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
        board_state: &mut BoardCircuitsState,
        tasks: &mut UpdateTaskPool,
        state: WireState,
    ) {
        let changed = board_state.set_pin(self.circuit.id, self.id, state);
        if changed && let Some(wire) = self.wire.read().as_ref().map(|w| w.id) {
            tasks.add_wire_task(wire, false);
        }
    }

    pub fn get_state(&self, state: &BoardCircuitsState) -> WireState {
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
            PinType::Inside => {
                tasks.add_update_input_task(self.circuit.id, self.id, true);
            }
            PinType::Outside => {
                tasks.add_wire_task(old_wire, true);
            }
        }
    }

    fn handle_connect(&self, wire: usize, tasks: &mut UpdateTaskPool) {
        match self.ty {
            PinType::Outside => {
                tasks.add_wire_task(wire, true);
            }
            PinType::Inside => {
                tasks.add_update_input_task(self.circuit.id, self.id, true);
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

#[derive(Clone, Copy)]
pub enum CircuitRenderPurpose {
    Icon,
    PlacementPreview,
    InWorld,
}

pub struct CircuitRenderingContext<'a> {
    pub paint: &'a PaintContext<'a>,
    pub screen_rect: Rect,
    pub selection: Option<CircuitSelectionRenderingContext<'a>>,
    pub transform: CircuitTransform,
    pub purpose: CircuitRenderPurpose,

    // internal for transform_pos
    world_size: Vec2usize,
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
        purpose: CircuitRenderPurpose,
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
            world_size: render_size,
            selection,
            transform,
            purpose,
            angle,
            flip,
        }
    }

    /// Transform circuit coordinate [0..size] to screen coordinate
    pub fn transform_pos(&self, pos: Vec2f) -> Vec2f {
        let norm = pos / self.world_size.convert(|v| v as f32);

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

        self.screen_rect.lerp_inside(norm).into()
    }

    pub fn world_size(&self) -> Vec2usize {
        self.world_size
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

impl FlipType {
    fn has_vertical(self) -> bool {
        match self {
            FlipType::Vertical => true,
            FlipType::Horizontal => false,
            FlipType::Both => true,
        }
    }

    fn has_horizontal(self) -> bool {
        match self {
            FlipType::Vertical => false,
            FlipType::Horizontal => true,
            FlipType::Both => true,
        }
    }
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
            Some(ft) => {
                let x = if ft.has_horizontal() && size.x > 1 {
                    (size.x - 1) - pos.x
                } else {
                    pos.x
                };
                let y = if ft.has_vertical() && size.y > 1 {
                    (size.y - 1) - pos.y
                } else {
                    pos.y
                };
                [x, y].into()
            }
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
            Some(ft) => {
                let x = if ft.has_horizontal() && size.x > 1 {
                    (size.x - 1) - rotated_pos.x
                } else {
                    rotated_pos.x
                };
                let y = if ft.has_vertical() && size.y > 1 {
                    (size.y - 1) - rotated_pos.y
                } else {
                    rotated_pos.y
                };
                [x, y].into()
            }
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
                let default_rotated = self.dir.rotated_counterclockwise_by(default_dir);
                flipped.rotated_clockwise_by(default_rotated.into())
            }
        }
    }

    pub fn backtransform_dir(
        &self,
        dir: Direction8,
        support: Option<TransformSupport>,
    ) -> Direction8 {
        let default_dir = self.support.rotation_default_dir(support);

        let dir = match default_dir {
            None => dir,
            Some(default_dir) => {
                let default_rotated = self.dir.rotated_counterclockwise_by(default_dir);
                dir.rotated_counterclockwise_by(default_rotated.into())
            }
        };

        let flip = self.flip.then(|| self.support.flip_type(support)).flatten();

        match flip {
            None => dir,
            Some(FlipType::Vertical) => dir.flip_by(Direction8::Left),
            Some(FlipType::Horizontal) => dir.flip_by(Direction8::Up),
            Some(FlipType::Both) => dir.inverted(),
        }
    }

    pub fn transform_pins(
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

// TODO: read-only state for draws
pub struct UntypedCircuitCtx<'a> {
    pub state: &'a mut BoardCircuitsState,
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

pub struct PropertyChangedParams {
    pub trigger_signal_update: bool,
}

impl Default for PropertyChangedParams {
    fn default() -> Self {
        Self {
            trigger_signal_update: true,
        }
    }
}

pub struct CircuitCtx<'a, C: CircuitImpl> {
    pub state: &'a mut BoardCircuitsState,
    pub circuit: &'a Arc<Circuit>,
    pub tasks: &'a mut UpdateTaskPool,
    pub instance: &'a C::Instance,
}

impl<C: CircuitImpl> CircuitCtx<'_, C> {
    fn set_pin_output(&mut self, pin: &CircuitPin, state: WireState) {
        pin.set_output(self.state, self.tasks, state);
    }

    fn get_pin_input(&self, pin: &CircuitPin) -> WireState {
        pin.get_state(self.state)
    }

    fn read_internal_state(&self) -> Option<&C::State> {
        self.state.read_internal_circuit_state(self.circuit.id)
    }

    fn write_internal_state(&mut self) -> &mut C::State {
        self.state.write_internal_circuit_state(self.circuit.id)
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

    fn save_config(&self) -> Option<RawValue> {
        None
    }

    fn save_instance(&self, circuit: &Circuit, instance: &Self::Instance) -> Option<RawValue> {
        let _ = (circuit, instance);
        None
    }

    fn save_state(
        &self,
        circuit: &Circuit,
        instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<RawValue> {
        let _ = (circuit, instance, state);
        None
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        let _ = data;
        Ok(())
    }

    fn load_instance(
        &self,
        circuit: &Arc<Circuit>,
        data: &RawValue,
    ) -> Result<Self::Instance, eyre::Report> {
        let _ = data;
        Ok(self.create_instance(circuit))
    }

    fn load_state(
        &self,
        circuit: &Arc<Circuit>,
        instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        let _ = (circuit, instance, data);

        Ok(Self::State::default())
    }

    fn draw_blueprint_pins(&self) -> bool {
        true
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        let _ = f;
    }

    fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue> {
        let _ = id;
        None
    }

    fn property_changed(
        &self,
        circuit_instance: Option<(&Circuit, &mut Self::Instance)>,
        prop: &str,
        params: &mut PropertyChangedParams,
    ) {
        let _ = (circuit_instance, prop, params);
    }
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
        fn draw_blueprint_pins(&self) -> bool;
        fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo));
        fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue>;
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

        fn save_config<C: CircuitImpl>(this: &C) -> Option<RawValue> {
            this.save_config()
        }

        fn save_instance<C: CircuitImpl>(this: &C, circuit: &Circuit, instance: &Box<dyn Any + Send + Sync>) -> Option<RawValue> {
            this.save_instance(circuit, instance.downcast_ref()?)
        }

        fn save_state<C: CircuitImpl>(this: &C, circuit: &Circuit, instance: &Box<dyn Any + Send + Sync>, state: &Box<dyn Any + Send + Sync>) -> Option<RawValue> {
            this.save_state(circuit, instance.downcast_ref()?, state.downcast_ref()?)
        }

        fn load_config<C: CircuitImpl>(this: &mut C, data: &RawValue) -> Result<(), eyre::Report> {
            this.load_config(data)
        }

        fn load_instance<C: CircuitImpl>(this: &C, circuit: &Arc<Circuit>, data: &RawValue) -> Result<Box<dyn Any + Send + Sync>, eyre::Report> {
            this.load_instance(circuit, data).map(|i| Box::new(i) as Box<_>)
        }

        fn load_state<C: CircuitImpl>(this: &C, circuit: &Arc<Circuit>, instance: &Box<dyn Any + Send + Sync>, data: &RawValue) -> Result<Box<dyn Any + Send + Sync>, eyre::Report> {
            this.load_state(circuit, instance.downcast_ref().expect("incorrect circuit instance"), data).map(|i| Box::new(i) as Box<_>)
        }

        fn property_changed<C: CircuitImpl>(this: &C, circuit_instance: Option<(&Circuit, &mut Box<dyn Any + Send + Sync>)>, prop: &str, params: &mut PropertyChangedParams) {
            this.property_changed(circuit_instance.map(|(c, i)| (c, i.downcast_mut().expect("incorrect circuit instance"))), prop, params)
        }
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

#[derive(Clone)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transforms() {
        for start_dir in Direction4::ALL {
            for flip in [FlipType::Horizontal, FlipType::Vertical, FlipType::Both] {
                let support = CircuitTransformSupport {
                    rotation: Some(CircuitRotationSupport {
                        support: TransformSupport::Automatic,
                        default_dir: start_dir,
                    }),
                    flip: Some(CircuitFlipSupport {
                        support: TransformSupport::Automatic,
                        ty: flip,
                    }),
                };

                for dir in Direction4::ALL {
                    for flip in [false, true] {
                        let tr = CircuitTransform { support, dir, flip };

                        for dir2 in Direction8::ALL {
                            let int = tr.transform_dir(dir2, None);
                            assert_eq!(
                                tr.backtransform_dir(int, None),
                                dir2,
                                "retransform {dir:?} through {tr:?}, intermediate {int:?}"
                            );
                        }

                        let size = Vec2usize::new(10, 10);

                        for x in 0..10 {
                            for y in 0..10 {
                                let pos = Vec2usize::new(x, y);
                                let int = tr.transform_pos(size, pos, None);
                                assert_eq!(
                                    tr.backtransform_pos(size, int, None),
                                    pos,
                                    "retransform {pos:?} through {tr:?}, intermediate {int:?}"
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}
