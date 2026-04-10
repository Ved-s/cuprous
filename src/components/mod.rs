use std::{
    any::Any,
    collections::HashMap,
    f32::consts::TAU,
    ops::Deref,
    sync::{Arc, Weak},
    time::Duration,
};

use eframe::egui::Rect;
use parking_lot::{Mutex, RwLock};
use smoldata::{SmolReadWrite, raw::RawValue};

use crate::{
    Direction4, Direction8, PaintContext,
    board::{Board, Wire},
    components::props::{PropertyInfo, PropertyValue},
    io::savestate,
    selection::SelectionRenderer,
    state::{BoardState, components::BoardComponentsState, sim::UpdateTaskPool, wires::WireState},
    str::ArcStaticStr,
    time::{self, Instant, TimeProvider},
    vector::{Vec2f, Vec2isize, Vec2usize},
};

pub mod buffer;
pub mod button;
pub mod constant;
pub mod error_filter;
pub mod gates;
pub mod test;

pub mod props;

pub struct Component {
    pub id: usize,
    pub board: Weak<Board>,
    pub info: RwLock<ComponentInfo>,
    pub imp: RwLock<ComponentImplData>,
    pub pins: RwLock<Box<[RealizedPin]>>,
}

impl Component {
    pub fn save(&self) -> savestate::Component {
        let imp = self.imp.read();
        let info = self.info.read();

        savestate::Component {
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
        component_data: &savestate::Component,
        blueprints: &HashMap<ArcStaticStr, Arc<RwLock<ComponentBlueprint>>>,
    ) -> Component {
        let Some(blueprint) = blueprints.get(&component_data.id) else {
            todo!("unloaded component");
        };

        let blueprint = blueprint.read();

        let info = ComponentInfo {
            pos: component_data.pos,
            render_size: 0.into(), // calculated later
            size: 0.into(),        // calculated later
            transform: ComponentTransform {
                support: blueprint.transform.support,
                dir: component_data.dir,
                flip: component_data.flip,
            },
        };

        let imp = ComponentImplData {
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

    pub fn load_finish(self: &Arc<Self>, data: &savestate::Component) {
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
                pin: Arc::new(ComponentPin {
                    id,
                    ty: desc.ty,
                    component: self.clone(),
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
pub struct ComponentInfo {
    pub pos: Vec2isize,

    /// Size before transformations
    pub render_size: Vec2usize,
    pub size: Vec2usize,
    pub transform: ComponentTransform,
}

pub struct ComponentImplData {
    pub imp: ComponentImplBox,
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

    pub fn into_realized(self, component: Arc<Component>, id: usize) -> RealizedPin {
        RealizedPin {
            pin: Arc::new(ComponentPin {
                id,
                wire: RwLock::new(None),
                ty: self.ty,
                component,
            }),
            desc: self,
        }
    }
}

pub struct ComponentPin {
    pub id: usize,
    pub ty: PinType,
    pub component: Arc<Component>,
    pub wire: RwLock<Option<Arc<Wire>>>,
}
impl ComponentPin {
    pub fn set_output(
        &self,
        board_state: &mut BoardComponentsState,
        tasks: &mut UpdateTaskPool,
        state: WireState,
    ) {
        let changed = board_state.set_pin(self.component.id, self.id, state);
        if changed && let Some(wire) = self.wire.read().as_ref().map(|w| w.id) {
            tasks.add_wire_task(wire, false);
        }
    }

    pub fn get_state(&self, state: &BoardComponentsState) -> WireState {
        state.get_pin(self.component.id, self.id)
    }

    pub(crate) fn disconnect(&self, tasks: &mut UpdateTaskPool) {
        let mut pin_wire = self.wire.write();
        let Some(wire) = pin_wire.deref() else {
            return;
        };

        wire.remove_pin(self.component.id, self.id);
        let id = wire.id;

        *pin_wire = None;

        self.handle_disconnect(id, tasks);
    }

    pub(crate) fn connect(self: &Arc<Self>, wire: Arc<Wire>, tasks: &mut UpdateTaskPool) {
        let mut pin_wire = self.wire.write();
        if let Some(wire) = pin_wire.deref() {
            wire.remove_pin(self.component.id, self.id);
            self.handle_disconnect(wire.id, tasks);
        };

        wire.add_pin(self.component.clone(), self.clone());
        let id = wire.id;
        *pin_wire = Some(wire);
        self.handle_connect(id, tasks);
    }

    fn handle_disconnect(&self, old_wire: usize, tasks: &mut UpdateTaskPool) {
        match self.ty {
            PinType::Inside => {
                tasks.add_update_input_task(self.component.id, self.id, true);
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
                tasks.add_update_input_task(self.component.id, self.id, true);
            }
        }
    }
}

pub struct RealizedPin {
    pub desc: PinDescription,
    pub pin: Arc<ComponentPin>,
}

pub struct ComponentSelectionRenderingContext<'a> {
    pub renderer: Arc<Mutex<SelectionRenderer>>,
    pub custom_selection: &'a mut bool,
}

#[derive(Clone, Copy)]
pub enum ComponentRenderPurpose {
    Icon,
    PlacementPreview,
    InWorld,
}

pub struct ComponentRenderingContext<'a> {
    pub paint: &'a PaintContext<'a>,
    pub screen_rect: Rect,
    pub selection: Option<ComponentSelectionRenderingContext<'a>>,
    pub transform: ComponentTransform,
    pub purpose: ComponentRenderPurpose,

    // internal for transform_pos
    world_size: Vec2usize,
    angle: Option<f32>,
    flip: Option<FlipType>,
}

impl<'a> ComponentRenderingContext<'a> {
    pub fn new(
        ctx: &'a PaintContext,
        screen_rect: Rect,
        render_size: Vec2usize,
        selection: Option<ComponentSelectionRenderingContext<'a>>,
        transform: ComponentTransform,
        purpose: ComponentRenderPurpose,
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

    /// Transform component coordinate [0..size] to screen coordinate
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
pub struct ComponentRotationSupport {
    pub support: TransformSupport,
    pub default_dir: Direction4,
}

#[derive(Debug, Clone, Copy)]
pub struct ComponentFlipSupport {
    pub support: TransformSupport,
    pub ty: FlipType,
}

#[derive(Debug, Clone, Copy)]
pub struct ComponentTransformSupport {
    pub rotation: Option<ComponentRotationSupport>,
    pub flip: Option<ComponentFlipSupport>,
}

impl ComponentTransformSupport {
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
pub struct ComponentTransform {
    pub support: ComponentTransformSupport,
    pub dir: Direction4,
    pub flip: bool,
}
impl ComponentTransform {
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

pub struct PropertyChangedParams {
    pub trigger_update: bool,
}

impl Default for PropertyChangedParams {
    fn default() -> Self {
        Self {
            trigger_update: true,
        }
    }
}

#[derive(Debug, Clone, SmolReadWrite, Hash, PartialEq, Eq)]
pub enum ComponentUpdateReason {
    ChangedPin(usize),
    PropertyChanged(ArcStaticStr),
    NewPins,
    ComponentPlaced,
    StateReset,
    Timer,
}

// TODO: read-only state for draws (depends on widgets)
pub struct UntypedComponentCtx<'a> {
    pub state: &'a mut BoardState,
    pub component: &'a Arc<Component>,
    pub tasks: &'a mut UpdateTaskPool,
    pub instance: &'a dyn Any,
}

impl<'a> UntypedComponentCtx<'a> {
    pub fn make_typed<C: ComponentImpl>(self) -> ComponentCtx<'a, C> {
        ComponentCtx {
            state: self.state,
            component: self.component,
            tasks: self.tasks,
            instance: self
                .instance
                .downcast_ref::<C::Instance>()
                .expect("correct instance for a component"),
        }
    }
}

pub struct ComponentCtx<'a, C: ComponentImpl> {
    pub state: &'a mut BoardState,
    pub component: &'a Arc<Component>,
    pub tasks: &'a mut UpdateTaskPool,
    pub instance: &'a C::Instance,
}

impl<C: ComponentImpl> ComponentCtx<'_, C> {
    pub fn set_pin_output(&mut self, pin: &ComponentPin, state: WireState) {
        pin.set_output(&mut self.state.components, self.tasks, state);
    }

    pub fn get_pin_input(&self, pin: &ComponentPin) -> WireState {
        pin.get_state(&self.state.components)
    }

    pub fn read_internal_state(&self) -> Option<&C::State> {
        self.state
            .components
            .read_internal_component_state(self.component.id)
    }

    pub fn write_internal_state(&mut self) -> &mut C::State {
        self.state
            .components
            .write_internal_component_state(self.component.id)
    }

    pub fn time_provider(&self) -> &dyn TimeProvider {
        // todo: correct time provider
        &time::SYSTEM
    }

    pub fn get_timer(&self) -> Option<(Instant, Option<Duration>)> {
        self.state.get_timer(self.component.id)
    }

    pub fn set_timer(&mut self, at: Instant, interval: Option<Duration>) {
        self.state.set_timer(self.component.id, at, interval)
    }

    pub fn reset_timer(&mut self) {
        self.state.reset_timer(self.component.id)
    }
}

pub trait ComponentImpl: Clone + Send + Sync {
    type State: Default + Send + Sync + 'static;
    type Instance: Send + Sync + 'static;

    fn id(&self) -> ArcStaticStr;
    fn display_name(&self) -> ArcStaticStr;

    /// Ignore `transform` if component doesn't support transforms, or supports auto transforms
    fn size(&self, transform: ComponentTransform) -> Vec2usize;

    /// Ignore `transform` if component doesn't support transforms, or supports auto transforms
    fn occupies_quarter(&self, transform: ComponentTransform, qpos: Vec2usize) -> bool {
        let _ = (transform, qpos);
        true
    }

    /// Ignore `transform` if component doesn't support transforms, or supports auto transforms
    fn describe_pins(&self, transform: ComponentTransform) -> Box<[PinDescription]>;

    fn transform_support(&self) -> ComponentTransformSupport {
        ComponentTransformSupport {
            rotation: Some(ComponentRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Up,
            }),
            flip: None,
        }
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext);

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance;

    fn update(&self, ctx: ComponentCtx<Self>, reason: ComponentUpdateReason);

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let _ = (component, instance);
    }

    fn save_config(&self) -> Option<RawValue> {
        None
    }

    fn save_instance(&self, component: &Component, instance: &Self::Instance) -> Option<RawValue> {
        let _ = (component, instance);
        None
    }

    fn save_state(
        &self,
        component: &Component,
        instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<RawValue> {
        let _ = (component, instance, state);
        None
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        let _ = data;
        Ok(())
    }

    fn load_instance(
        &self,
        component: &Arc<Component>,
        data: &RawValue,
    ) -> Result<Self::Instance, eyre::Report> {
        let _ = data;
        Ok(self.create_instance(component))
    }

    fn load_state(
        &self,
        component: &Arc<Component>,
        instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        let _ = (component, instance, data);

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
        component_instance: Option<(&Component, &mut Self::Instance)>,
        prop: &str,
        params: &mut PropertyChangedParams,
    ) {
        let _ = (component_instance, prop, params);
    }
}

traitbox::traitbox! {
    pub box ComponentImplBox;

    #[as_impl]
    trait ComponentImpl {
        fn id(&self) -> ArcStaticStr;
        fn display_name(&self) -> ArcStaticStr;
        fn size(&self, transform: ComponentTransform) -> Vec2usize;
        fn occupies_quarter(&self, transform: ComponentTransform, qpos: Vec2usize) -> bool;
        fn describe_pins(&self, transform: ComponentTransform) -> Box<[PinDescription]>;
        fn transform_support(&self) -> ComponentTransformSupport;
        fn draw_blueprint_pins(&self) -> bool;
        fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo));
        fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue>;
    }

    impl {
        fn create_instance<C: ComponentImpl>(this: &C, component: &Arc<Component>) -> Box<dyn Any + Send + Sync> {
            Box::new(this.create_instance(component))
        }

        fn update<C: ComponentImpl>(this: &C, ctx: UntypedComponentCtx, reason: ComponentUpdateReason) {
            this.update(ctx.make_typed(), reason);
        }

        fn pins_changed<C: ComponentImpl>(this: &C, component: &Component, instance: &mut Box<dyn Any + Send + Sync>) {
            this.pins_changed(component, instance.downcast_mut().expect("incorrect component instance"));
        }

        fn draw<C: ComponentImpl>(this: &C, component: Option<UntypedComponentCtx>, render: &ComponentRenderingContext) {
            this.draw(component.map(|c| c.make_typed()), render);
        }

        fn save_config<C: ComponentImpl>(this: &C) -> Option<RawValue> {
            this.save_config()
        }

        fn save_instance<C: ComponentImpl>(this: &C, component: &Component, instance: &Box<dyn Any + Send + Sync>) -> Option<RawValue> {
            this.save_instance(component, instance.downcast_ref()?)
        }

        fn save_state<C: ComponentImpl>(this: &C, component: &Component, instance: &Box<dyn Any + Send + Sync>, state: &Box<dyn Any + Send + Sync>) -> Option<RawValue> {
            this.save_state(component, instance.downcast_ref()?, state.downcast_ref()?)
        }

        fn load_config<C: ComponentImpl>(this: &mut C, data: &RawValue) -> Result<(), eyre::Report> {
            this.load_config(data)
        }

        fn load_instance<C: ComponentImpl>(this: &C, component: &Arc<Component>, data: &RawValue) -> Result<Box<dyn Any + Send + Sync>, eyre::Report> {
            this.load_instance(component, data).map(|i| Box::new(i) as Box<_>)
        }

        fn load_state<C: ComponentImpl>(this: &C, component: &Arc<Component>, instance: &Box<dyn Any + Send + Sync>, data: &RawValue) -> Result<Box<dyn Any + Send + Sync>, eyre::Report> {
            this.load_state(component, instance.downcast_ref().expect("incorrect component instance"), data).map(|i| Box::new(i) as Box<_>)
        }

        fn property_changed<C: ComponentImpl>(this: &C, component_instance: Option<(&Component, &mut Box<dyn Any + Send + Sync>)>, prop: &str, params: &mut PropertyChangedParams) {
            this.property_changed(component_instance.map(|(c, i)| (c, i.downcast_mut().expect("incorrect component instance"))), prop, params)
        }
    }

    trait Clone {
        fn clone(&self) -> Self;
    }

    auto trait Send {}
    auto trait Sync {}
}

#[derive(Clone)]
pub struct ComponentBlueprint {
    pub id: ArcStaticStr,
    pub display_name: ArcStaticStr,
    pub imp: ComponentImplBox,
    pub inner_size: Vec2usize,
    pub transformed_size: Vec2usize,
    pub pins: Box<[PinDescription]>,
    pub transform: ComponentTransform,
}

impl ComponentBlueprint {
    pub fn new(imp: ComponentImplBox) -> Self {
        let trans_support = imp.transform_support();
        let dir = trans_support
            .rotation
            .map(|r| r.default_dir)
            .unwrap_or(Direction4::Up);
        let transform = ComponentTransform {
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
        self.display_name = self.imp.display_name()
    }
}

impl<T: ComponentImpl + 'static> From<T> for ComponentBlueprint {
    fn from(value: T) -> Self {
        Self::new(ComponentImplBox::new(value))
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
                let support = ComponentTransformSupport {
                    rotation: Some(ComponentRotationSupport {
                        support: TransformSupport::Automatic,
                        default_dir: start_dir,
                    }),
                    flip: Some(ComponentFlipSupport {
                        support: TransformSupport::Automatic,
                        ty: flip,
                    }),
                };

                for dir in Direction4::ALL {
                    for flip in [false, true] {
                        let tr = ComponentTransform { support, dir, flip };

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
