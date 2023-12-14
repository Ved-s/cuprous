use std::{marker::PhantomData, sync::Weak};

use eframe::epaint::{Color32, Stroke};
use emath::{pos2, vec2, Pos2};

use crate::{
    board::ActiveCircuitBoard,
    circuits::{
        props::{CircuitProperty, CircuitPropertyStore, RangedValue},
        *,
    },
    containers::FixedVec,
    describe_directional_circuit,
    path::{PathItem, PathItemIterator},
    vector::Vec2f,
    Direction4, Mutex,
};

pub enum GateWireStates<'a> {
    States(&'a [WireState]),
    Default(u32),
}

impl<'a> GateWireStates<'a> {
    pub fn count(&self) -> usize {
        match self {
            GateWireStates::States(s) => s.len(),
            GateWireStates::Default(c) => *c as usize,
        }
    }

    pub fn get(&self, index: usize, default: WireState) -> WireState {
        match self {
            GateWireStates::States(s) => s.get(index).copied().unwrap_or(default),
            GateWireStates::Default(_) => default,
        }
    }
}

pub trait GateImpl {
    const TYPE: &'static str;
    const OP_DESC: &'static str;

    fn process(inputs: &[bool], extra: bool) -> bool;
    fn draw(
        wires: GateWireStates,
        angle: f32,
        in_world_preview: bool,
        extra: bool,
        ctx: &PaintContext,
    );

    fn extra_toggle_name() -> Option<&'static str> {
        None
    }
}

thread_local! {
    static INPUT_STATES: Mutex<Vec<WireState>> = Default::default();
    static INPUT_BOOLS: Mutex<Vec<bool>> = Default::default();
    static INPUT_DISPLAY_NAMES: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static INPUT_NAMES: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
}

struct Gate<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    inputs: Box<[CircuitPinInfo]>,
    dir: Direction4,
    extra: bool,
    output: CircuitPinInfo,
    _phantom: PhantomData<I>,
}

impl<I> Gate<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    fn new() -> Self {
        let description = Self::describe(Direction4::Right, 2);
        let output = description
            .pins
            .last()
            .expect("pins should exist!")
            .to_info();
        let inputs = description.pins[..description.pins.len() - 1]
            .iter()
            .map(|p| p.to_info())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self {
            inputs,
            output,
            extra: false,
            dir: Direction4::Right,
            _phantom: PhantomData,
        }
    }

    fn size_props(props: &CircuitPropertyStore) -> Vec2u {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let inputs = props
            .read_clone("inputs")
            .unwrap_or(RangedValue::new_from(2.., 1, 2))
            .get();
        Self::size(dir, inputs)
    }

    // inputs: 2..=3 -> 4, 3
    // inputs: 4..=5 -> 5, 5
    fn size(dir: Direction4, inputs: u32) -> Vec2u {
        let [width, height] = calc_size_from_inputs(inputs);

        if dir.is_horizontal() {
            [width, height].into()
        } else {
            [height, width].into()
        }
    }

    fn describe_props(props: &CircuitPropertyStore) -> DynCircuitDescription {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let inputs = props
            .read_clone("inputs")
            .unwrap_or(RangedValue::new_from(2.., 1, 2))
            .get();
        Self::describe(dir, inputs)
    }

    // [in_0, in_1 ..., out]
    fn describe(dir: Direction4, inputs: u32) -> DynCircuitDescription {
        let [width, height] = calc_size_from_inputs(inputs);

        let mut pins = Vec::with_capacity(inputs as usize + 1);

        let dir_normalized = dir.rotate_counterclockwise_by(Direction4::Right);
        let size_rotated = Self::size(dir, inputs);

        let y_iter = (0..height).filter(|&y| inputs % 2 != 0 || y != inputs >> 1);

        for (i, y) in y_iter.enumerate() {
            let display_dir = Direction4::Left.rotate_clockwise_by(dir_normalized);
            let pos =
                crate::circuits::rotate_pos([0, y], size_rotated.into(), dir_normalized).into();

            let name_arc = INPUT_NAMES.with(|names| {
                let mut names = names.lock();
                if let Some(weak_name) = names.get(i) {
                    if let Some(name) = weak_name.upgrade() {
                        return name;
                    }
                }

                let name: Arc<str> = format!("in_{i}").into();
                names.set(Arc::downgrade(&name), i);
                name
            });

            let display_name_arc = INPUT_DISPLAY_NAMES.with(|names| {
                let mut names = names.lock();
                if let Some(weak_name) = names.get(i) {
                    if let Some(name) = weak_name.upgrade() {
                        return name;
                    }
                }

                let name: Arc<str> = format!("In {i}").into();
                names.set(Arc::downgrade(&name), i);
                name
            });

            let pin = CircuitPinDescription {
                active: true,
                display_name: display_name_arc.into(),
                display_dir: Some(display_dir),
                dir: InternalPinDirection::Inside,
                name: name_arc.into(),
                pos,
            };
            pins.push(pin);
        }

        let out_display_dir = Direction4::Right.rotate_clockwise_by(dir_normalized);
        let out_pos = crate::circuits::rotate_pos(
            [width - 1, inputs >> 1],
            size_rotated.into(),
            dir_normalized,
        )
        .into();

        pins.push(CircuitPinDescription {
            active: true,
            display_name: "Out".into(),
            display_dir: Some(out_display_dir),
            dir: InternalPinDirection::Outside,
            name: "out".into(),
            pos: out_pos,
        });

        DynCircuitDescription {
            size: size_rotated,
            pins: pins.into(),
        }
    }
}

impl<I> CircuitImpl for Gate<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    fn draw(&self, state: &CircuitStateContext, paint_ctx: &PaintContext) {
        let angle = self.dir.inverted_ud().angle_to_right();
        let states = self.inputs.iter().map(|i| i.get_state(state));
        INPUT_STATES.with(|s| {
            let mut s = s.lock();

            s.clear();
            s.extend(states);
            I::draw(
                GateWireStates::States(&s),
                angle,
                false,
                self.extra,
                paint_ctx,
            );
        });
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);
        self.output = description
            .pins
            .last()
            .expect("pins should exist!")
            .to_info();
        self.inputs = description.pins[..description.pins.len() - 1]
            .iter()
            .map(|p| p.to_info())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.inputs
            .iter()
            .cloned()
            .chain([self.output.clone()])
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn update_signals(&self, ctx: &CircuitStateContext, _: Option<usize>) {
        let states = self.inputs.iter().map(|i| i.get_state(ctx));
        INPUT_BOOLS.with(|b| {
            let mut b = b.lock();

            b.clear();
            for state in states {
                match state {
                    WireState::None => continue,
                    WireState::True => b.push(true),
                    WireState::False => b.push(false),
                    WireState::Error => {
                        self.output.set_state(ctx, WireState::Error);
                        return;
                    }
                }
            }
            if b.is_empty() {
                self.output.set_state(ctx, WireState::None);
            } else {
                self.output
                    .set_state(ctx, I::process(&b, self.extra).into());
            }
        });
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::size_props(&circ.props)
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" || prop_id == "inputs" {
            *resize = true;
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        self.dir = circ.props.read_clone("dir").unwrap_or(Direction4::Right);
        self.extra = circ.props.read_clone("extra").unwrap_or(false);
    }
}

pub struct GatePreview<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    _phantom: PhantomData<I>,
    id: Arc<str>,
    name: Arc<str>,
    description: Arc<str>,
}

impl<I> GatePreview<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    pub fn new() -> Self {
        let ty = I::TYPE;
        let ty_upper = ty.to_uppercase();
        let ty_upper = &&ty_upper;
        let op_desc = I::OP_DESC;
        let id = ty.to_lowercase().into();
        let name = format!("{ty_upper} gate").into();
        let description = format!("{ty_upper} gate, performing logical {ty_upper} operation.\n\
                                   {op_desc}\n\
                                   \n\
                                   Can be configured to have multiple inputs.\n\
                                   Inputs with None state are ignored.\n\
                                   Output is Error when any input is Error.\
                                  ").into();
        Self {
            _phantom: PhantomData,
            id,
            name,
            description,
        }
    }
}

impl<I> CircuitPreviewImpl for GatePreview<I>
where
    I: GateImpl + Send + Sync + 'static,
{
    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let inputs = props
            .read_clone("inputs")
            .unwrap_or(RangedValue::new_from(2.., 1, 2))
            .get();
        let extra = props.read_clone("extra").unwrap_or(false);
        let angle = dir.inverted_ud().angle_to_right();
        I::draw(GateWireStates::Default(inputs), angle, in_world, extra, ctx);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Gate::<I>::new())
    }

    fn type_name(&self) -> DynStaticStr {
        self.id.clone().into()
    }

    fn display_name(&self) -> DynStaticStr {
        self.name.clone().into()
    }

    fn description(&self) -> DynStaticStr {
        self.description.clone().into()
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        let copy = Self {
            _phantom: PhantomData,
            id: self.id.clone(),
            name: self.name.clone(),
            description: self.description.clone(),
        };
        Some(Box::new(copy))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        let mut props = vec![
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("inputs", "Inputs", RangedValue::new_from(2u32.., 1, 2)),
        ];

        if let Some(name) = I::extra_toggle_name() {
            props.push(CircuitProperty::new("extra", name, false));
        }
        CircuitPropertyStore::new(props)
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let inputs = props
            .read_clone("inputs")
            .unwrap_or(RangedValue::new_from(2.., 1, 2))
            .get();
        Gate::<I>::describe(dir, inputs)
    }
}

struct Gate2497 {
    in_a: CircuitPinInfo,
    in_b: CircuitPinInfo,
    out: CircuitPinInfo,
    dir: Direction4,
    switch: bool,
}

impl Gate2497 {
    fn new() -> Self {
        let description = Self::describe(Direction4::Right);
        Self {
            in_a: description.pins[0].to_info(),
            in_b: description.pins[1].to_info(),
            out: description.pins[2].to_info(),
            dir: Direction4::Right,
            switch: false,
        }
    }

    fn draw(angle: f32, in_states: [WireState; 2], in_world_preview: bool, ctx: &PaintContext) {
        let size = vec2(5.0, 3.0);
        let transformer = |p: Pos2| {
            ctx.rect.lerp_inside(
                Vec2f::from(p.to_vec2() / size)
                    .rotated_xy(angle, 0.5)
                    .into(),
            )
        };

        let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.02);

        ctx.paint.line_segment(
            [transformer(pos2(0.5, 0.5)), transformer(pos2(0.8, 0.5))],
            Stroke::new(
                ActiveCircuitBoard::WIRE_THICKNESS * ctx.screen.scale,
                in_states[0].color(),
            ),
        );

        ctx.paint.line_segment(
            [transformer(pos2(0.5, 2.5)), transformer(pos2(0.8, 2.5))],
            Stroke::new(
                ActiveCircuitBoard::WIRE_THICKNESS * ctx.screen.scale,
                in_states[1].color(),
            ),
        );

        let fill = [
            PathItem::MoveTo(pos2(4.25, 1.5)),
            PathItem::QuadraticBezier(pos2(3.25, 0.0), pos2(2.25, 0.0)),
            PathItem::LineTo(pos2(0.25, 0.0)),
            PathItem::CubicBezier(pos2(1.2, 0.8), pos2(1.2, 2.2), pos2(0.25, 3.0)),
            PathItem::LineTo(pos2(2.25, 3.0)),
            PathItem::QuadraticBezier(pos2(3.25, 3.0), pos2(4.25, 1.5)),
            PathItem::ClosePath,
        ];

        let outer = [
            PathItem::MoveTo(pos2(0.26, 0.0)),
            PathItem::LineTo(pos2(2.25, 0.0)),
            PathItem::QuadraticBezier(pos2(3.25, 0.0), pos2(4.25, 1.5)),
            PathItem::QuadraticBezier(pos2(3.25, 3.0), pos2(2.25, 3.0)),
            PathItem::LineTo(pos2(0.25, 3.0)),
        ];

        let inner = [
            PathItem::MoveTo(pos2(0.26, -0.025)),
            PathItem::LineTo(pos2(0.22, -0.025)),
            PathItem::CubicBezier(
                pos2(1.2, (1.0 / 5.0) * 3.0),
                pos2(1.2, (4.0 / 5.0) * 3.0),
                pos2(0.22, 3.0 + 0.025),
            ),
            PathItem::LineTo(pos2(0.26, 3.0 + 0.025)),
        ];

        let opacity = if in_world_preview { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        fill.into_iter().create_path_shapes(
            fill_color,
            Stroke::NONE,
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );

        outer.into_iter().create_path_shapes(
            Color32::TRANSPARENT,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );

        inner.into_iter().create_path_shapes(
            Color32::TRANSPARENT,
            Stroke::new(0.1 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );

        let top_not = [
            PathItem::MoveTo(pos2(1.5, 0.0)),
            PathItem::LineTo(pos2(2.25, 0.4)),
            PathItem::LineTo(pos2(2.25, -0.4)),
            PathItem::ClosePath,
        ];
        top_not.into_iter().create_path_shapes(
            fill_color,
            Stroke::new(0.1 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );
        ctx.paint.circle(
            transformer(pos2(1.4, 0.0)),
            0.15 * ctx.screen.scale,
            fill_color,
            Stroke::new(0.1 * ctx.screen.scale, border_color),
        );

        let bottom_not = [
            PathItem::MoveTo(pos2(2.0, 3.0)),
            PathItem::LineTo(pos2(1.25, 3.4)),
            PathItem::LineTo(pos2(1.25, 2.6)),
            PathItem::ClosePath,
        ];
        bottom_not.into_iter().create_path_shapes(
            fill_color,
            Stroke::new(0.1 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );
        ctx.paint.circle(
            transformer(pos2(2.1, 3.0)),
            0.15 * ctx.screen.scale,
            fill_color,
            Stroke::new(0.1 * ctx.screen.scale, border_color),
        );

        let circle_pos = transformer(pos2(4.32, 1.5));
        ctx.paint.circle(
            circle_pos,
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
        );
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<3> {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Self::describe(dir)
    }

    fn describe(dir: Direction4) -> CircuitDescription<3> {
        describe_directional_circuit! {
            default_dir: Right,
            dir: dir,
            size: [5, 3],

            "in_a": Inside, "A", Left, [0, 0],
            "in_b": Inside, "B", Left, [0, 2],
            "out": Outside, "Out", Right, [4, 1],
        }
    }

    fn state_to_f32(state: WireState) -> f32 {
        match state {
            WireState::None => -1.0,
            WireState::False => 0.0,
            WireState::True => 1.0,
            WireState::Error => 2.0,
        }
    }

    fn f32_to_state(value: f32) -> WireState {
        let value = (value.round().clamp(-1.0, 2.0) + 1.0) as usize;
        match value {
            0 => WireState::None,
            1 => WireState::False,
            2 => WireState::True,
            _ => WireState::Error,
        }
    }
}

impl CircuitImpl for Gate2497 {
    fn draw(&self, ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let angle = self.dir.inverted_ud().angle_to_right();
        let states = [self.in_a.get_state(ctx), self.in_b.get_state(ctx)];
        Self::draw(angle, states, false, paint_ctx);
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let descriptions = Self::describe_props(&circ.props);
        let infos = descriptions
            .pins
            .iter()
            .map(|i| i.to_info())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.in_a = infos[0].clone();
        self.in_b = infos[1].clone();
        self.out = infos[2].clone();
        infos
    }

    fn update_signals(&self, ctx: &CircuitStateContext, _: Option<usize>) {
        let a = Self::state_to_f32(self.in_a.get_state(ctx));
        let b = Self::state_to_f32(self.in_b.get_state(ctx));
        let out = if self.switch { a + b } else { a - b };
        self.out.set_state(ctx, Self::f32_to_state(out));
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        self.switch = circ.props.read_clone("switch").unwrap_or(false);
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }
}

pub struct Gate2497Preview;

impl CircuitPreviewImpl for Gate2497Preview {
    fn type_name(&self) -> DynStaticStr {
        "gate2497".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "NORXONDOR GORGONAX".into()
    }

    fn description(&self) -> DynStaticStr {
        "https://xkcd.com/2497".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        let angle = dir.inverted_ud().angle_to_right();
        Gate2497::draw(angle, [WireState::False; 2], in_world, ctx);
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Gate2497::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Gate2497Preview))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Right),
            CircuitProperty::new("switch", "Switch", false),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Right);
        Gate2497::describe(dir).to_dyn()
    }
}

pub fn calc_size_from_inputs(inputs: u32) -> [u32; 2] {
    let rad = inputs >> 1;

    let height = rad * 2 + 1;
    let width = rad + 3;
    [width, height]
}
