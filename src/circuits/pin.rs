use std::f32::consts::TAU;

use eframe::egui::{ComboBox, Sense, Ui};
use eframe::epaint::{PathShape, Stroke};
use emath::{pos2, vec2, Pos2, Rect};

use crate::circuits::props::CircuitProperty;
use crate::state::SafeWireState;
use crate::vector::Vec2f;
use crate::{circuits::*, describe_directional_circuit, unwrap_option_or_break, ArcString};

use super::props::CircuitPropertyImpl;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlPinPosition {
    Left,
    Behind,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PinType {
    Pico, // Parent Input, Child Output
    Cipo, // Child Input, Parent Output
    Controlled,
}

impl PinType {
    #[allow(unused)]
    pub fn is_pico(self) -> bool {
        matches!(self, Self::Pico)
    }

    pub fn is_cipo(self) -> bool {
        matches!(self, Self::Cipo)
    }

    pub fn is_controlled(self) -> bool {
        matches!(self, Self::Controlled)
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, Copy)]
struct State {
    state: SafeWireState,
}

impl InternalCircuitState for State {
    fn serialize(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap()
    }
}

pub struct Circuit {
    pub dir: Direction4,
    pub ty: PinType,
    pub cpos: ControlPinPosition,

    pin: CircuitPinInfo,
    ctl: Option<CircuitPinInfo>,
}

impl Circuit {
    pub const DEFAULT_DIR: Direction4 = Direction4::Right;

    fn new() -> Self {
        let description =
            Self::describe(Self::DEFAULT_DIR, PinType::Pico, ControlPinPosition::Left);

        Self {
            dir: Self::DEFAULT_DIR,
            ty: PinType::Pico,
            cpos: ControlPinPosition::Left,

            pin: description.pins[0].to_info(),
            ctl: description.pins[1].to_active_info(),
        }
    }

    fn draw(
        ctl: Option<(ControlPinPosition, WireState)>,
        outside_state: WireState,
        state: WireState,
        is_pico: bool,
        angle: f32,
        ctx: &PaintContext,
    ) -> Pos2 {
        let (size, center) = match ctl {
            None => ((2, 1), (0, 0)),
            Some((ControlPinPosition::Left, _)) => ((2, 2), (0, 1)),
            Some((ControlPinPosition::Right, _)) => ((2, 2), (0, 0)),
            Some((ControlPinPosition::Behind, _)) => ((3, 1), (1, 0)),
        };
        let size = vec2(size.0 as f32, size.1 as f32);
        let transformer = |p: Pos2| {
            ctx.rect.lerp_inside(
                Vec2f::from(p.to_vec2() / size)
                    .rotated_xy(angle, 0.5)
                    .into(),
            )
        };

        let scale = ctx.screen.scale;
        let center_off = vec2(center.0 as f32, center.1 as f32);
        let center = transformer(pos2(center.0 as f32 + 0.5, center.1 as f32 + 0.5));

        ctx.paint.line_segment(
            [
                transformer(pos2(0.95, 0.5) + center_off),
                transformer(pos2(1.5, 0.5) + center_off),
            ],
            Stroke::new(ActiveCircuitBoard::WIRE_THICKNESS * scale, state.color()),
        );

        let points = if is_pico {
            [pos2(1.12, 0.75), pos2(1.42, 0.5), pos2(1.12, 0.25)] // Right
        } else {
            [pos2(1.3, 0.75), pos2(1.0, 0.5), pos2(1.3, 0.25)] // Left
        };
        let points = points
            .into_iter()
            .map(|p| transformer(p + center_off))
            .collect();
        ctx.paint.add(PathShape {
            points,
            closed: true,
            fill: state.color(),
            stroke: Stroke::NONE,
        });
        ctx.paint
            .circle_filled(center, 0.3 * scale, outside_state.color());
        ctx.paint.circle_stroke(
            center,
            0.45 * scale,
            Stroke::new(0.1 * scale, state.color()),
        );

        if let Some((ctl_dir, ctl_state)) = ctl {
            let ctl_angle = match ctl_dir {
                ControlPinPosition::Left => TAU * 0.5,
                ControlPinPosition::Behind => TAU * 0.25,
                ControlPinPosition::Right => 0.0,
            };

            let ctl_transformer = |p: Pos2| {
                transformer(Pos2::from(Vec2f::from(p).rotated_xy(ctl_angle, 0.5)) + center_off)
            };

            ctx.paint.line_segment(
                [
                    ctl_transformer(pos2(0.5, 1.5)),
                    ctl_transformer(pos2(0.5, 1.24)),
                ],
                Stroke::new(
                    ActiveCircuitBoard::WIRE_THICKNESS * scale,
                    ctl_state.color(),
                ),
            );

            ctx.paint.add(PathShape {
                points: vec![
                    ctl_transformer(pos2(0.25, 1.25)),
                    ctl_transformer(pos2(0.5, 1.0)),
                    ctl_transformer(pos2(0.75, 1.25)),
                ],
                closed: true,
                fill: ctl_state.color(),
                stroke: Stroke::NONE,
            });
        }

        center
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Pico);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        Self::describe(dir, ty, cpos)
    }

    // cpos:
    //   + - - +  + - - +  + - - +  + - - - +
    //   | C 0 |  | 1   |  | C 0 |  | 1 C 0 |
    //   * - - *  | C 0 |  | 1   |  * - - - *
    //            * - - *  * - - *
    //    None     Left     Right    Behind
    fn describe(dir: Direction4, ty: PinType, cpos: ControlPinPosition) -> CircuitDescription<2> {
        let cpos = ty.is_controlled().then_some(cpos);

        let (size, pin_pos, ctl_pos, ctl_dir) = match cpos {
            None => ((2, 1), (1, 0), (0, 0), Direction4::Down),
            Some(ControlPinPosition::Left) => ((2, 2), (1, 1), (0, 0), Direction4::Up),
            Some(ControlPinPosition::Right) => ((2, 2), (1, 0), (0, 1), Direction4::Down),
            Some(ControlPinPosition::Behind) => ((3, 1), (2, 0), (0, 0), Direction4::Left),
        };

        let pin_dir = match ty {
            PinType::Pico => InternalPinDirection::Outside,
            PinType::Cipo => InternalPinDirection::Inside,
            PinType::Controlled => InternalPinDirection::StateDependent {
                default: PinDirection::Inside,
            },
        };

        describe_directional_circuit! {
            default_dir: Self::DEFAULT_DIR,
            dir: dir,
            size: [size.0, size.1],
            "pin": pin_dir, "Signal", Right, [pin_pos.0, pin_pos.1],
            "ctl": Inside, "Direction", ctl_dir, [ctl_pos.0, ctl_pos.1], active: ty.is_controlled()
        }
    }

    fn is_pico(&self, state_ctx: &CircuitStateContext) -> bool {
        // parent pin Inside -> child pin Outside
        matches!(self.pin.get_direction(state_ctx), PinDirection::Outside)
    }

    pub fn get_designer_info(&self, props: &CircuitPropertyStore) -> crate::ui::designer::CircuitDesignPinInfo {
        let dir = match self.ty {
            PinType::Pico => InternalPinDirection::Inside,
            PinType::Cipo => InternalPinDirection::Outside,
            PinType::Controlled => InternalPinDirection::StateDependent { default: PinDirection::Inside },
        };
        crate::ui::designer::CircuitDesignPinInfo {
            dir,
            display_dir: Some(self.dir.inverted()),
            display_name: props.read("name", |s: &ArcString| DynStaticStr::Dynamic(s.get_arc())).unwrap_or(DynStaticStr::Static("")),
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let outside_state = state_ctx
            .global_state
            .parent
            .read()
            .as_ref()
            .map(|p| {
                let board = p.state.board.read();
                let circuit = board.circuits.get(p.circuit)?;
                let outer = circuit.read_imp::<super::board::Circuit, _>(|board| {
                    board.resolve_inner_to_outer(state_ctx.circuit.id)
                })??;
                let outer_state = CircuitStateContext::new(p.state.clone(), circuit.clone());
                let state = circuit.info.read().pins.get(outer)?.get_state(&outer_state);
                Some(state)
            })
            .map(|s| s.unwrap_or(WireState::None));

        let outside_state = outside_state.unwrap_or_else(|| {
            state_ctx
                .clone_circuit_internal_state::<State>()
                .unwrap_or_default()
                .state
                .0
        });
        let state = self
            .pin
            .get_wire_state(state_ctx)
            .unwrap_or_else(|| self.pin.get_state(state_ctx));
        let cpos = self
            .ctl
            .as_ref()
            .map(|c| (self.cpos, c.get_state(state_ctx)));
        let is_pico = self.is_pico(state_ctx);
        let angle = self.dir.inverted_ud().angle_to_right();

        let center = Circuit::draw(cpos, outside_state, state, is_pico, angle, paint_ctx);

        // TODO: don't interact if this state has parent state

        let size = vec2(paint_ctx.screen.scale, paint_ctx.screen.scale);
        let rect = Rect::from_center_size(center, size);
        let interaction = paint_ctx.ui.interact(
            rect,
            paint_ctx.ui.auto_id_with(state_ctx.circuit.pos),
            Sense::click(),
        );

        if interaction.clicked() {
            let (shift, control) = paint_ctx
                .ui
                .input(|input| (input.modifiers.shift, input.modifiers.command));
            let new_state = state_ctx.write_circuit_internal_state(|s: &mut State| {
                // None -> False -> True -> [control: Error] -> [shift ? None : False]
                // shift: None -> False -> True -> None
                // control: None | Error -> False -> True -> Error

                s.state.0 = match (s.state.0, shift, control) {
                    (WireState::True, false, false) => WireState::False,
                    (WireState::False, false, false) => WireState::True,
                    (_, false, false) => WireState::False,

                    (WireState::None, _, _) => WireState::False,
                    (WireState::False, _, _) => WireState::True,

                    (WireState::True, _, true) => WireState::Error,
                    (WireState::Error, false, true) => WireState::False,
                    (WireState::Error, true, true) => WireState::None,

                    (WireState::True, true, false) => WireState::None,
                    (WireState::Error, true, false) => WireState::None,
                };
                s.state.0
            });

            if self.is_pico(state_ctx) {
                self.pin.set_state(state_ctx, new_state);
            }
        }
    }

    fn create_pins(&mut self, props: &CircuitPropertyStore) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(props);

        self.pin = description.pins[0].to_info();
        self.ctl = description.pins[1].to_active_info();

        let mut vec = vec![self.pin.clone()];
        if let Some(ctl) = self.ctl.clone() {
            vec.push(ctl);
        }
        vec.into_boxed_slice()
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        if let None | Some(1) = changed_pin {
            let dir = match &self.ctl {
                Some(ctl) => match ctl.get_state(state_ctx) {
                    WireState::True => PinDirection::Inside,
                    _ => PinDirection::Outside,
                },
                None => match self.ty {
                    PinType::Pico => PinDirection::Outside,
                    PinType::Cipo => PinDirection::Inside,
                    PinType::Controlled => PinDirection::Inside, // WARN: this should be invalid state
                },
            };

            self.pin.set_direction(state_ctx, dir);
        }
        if let None | Some(0) = changed_pin {

            let parent = state_ctx.global_state.parent.read();
            match parent.as_ref() {
                Some(parent) => 'm: {
                    let board = parent.state.board.read();
                    let circuit = board.circuits.get(parent.circuit);
                    let circuit = unwrap_option_or_break!(circuit, 'm);
                    let outer = circuit.read_imp::<super::board::Circuit, _>(|board| {
                        board.resolve_inner_to_outer(state_ctx.circuit.id)
                    }).flatten();
                    let outer = unwrap_option_or_break!(outer, 'm);
                    let outer_state = CircuitStateContext::new(parent.state.clone(), circuit.clone());
                    let info = circuit.info.read();
                    let outer_pin = info.pins.get(outer);
                    let outer_pin = unwrap_option_or_break!(outer_pin, 'm);
                    
                    let this_dir = self.pin.get_direction(state_ctx);
                    let outer_dir = outer_pin.get_direction(&outer_state);

                    match (this_dir, outer_dir) {
                        (PinDirection::Inside, PinDirection::Outside) => outer_pin.set_state(&outer_state, self.pin.get_state(state_ctx)),
                        (PinDirection::Outside, PinDirection::Inside) => self.pin.set_state(state_ctx, outer_pin.get_state(&outer_state)),
                        (PinDirection::Outside, PinDirection::Outside) =>self.pin.set_state(state_ctx, WireState::None),
                        _ => {}
                    }
                },
                None => {
                    if self.is_pico(state_ctx) {
                        self.pin.set_state(
                            state_ctx,
                            state_ctx
                                .clone_circuit_internal_state::<State>()
                                .unwrap_or_default()
                                .state
                                .0,
                        )
                    }
                },
            }
        }
    }

    fn size(&self, props: &CircuitPropertyStore) -> Vec2u {
        Self::describe_props(props).size
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" || prop_id == "ty" || prop_id == "cpos" {
            *resize = true;
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, props: &CircuitPropertyStore, _: Option<&str>) {
        self.dir = props.read_clone("dir").unwrap_or(Self::DEFAULT_DIR);
        self.ty = props.read_clone("ty").unwrap_or(PinType::Pico);
        self.cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);
    }

    fn load_internal(
        &self,
        data: &serde_intermediate::Intermediate,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::de::intermediate::deserialize::<State>(data)
            .ok()
            .map(|s| Box::new(s) as Box<dyn InternalCircuitState>)
    }

    fn postload(&mut self, state: &CircuitStateContext, _: bool) {
        let pos = state.circuit.pos;
        let hex_x = format!("{:x}", pos.x());
        let hex_y = format!("{:x}", pos.x());
        let uid = format!("{:x}:{:x}:{}{}", hex_x.len(), hex_y.len(), hex_x, hex_y);

        let board = state.global_state.board.read();
        board.pins.write().insert(uid.into(), state.circuit.id);
    }
}

pub const TYPEID: &str = "pin";

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        TYPEID.into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _: bool) {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Pico);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        let cpos = ty.is_controlled().then_some((cpos, WireState::False));
        let angle = dir.inverted_ud().angle_to_right();

        Circuit::draw(
            cpos,
            WireState::False,
            WireState::False,
            !ty.is_cipo(),
            angle,
            ctx,
        );
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn load_impl_data(
        &self,
        _: &serde_intermediate::Intermediate,
        _: &Arc<SimulationContext>,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Circuit::DEFAULT_DIR),
            CircuitProperty::new("ty", "Pin type", PinType::Pico),
            CircuitProperty::new("cpos", "Control pos", ControlPinPosition::Left),
        ])
    }

    fn display_name(&self) -> DynStaticStr {
        "Circuit pin".into()
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Circuit::describe_props(props).to_dyn()
    }
}

impl CircuitPropertyImpl for ControlPinPosition {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let mut changed = false;
        ComboBox::from_id_source("cpp_ui")
            .selected_text(if not_equal {
                Default::default()
            } else {
                match *self {
                    ControlPinPosition::Left => "Left",
                    ControlPinPosition::Right => "Right",
                    ControlPinPosition::Behind => "Behind",
                }
            })
            .show_ui(ui, |ui| {
                for p in [
                    ControlPinPosition::Left,
                    ControlPinPosition::Right,
                    ControlPinPosition::Behind,
                ] {
                    let name = match p {
                        ControlPinPosition::Left => "Left",
                        ControlPinPosition::Right => "Right",
                        ControlPinPosition::Behind => "Behind",
                    };
                    let res = ui.selectable_value(self, p, name);
                    if res.changed() || res.clicked() {
                        changed = true;
                    }
                }
            });
        changed.then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            *self = d;
        }
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut() {
            *r = *self;
        }
    }
}

impl<'de> Deserialize<'de> for ControlPinPosition {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(match char::deserialize(deserializer)? {
            'r' => Self::Right,
            'b' => Self::Behind,
            _ => Self::Left,
        })
    }
}

impl Serialize for ControlPinPosition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            ControlPinPosition::Left => 'l'.serialize(serializer),
            ControlPinPosition::Behind => 'b'.serialize(serializer),
            ControlPinPosition::Right => 'r'.serialize(serializer),
        }
    }
}

impl CircuitPropertyImpl for PinType {
    fn equals(&self, other: &dyn CircuitPropertyImpl) -> bool {
        other.is_type_and(|o: &Self| o == self)
    }

    fn ui(&mut self, ui: &mut Ui, not_equal: bool) -> Option<Box<dyn CircuitPropertyImpl>> {
        let old = *self;
        let mut changed = false;
        ComboBox::from_id_source("pt_ui")
            .selected_text(if not_equal {
                Default::default()
            } else {
                match *self {
                    PinType::Pico => "Inside",
                    PinType::Cipo => "Outside",
                    PinType::Controlled => "Controlled",
                }
            })
            .show_ui(ui, |ui| {
                for p in [PinType::Pico, PinType::Cipo, PinType::Controlled] {
                    let name = match p {
                        PinType::Pico => "Inside",
                        PinType::Cipo => "Outside",
                        PinType::Controlled => "Controlled",
                    };
                    let res = ui.selectable_value(self, p, name);
                    if res.changed() || res.clicked() {
                        changed = true;
                    }
                }
            });
        changed.then(|| Box::new(old) as Box<dyn CircuitPropertyImpl>)
    }

    fn clone(&self) -> Box<dyn CircuitPropertyImpl> {
        Box::new(Clone::clone(self))
    }

    fn save(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(&self).unwrap_or_default()
    }

    fn load(&mut self, data: &serde_intermediate::Intermediate) {
        if let Ok(d) = serde_intermediate::de::intermediate::deserialize(data) {
            *self = d;
        }
    }

    fn copy_into(&self, other: &mut dyn CircuitPropertyImpl) {
        if let Some(r) = other.downcast_mut() {
            *r = *self;
        }
    }
}

impl<'de> Deserialize<'de> for PinType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        Ok(match char::deserialize(deserializer)? {
            'o' => Self::Cipo,
            'c' => Self::Controlled,
            _ => Self::Pico,
        })
    }
}

impl Serialize for PinType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            PinType::Pico => 'i'.serialize(serializer),
            PinType::Cipo => 'o'.serialize(serializer),
            PinType::Controlled => 'c'.serialize(serializer),
        }
    }
}
