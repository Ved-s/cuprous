use std::f32::consts::TAU;

use eframe::egui::{ComboBox, Ui};
use eframe::epaint::{PathShape, Stroke};
use emath::{pos2, vec2, Pos2};

use crate::circuits::props::CircuitProperty;
use crate::vector::Vec2f;
use crate::{circuits::*, describe_directional_circuit};

use super::props::CircuitPropertyImpl;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ControlPinPosition {
    Left,
    Behind,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PinType {
    Inside,
    Outside,
    Controlled,
}

impl PinType {
    #[allow(unused)]
    pub fn is_inside(self) -> bool {
        matches!(self, Self::Inside)
    }

    pub fn is_outside(self) -> bool {
        matches!(self, Self::Outside)
    }

    pub fn is_controlled(self) -> bool {
        matches!(self, Self::Controlled)
    }
}

struct Circuit {
    dir: Direction4,
    ty: PinType,
    cpos: ControlPinPosition,

    pin: CircuitPinInfo,
    ctl: Option<CircuitPinInfo>,
}

impl Circuit {
    const DEFAULT_DIR: Direction4 = Direction4::Right;

    fn new() -> Self {
        let description =
            Self::describe(Self::DEFAULT_DIR, PinType::Inside, ControlPinPosition::Left);

        Self {
            dir: Self::DEFAULT_DIR,
            ty: PinType::Inside,
            cpos: ControlPinPosition::Left,

            pin: description.pins[0].to_info(),
            ctl: description.pins[1].to_active_info(),
        }
    }

    fn draw(
        ctl: Option<(ControlPinPosition, WireState)>,
        outside_state: WireState,
        state: WireState,
        is_inside: bool,
        angle: f32,
        ctx: &PaintContext,
    ) {
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
            [transformer(pos2(0.95, 0.5) + center_off), transformer(pos2(1.5, 0.5) + center_off)],
            Stroke::new(ActiveCircuitBoard::WIRE_THICKNESS * scale, state.color()),
        );

        let points = if is_inside {
            [pos2(1.12, 0.75), pos2(1.42, 0.5), pos2(1.12, 0.25)]
        } else {
            [pos2(1.3, 0.75), pos2(1.0, 0.5), pos2(1.3, 0.25)]
        };
        let points = points.into_iter().map(|p| transformer(p + center_off)).collect();
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
                [ctl_transformer(pos2(0.5, 1.5)), ctl_transformer(pos2(0.5, 1.24))],
                Stroke::new(ActiveCircuitBoard::WIRE_THICKNESS * scale, ctl_state.color()),
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
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Inside);
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
            PinType::Inside => InternalPinDirection::Inside,
            PinType::Outside => InternalPinDirection::Outside,
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
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {

        // TODO: fetch outside pin state
        let outside_state = WireState::None;
        let state = self.pin.get_wire_state(state_ctx).unwrap_or(outside_state);
        let cpos = self
            .ctl
            .as_ref()
            .map(|c| (self.cpos, c.get_state(state_ctx)));
        let is_inside = matches!(self.pin.get_direction(state_ctx), PinDirection::Inside);
        let angle = self.dir.inverted_ud().angle_to_right();

        Circuit::draw(cpos, outside_state, state, is_inside, angle, paint_ctx);
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
        if let (None | Some(1), Some(ctl)) = (changed_pin, &self.ctl) {
            let is_outside = matches!(ctl.get_state(state_ctx), WireState::True);
            let dir = match is_outside {
                true => PinDirection::Outside,
                false => PinDirection::Inside,
            };
            self.pin.set_direction(state_ctx, dir);
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
        self.ty = props.read_clone("ty").unwrap_or(PinType::Inside);
        self.cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);
    }
}

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        "pin".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _: bool) {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Inside);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        let cpos = ty.is_controlled().then_some((cpos, WireState::False));
        let angle = dir.inverted_ud().angle_to_right();

        Circuit::draw(
            cpos,
            WireState::False,
            WireState::False,
            !ty.is_outside(),
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
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Circuit::DEFAULT_DIR),
            CircuitProperty::new("ty", "Pin type", PinType::Inside),
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
                    PinType::Inside => "Inside",
                    PinType::Outside => "Outside",
                    PinType::Controlled => "Controlled",
                }
            })
            .show_ui(ui, |ui| {
                for p in [PinType::Inside, PinType::Outside, PinType::Controlled] {
                    let name = match p {
                        PinType::Inside => "Inside",
                        PinType::Outside => "Outside",
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
            'o' => Self::Outside,
            'c' => Self::Controlled,
            _ => Self::Inside,
        })
    }
}

impl Serialize for PinType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match *self {
            PinType::Inside => 'i'.serialize(serializer),
            PinType::Outside => 'o'.serialize(serializer),
            PinType::Controlled => 'c'.serialize(serializer),
        }
    }
}
