use eframe::egui::{ComboBox, Ui};

use crate::circuits::props::CircuitProperty;
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
    ctl: Option<CircuitPinInfo>
}

impl Circuit {
    const DEFAULT_DIR: Direction4 = Direction4::Right;

    fn new() -> Self {

        let description = Self::describe(Self::DEFAULT_DIR, PinType::Inside, ControlPinPosition::Left);

        Self {
            dir: Self::DEFAULT_DIR,
            ty: PinType::Inside,
            cpos: ControlPinPosition::Left,

            pin: description.pins[0].to_info(),
            ctl: description.pins[1].to_active_info()
        }
    }

    fn draw(
        dir: Direction4,
        ctl: Option<(ControlPinPosition, WireState)>,
        outside_state: WireState,
        state: WireState,
        is_input: bool,
        ctx: &PaintContext,
        semi_transparent: bool,
    ) {
        todo!()
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Inside);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        Self::describe(dir, ty, cpos)
    }

    // cpos:
    //   + - - +  + - - +  + - - +  + - - - +
    //   |   0 |  | 1   |  |   0 |  | 1   0 |
    //   * - - *  |   0 |  | 1   |  * - - - *
    //            * - - *  * - - *
    //    None     Left     Right    Behind
    fn describe(dir: Direction4, ty: PinType, cpos: ControlPinPosition) -> CircuitDescription<2> {
        let cpos = ty.is_controlled().then_some(cpos);

        let (size, pin_pos, ctl_pos) = match cpos {
            None => ((2, 1), (1, 0), (0, 0)),
            Some(ControlPinPosition::Left) => ((2, 2), (1, 1), (0, 0)),
            Some(ControlPinPosition::Right) => ((2, 2), (1, 0), (0, 1)),
            Some(ControlPinPosition::Behind) => ((3, 1), (2, 0), (0, 0)),
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
            "pin": pin_dir, "Signal", Left, [pin_pos.0, pin_pos.1],
            "ctl": Inside, "Direction", Down, [ctl_pos.0, ctl_pos.1], active: ty.is_controlled()
        }
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {

        let outside_state = self.pin.get_state(state_ctx);
        let state = self.pin.get_wire_state(state_ctx).unwrap_or(outside_state);
        let cpos = self.ctl.as_ref().map(|c| (self.cpos, c.get_state(state_ctx)));
        let is_input = matches!(self.pin.get_direction(state_ctx), PinDirection::Inside);

        Circuit::draw(
            self.dir,
            cpos,
            outside_state,
            state,
            is_input,
            paint_ctx,
            false,
        );
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
        todo!()
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

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Circuit::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Inside);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        let cpos = ty.is_controlled().then_some((cpos, WireState::False));

        Circuit::draw(dir, cpos, WireState::False, WireState::False, !ty.is_outside(), ctx, in_world);
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
            ControlPinPosition::Behind => 'r'.serialize(serializer),
            ControlPinPosition::Right => 'b'.serialize(serializer),
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
