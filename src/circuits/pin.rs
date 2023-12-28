use std::collections::hash_map::DefaultHasher;
use std::fmt::Write;
use std::hash::{Hash, Hasher};

use eframe::egui::{ComboBox, CursorIcon, Sense, Ui};
use emath::{vec2, Rect};

use crate::board::CircuitDesignPin;
use crate::circuits::props::CircuitProperty;
use crate::error::ResultReport;
use crate::state::{SafeWireState, VisitedItem};
use crate::{
    circuits::*, describe_directional_circuit, random_u128, unwrap_option_or_break,
    unwrap_option_or_return, ArcString,
};

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
    Bidirectional,
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

    pub fn is_bidirectional(self) -> bool {
        matches!(self, Self::Bidirectional)
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
struct PinState {
    state: SafeWireState,
}

impl InternalCircuitState for PinState {
    fn serialize(&self, _: bool) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
enum PinIdType {
    Pasted,
    Loaded,
    Random,
    BackCompat,
}

#[derive(Serialize, Deserialize)]
struct PartialPinDesignInfo {
    pos: Vec2u,
    display_dir: Option<Direction4>,
    display_name: DynStaticStr,
}

#[derive(Serialize, Deserialize)]
struct PinModel {
    #[serde(default)]
    id: Option<Arc<str>>,

    #[serde(default)]
    design: Option<PartialPinDesignInfo>,
}

pub struct Pin {
    pub dir: Direction4,
    pub ty: PinType,
    pub cpos: ControlPinPosition,

    id: Arc<str>,
    id_ty: PinIdType,
    pin: CircuitPinInfo,
    ctl: Option<CircuitPinInfo>,
    design: Option<PartialPinDesignInfo>,
}

impl Pin {
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
            id: "".into(),
            id_ty: PinIdType::Random,
            design: None,
        }
    }

    fn describe_props(props: &CircuitPropertyStore) -> CircuitDescription<2> {
        let dir = props.read_clone("dir").unwrap_or(Pin::DEFAULT_DIR);
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
            PinType::Bidirectional => InternalPinDirection::Custom,
        };

        describe_directional_circuit! {
            default_dir: Self::DEFAULT_DIR,
            dir: dir,
            size: [size.0, size.1],
            "pin": pin_dir, "Signal", Right, [pin_pos.0, pin_pos.1],
            "ctl": Inside, "Direction", ctl_dir, [ctl_pos.0, ctl_pos.1], active: ty.is_controlled()
        }
    }

    fn is_pico(&self, state_ctx: &CircuitStateContext) -> Option<bool> {
        // parent pin Inside -> child pin Outside
        match self.pin.get_direction(state_ctx) {
            PinDirection::Inside => Some(false),
            PinDirection::Outside => Some(true),
            PinDirection::Custom => None,
        }
    }

    pub fn get_designer_info(
        &self,
        props: &CircuitPropertyStore,
    ) -> crate::ui::designer::CircuitDesignPinInfo {
        let dir = match self.ty {
            PinType::Pico => InternalPinDirection::Inside,
            PinType::Cipo => InternalPinDirection::Outside,
            PinType::Controlled => InternalPinDirection::StateDependent {
                default: PinDirection::Inside,
            },
            PinType::Bidirectional => InternalPinDirection::Custom,
        };
        crate::ui::designer::CircuitDesignPinInfo {
            dir,
            display_dir: Some(self.dir.inverted()),
            display_name: props
                .read("name", |s: &ArcString| DynStaticStr::Dynamic(s.get_arc()))
                .unwrap_or(DynStaticStr::Static("")),
        }
    }

    fn gen_back_compat_id(pos: Vec2i) -> String {
        let hex_x = format!("{}{:x}", if pos.x < 0 { "-" } else { "" }, pos.x.abs());
        let hex_y = format!("{}{:x}", if pos.y < 0 { "-" } else { "" }, pos.y.abs());

        format!("{:x}:{:x}:{}{}", hex_x.len(), hex_y.len(), hex_x, hex_y)
    }

    fn gen_random_id() -> String {
        format!("{:x}", random_u128())
    }
}

impl CircuitImpl for Pin {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let outside_color = state_ctx
            .global_state
            .get_parent()
            .as_ref()
            .map(|p| {
                let outer = p.circuit.read_imp::<super::board::Board, _>(|board| {
                    board.resolve_inner_to_outer(state_ctx.circuit.id)
                })??;
                let outer_state = CircuitStateContext::new(p.state.clone(), p.circuit.clone());
                let state = p
                    .circuit
                    .info
                    .read()
                    .pins
                    .get(outer)?
                    .get_state(&outer_state);
                Some(state.color())
            })
            .map(|s| s.unwrap_or(WireState::None.color()));

        let wire_color = self
            .pin
            .get_wire_state(state_ctx)
            .map(|s| WireState::color(&s));

        let outside_color = outside_color
            .or_else(|| {
                if self.is_pico(state_ctx) != Some(false) {
                    Some(
                        state_ctx
                            .read_circuit_internal_state(|s: &PinState| s.state.0.color())
                            .unwrap_or(WireState::None.color()),
                    )
                } else {
                    None
                }
            })
            .or(wire_color)
            .unwrap_or_default();

        let color = wire_color.unwrap_or_else(|| self.pin.get_state(state_ctx).color());
        let cpos = self
            .ctl
            .as_ref()
            .map(|c| (self.cpos, c.get_state(state_ctx).color()));
        let is_pico = self.is_pico(state_ctx);
        let angle = self.dir.inverted_ud().angle_to_right();

        let center =
            crate::graphics::inside_pin(cpos, outside_color, color, is_pico, angle, paint_ctx);

        if state_ctx.global_state.get_parent().is_none() {
            let size = vec2(paint_ctx.screen.scale, paint_ctx.screen.scale);
            let rect = Rect::from_center_size(center, size);
            let interaction = paint_ctx.ui.interact(
                rect,
                paint_ctx.ui.auto_id_with(state_ctx.circuit.pos),
                Sense::click(),
            );

            if interaction.hovered() {
                paint_ctx.ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
            }

            if interaction.clicked() {
                let (shift, control) = paint_ctx
                    .ui
                    .input(|input| (input.modifiers.shift, input.modifiers.command));
                let new_state = state_ctx.write_circuit_internal_state(|s: &mut PinState| {
                    // None -> False -> True -> [control: Error] -> [shift ? None : False]
                    // shift: None -> False -> True -> None
                    // control: None | Error -> False -> True -> Error

                    s.state.0 = match (&s.state.0, shift, control) {
                        (WireState::True, false, false) => WireState::False,
                        (WireState::False, false, false) => WireState::True,

                        (WireState::None, _, _) => WireState::False,
                        (WireState::False, _, _) => WireState::True,

                        (WireState::True, _, true) => WireState::Error,
                        (WireState::Error, false, true) => WireState::False,
                        (WireState::Error, true, true) => WireState::None,

                        (WireState::True, true, false) => WireState::None,
                        (WireState::Error, true, false) => WireState::None,

                        (_, false, false) => WireState::False,
                        (_, false, true) => WireState::Error,
                        (_, true, _) => WireState::None,
                    };
                    s.state.0.clone()
                });

                if self.is_pico(state_ctx) != Some(false) {
                    self.pin.set_state(state_ctx, new_state);
                }
            }
        }
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);

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
                    _ => return,
                },
            };

            self.pin.set_direction(state_ctx, dir);

            let parent = state_ctx.global_state.get_parent();
            if let Some(parent) = parent {
                let outer = parent
                    .circuit
                    .read_imp::<super::board::Board, _>(|board| {
                        board.resolve_inner_to_outer(state_ctx.circuit.id)
                    })
                    .flatten();
                let outer = unwrap_option_or_return!(outer);
                let outer_state =
                    CircuitStateContext::new(parent.state.clone(), parent.circuit.clone());
                let info = parent.circuit.info.read();
                let outer_pin = info.pins.get(outer);
                let outer_pin = unwrap_option_or_return!(outer_pin);

                let dir_inv = match dir {
                    PinDirection::Inside => PinDirection::Outside,
                    PinDirection::Outside => PinDirection::Inside,
                    PinDirection::Custom => PinDirection::Custom,
                };

                outer_pin.set_direction(&outer_state, dir_inv);
            }
        }
        if let None | Some(0) = changed_pin {
            let parent = state_ctx.global_state.get_parent();
            match parent {
                Some(parent) => 'm: {
                    let outer = parent
                        .circuit
                        .read_imp::<super::board::Board, _>(|board| {
                            board.resolve_inner_to_outer(state_ctx.circuit.id)
                        })
                        .flatten();
                    let outer = unwrap_option_or_break!(outer, 'm);
                    let outer_state =
                        CircuitStateContext::new(parent.state.clone(), parent.circuit.clone());
                    let info = parent.circuit.info.read();
                    let outer_pin = info.pins.get(outer);
                    let outer_pin = unwrap_option_or_break!(outer_pin, 'm);

                    let this_dir = self.pin.get_direction(state_ctx);
                    let outer_dir = outer_pin.get_direction(&outer_state);

                    match (this_dir, outer_dir) {
                        (PinDirection::Inside, PinDirection::Outside) => {
                            outer_pin.set_state(&outer_state, self.pin.get_state(state_ctx))
                        }
                        (PinDirection::Outside, PinDirection::Inside) => self
                            .pin
                            .set_state(state_ctx, outer_pin.get_state(&outer_state)),
                        (PinDirection::Outside, PinDirection::Outside) => {
                            self.pin.set_state(state_ctx, WireState::None)
                        }
                        _ => {}
                    }
                }
                None => {
                    if self.is_pico(state_ctx) != Some(false) {
                        self.pin.set_state(
                            state_ctx,
                            state_ctx
                                .clone_circuit_internal_state::<PinState>()
                                .unwrap_or_default()
                                .state
                                .0,
                        )
                    }
                }
            }
        }
    }

    fn custom_pin_mutate_state(
        &self,
        ctx: &CircuitStateContext,
        pin: usize,
        state: &mut WireState,
        visited_items: &mut VisitedList,
    ) {
        if pin != 0 {
            return;
        }

        let parent = ctx.global_state.get_parent();
        let parent = match parent {
            Some(p) => p,
            None => {
                ctx.read_circuit_internal_state(|s: &PinState| {
                    state.merge(&s.state.0);
                });
                return;
            }
        };

        let outer = parent
            .circuit
            .read_imp::<super::board::Board, _>(|board| {
                board.resolve_inner_to_outer(ctx.circuit.id)
            })
            .flatten();
        let outer = unwrap_option_or_return!(outer);
        let info = parent.circuit.info.read();
        let outer_pin_info = info.pins.get(outer);
        let outer_pin_info = unwrap_option_or_return!(outer_pin_info);
        let outer_pin = outer_pin_info.pin.read();

        if let Some(wire) = outer_pin.connected_wire() {
            visited_items.push(parent.state.board.uid, VisitedItem::Pin(outer_pin.id));
            parent.state.compute_wire_state(wire, state, visited_items);
            visited_items.pop(parent.state.board.uid);
        }
    }

    fn custom_pin_apply_state(
        &self,
        ctx: &CircuitStateContext,
        pin: usize,
        state: &WireState,
        visited_items: &mut VisitedList,
    ) {
        if pin != 0 {
            return;
        }

        let parent = ctx.global_state.get_parent();
        let parent = unwrap_option_or_return!(parent);

        let outer = parent
            .circuit
            .read_imp::<super::board::Board, _>(|board| {
                board.resolve_inner_to_outer(ctx.circuit.id)
            })
            .flatten();
        let outer = unwrap_option_or_return!(outer);
        let info = parent.circuit.info.read();
        let outer_pin_info = info.pins.get(outer);
        let outer_pin_info = unwrap_option_or_return!(outer_pin_info);
        let outer_pin = outer_pin_info.pin.read();

        outer_pin.set_input(&parent.state, state, false, None);
        if let Some(wire) = outer_pin.connected_wire() {
            visited_items.push(parent.state.board.uid, VisitedItem::Pin(outer_pin.id));
            parent
                .state
                .apply_wire_state(wire, state, false, visited_items);
            visited_items.pop(parent.state.board.uid);
        }
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" || prop_id == "ty" || prop_id == "cpos" {
            *resize = true;
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        self.dir = circ.props.read_clone("dir").unwrap_or(Self::DEFAULT_DIR);
        self.ty = circ.props.read_clone("ty").unwrap_or(PinType::Pico);
        self.cpos = circ
            .props
            .read_clone("cpos")
            .unwrap_or(ControlPinPosition::Left);

        let outer_pd = match self.ty {
            PinType::Pico => InternalPinDirection::Inside,
            PinType::Cipo => InternalPinDirection::Outside,
            PinType::Controlled => InternalPinDirection::StateDependent {
                default: PinDirection::Inside,
            },
            PinType::Bidirectional => InternalPinDirection::Custom,
        };

        // Update pin direction in the board design if possible
        let my_id = circ.board.pins.read().get_by_right(&circ.id).cloned();
        let my_id = unwrap_option_or_return!(my_id);

        let mut designs = circ.board.designs.write();
        let current = designs.current();
        let current_this = current.pins.iter().find(|p| p.id.deref() == my_id.deref());
        let current_this = unwrap_option_or_return!(current_this);

        if current_this.dir != outer_pd {
            let current = designs.current_mut();
            let current_this = current
                .pins
                .iter_mut()
                .find(|p| p.id.deref() == my_id.deref());
            let current_this = unwrap_option_or_return!(current_this);
            current_this.dir = outer_pd;
        }
    }

    fn load_internal(
        &self,
        _ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        _paste: bool,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::de::intermediate::deserialize::<PinState>(data)
            .report_error(errors)
            .map(|s| Box::new(s) as Box<dyn InternalCircuitState>)
    }

    fn circuit_init(&mut self, circ: &Arc<Circuit>, first_init: bool) {
        let mut board_pins = circ.board.pins.write();
        match self.id_ty {
            PinIdType::Pasted => {
                if board_pins.contains_left(&self.id) {
                    self.id = Self::gen_random_id().into();
                }
            }
            PinIdType::Loaded => {}
            PinIdType::Random => {
                self.id = Self::gen_random_id().into();
            }
            PinIdType::BackCompat => {
                self.id = Self::gen_back_compat_id(circ.pos).into();
            }
        }
        board_pins.insert(self.id.clone(), circ.id);
        drop(board_pins);

        if let Some(design_data) = self.design.take() {
            let mut designs = circ.board.designs.write();
            let design = designs.current_mut();
            if design.pins.iter().all(|p| p.id.deref() != self.id.deref()) {
                let dir = match self.ty {
                    PinType::Pico => InternalPinDirection::Inside,
                    PinType::Cipo => InternalPinDirection::Outside,
                    PinType::Controlled => InternalPinDirection::StateDependent {
                        default: PinDirection::Inside,
                    },
                    PinType::Bidirectional => InternalPinDirection::Custom,
                };
                design.pins.push(CircuitDesignPin {
                    id: self.id.clone().into(),
                    pos: design_data.pos,
                    dir,
                    display_dir: design_data.display_dir,
                    display_name: design_data.display_name,
                })
            }
        }

        if first_init {
            let name_empty = circ.props.read("name", |s: &ArcString| s.is_empty());
            if let Some(true) = name_empty {
                circ.props.write("name", |s: &mut ArcString| {
                    let string = s.get_mut();
                    string.clear();
                    string.push_str("Pin ");

                    let mut hasher = DefaultHasher::default();
                    self.id.hash(&mut hasher);
                    let hash = (hasher.finish() & 0xffffffff) as u32;
                    let _ = string.write_fmt(format_args!("{hash:x}"));
                });
            }
        }
    }

    fn circuit_remove(&mut self, circ: &Arc<Circuit>) {
        let kv = circ.board.pins.write().remove_by_right(&circ.id);
        let id = unwrap_option_or_return!(kv).0;

        let mut designs = circ.board.designs.write();
        let design = designs.current_mut();
        let index = design
            .pins
            .iter()
            .enumerate()
            .find_map(|(i, p)| (p.id.deref() == id.deref()).then_some(i));

        if let Some(index) = index {
            design.pins.remove(index);
        }
    }

    fn save(&self, circ: &Arc<Circuit>, copy: bool) -> serde_intermediate::Intermediate {
        let design_data = copy
            .then(|| {
                let designs = circ.board.designs.read();
                let design = designs.current();
                let pin_data = design.pins.iter().find(|p| p.id.deref() == self.id.deref());
                let pin_data = unwrap_option_or_return!(pin_data, None);
                Some(PartialPinDesignInfo {
                    pos: pin_data.pos,
                    display_dir: pin_data.display_dir,
                    display_name: pin_data.display_name.clone(),
                })
            })
            .flatten();
        let m = PinModel {
            id: Some(self.id.clone()),
            design: design_data,
        };
        serde_intermediate::to_intermediate(&m).unwrap_or_default()
    }

    fn load(
        &mut self,
        _circ: &Arc<Circuit>,
        data: &serde_intermediate::Intermediate,
        paste: bool,
        _errors: &mut ErrorList,
    ) {
        // Old data, use back-compat id
        if matches!(data, serde_intermediate::Intermediate::Unit) {
            self.id_ty = PinIdType::BackCompat;
        } else if let Ok(m) = serde_intermediate::from_intermediate::<PinModel>(data) {
            if let Some(id) = m.id {
                if paste {
                    self.id_ty = PinIdType::Pasted;
                } else {
                    self.id_ty = PinIdType::Loaded;
                }
                self.id = id;
            }
            self.design = paste.then_some(m.design).flatten();
        }
    }
}

pub const TYPEID: &str = "pin";

pub struct Preview {}

impl CircuitPreviewImpl for Preview {
    fn type_name(&self) -> DynStaticStr {
        TYPEID.into()
    }

    fn display_name(&self) -> DynStaticStr {
        "Circuit pin".into()
    }

    fn description(&self) -> DynStaticStr {
        "A component to communicate with outside board.\n\
         Represents a pin in a placeable circuit form of a circuit board.\n\
         Passes signals to or from another circuit board when current board is placed in a circuit form.\n\
         \n\
         Can be clicked on to change state if not connected to an outside pin.\n\
         Can carry signals outside, inside, be controlled by a separate Control input or be fully bidirectional.\n\
         Please note that Bidirectional mode may be less stable and performant.\
        ".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _: bool) {
        let dir = props.read_clone("dir").unwrap_or(Pin::DEFAULT_DIR);
        let ty = props.read_clone("ty").unwrap_or(PinType::Pico);
        let cpos = props.read_clone("cpos").unwrap_or(ControlPinPosition::Left);

        let cpos = ty.is_controlled().then_some((cpos, WireState::False.color()));
        let angle = dir.inverted_ud().angle_to_right();

        crate::graphics::inside_pin(
            cpos,
            WireState::False.color(),
            WireState::False.color(),
            ty.is_bidirectional().not().then_some(!ty.is_cipo()),
            angle,
            ctx,
        );
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Pin::new())
    }

    fn load_copy_data(
        &self,
        _imp: &serde_intermediate::Intermediate,
        _internal: &serde_intermediate::Intermediate,
        _ctx: &Arc<SimulationContext>,
        _errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(Preview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Pin::DEFAULT_DIR),
            CircuitProperty::new("ty", "Pin type", PinType::Pico),
            CircuitProperty::new("cpos", "Control pos", ControlPinPosition::Left),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Pin::describe_props(props).to_dyn()
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
                    PinType::Bidirectional => "Bidirectional",
                }
            })
            .show_ui(ui, |ui| {
                for p in [
                    PinType::Pico,
                    PinType::Cipo,
                    PinType::Controlled,
                    PinType::Bidirectional,
                ] {
                    let name = match p {
                        PinType::Pico => "Inside",
                        PinType::Cipo => "Outside",
                        PinType::Controlled => "Controlled",
                        PinType::Bidirectional => "Bidirectional",
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
            'b' => Self::Bidirectional,
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
            Self::Pico => 'i'.serialize(serializer),
            Self::Cipo => 'o'.serialize(serializer),
            Self::Controlled => 'c'.serialize(serializer),
            Self::Bidirectional => 'b'.serialize(serializer),
        }
    }
}
