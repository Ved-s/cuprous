#![allow(warnings)]

use std::sync::Weak;
use std::thread::LocalKey;

use eframe::egui::{Rounding, Stroke};
use emath::vec2;

use crate::circuits::props::CircuitProperty;
use crate::ext::IteratorExt;
use crate::graphics::{RelayDoubleThrowColors, RelaySingleThrowColors};
use crate::vector::Vec2f;
use crate::{circuits::*, describe_directional_circuit, Mutex};

use self::props::{CircuitPropertyImpl, RangedValue};

create_safe_prop_enums! {
    #[default(DoubleThrowAlternating)]
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Type {
        SingleThrow("st", "Single Throw"),
        DoubleThrow("dt", "Double Throw"),
        DoubleThrowAlternating("da", "Double Throw (alt)"),
    }
}

thread_local! {
    static DISPLAY_NAMES_A: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static NAMES_A: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static DISPLAY_NAMES_B: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static NAMES_B: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static DISPLAY_NAMES_C: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
    static NAMES_C: Mutex<FixedVec<Weak<str>>> = Mutex::new(vec![].into());
}

#[derive(Default, Serialize, Deserialize)]
struct RelayState {
    closed: bool,
}

impl InternalCircuitState for RelayState {
    fn serialize(&self, copy: bool) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap_or_default()
    }
}

struct Relay {
    dir: Direction4,
    flip: bool,
    ty: Type,
    groups: usize,

    coil: CircuitPinInfo,
    pins: Box<[CircuitPinInfo]>,
}

impl Relay {
    fn new() -> Self {
        Self {
            dir: Direction4::Left,
            ty: Type::DoubleThrowAlternating,
            flip: false,
            groups: 2,

            coil: CircuitPinInfo::new(
                0,
                InternalPinDirection::Inside,
                "coil",
                "Coil",
                Direction4::Left,
            ),
            pins: Default::default(),
        }
    }

    fn draw(
        pins: &mut dyn ExactSizeIterator<Item = Color32>,
        groups: usize,
        coil: (bool, Color32),
        dir: Direction4,
        ty: Type,
        flip: bool,
        ctx: &PaintContext,
    ) {
        let angle = dir.inverted_ud().angle_to_left();

        use crate::graphics::RelayGraphicsMode;

        match ty {
            Type::SingleThrow => crate::graphics::relay(
                coil.1,
                coil.0,
                RelayGraphicsMode::SingleThrow(&mut pins.chunks().map(|[a, b]| {
                    RelaySingleThrowColors {
                        input: a,
                        output: b,
                    }
                })),
                flip,
                angle,
                dir,
                ctx,
            ),
            Type::DoubleThrow | Type::DoubleThrowAlternating => crate::graphics::relay(
                coil.1,
                coil.0,
                RelayGraphicsMode::DoubleThrow {
                    alternate: matches!(ty, Type::DoubleThrowAlternating),
                    groups: &mut pins.chunks().map(|[a, b, c]| RelayDoubleThrowColors {
                        open: b,
                        closed: a,
                        center: c,
                    }),
                },
                flip,
                angle,
                dir,
                ctx,
            ),
        };
    }

    fn describe_props(props: &CircuitPropertyStore) -> DynCircuitDescription {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Left);
        let flip = props.read_clone("flip").unwrap_or(false);
        let ty = props
            .read_clone("ty")
            .unwrap_or(Type::DoubleThrowAlternating);
        let groups = props
            .read_clone("groups")
            .unwrap_or(RangedValue::new_from(1usize.., 1, 2))
            .get();
        Self::describe(dir, flip, ty, groups)
    }

    fn size_props(props: &CircuitPropertyStore) -> Vec2u {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Left);
        let ty = props
            .read_clone("ty")
            .unwrap_or(Type::DoubleThrowAlternating);
        let groups = props
            .read_clone("groups")
            .unwrap_or(RangedValue::new_from(1usize.., 1, 2))
            .get();

        let size = Self::size(ty, groups);
        if dir.is_vertical() {
            [size.y, size.x].into()
        } else {
            size
        }
    }

    fn size(ty: Type, groups: usize) -> Vec2u {
        let (double_throw, alternate_double_throw) = match ty {
            Type::SingleThrow => (false, false),
            Type::DoubleThrow => (true, false),
            Type::DoubleThrowAlternating => (true, true),
        };
        let width = calc_relay_width(double_throw, groups, alternate_double_throw);
        [width as u32, 3].into()
    }

    fn describe(dir: Direction4, flip: bool, ty: Type, groups: usize) -> DynCircuitDescription {
        let mut pins = vec![];
        pins.push(CircuitPinDescription {
            active: true,
            display_name: "Coil".into(),
            display_dir: Some(Direction4::Left),
            dir: InternalPinDirection::Inside,
            name: "coil".into(),
            pos: [0, 1].into(),
        });

        for i in 0..groups {
            let display_name_a = if groups == 1 {
                "A".into()
            } else {
                get_cached_name(&DISPLAY_NAMES_A, i, |i| format!("A{i}").into()).into()
            };

            let display_name_b = if groups == 1 {
                "B".into()
            } else {
                get_cached_name(&DISPLAY_NAMES_B, i, |i| format!("B{i}").into()).into()
            };

            let fi = i as u32;

            match ty {
                Type::SingleThrow => {
                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_a,
                        display_dir: Some(Direction4::Down),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_A, i, |i| format!("a{i}").into()).into(),
                        pos: [1 + fi, 2].into(),
                    });

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_b,
                        display_dir: Some(Direction4::Up),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_B, i, |i| format!("b{i}").into()).into(),
                        pos: [1 + fi, 0].into(),
                    });
                }
                Type::DoubleThrow => {
                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_a,
                        display_dir: Some(Direction4::Up),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_A, i, |i| format!("a{i}").into()).into(),
                        pos: [1 + fi * 2, 0].into(),
                    });

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_b,
                        display_dir: Some(Direction4::Up),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_B, i, |i| format!("b{i}").into()).into(),
                        pos: [2 + fi * 2, 0].into(),
                    });

                    let display_name_c = if groups == 1 {
                        "C".into()
                    } else {
                        get_cached_name(&DISPLAY_NAMES_C, i, |i| format!("C{i}").into()).into()
                    };

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_c,
                        display_dir: Some(Direction4::Down),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_C, i, |i| format!("c{i}").into()).into(),
                        pos: [1 + fi * 2, 2].into(),
                    });
                }
                Type::DoubleThrowAlternating => {
                    let inverted = i % 2 == 1;
                    let x = if i % 2 == 0 {
                        1 + (fi / 2) * 3
                    } else {
                        2 + (fi / 2) * 3
                    };

                    let dir_ab = if inverted {
                        Direction4::Down
                    } else {
                        Direction4::Up
                    };

                    let y_ab = if inverted { 2 } else { 0 };

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_a,
                        display_dir: Some(dir_ab),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_A, i, |i| format!("a{i}").into()).into(),
                        pos: [x, y_ab].into(),
                    });

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_b,
                        display_dir: Some(dir_ab),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_B, i, |i| format!("b{i}").into()).into(),
                        pos: [x + 1, y_ab].into(),
                    });

                    let display_name_c = if groups == 1 {
                        "C".into()
                    } else {
                        get_cached_name(&DISPLAY_NAMES_C, i, |i| format!("C{i}").into()).into()
                    };

                    let x_c = if inverted { x + 1 } else { x };

                    pins.push(CircuitPinDescription {
                        active: true,
                        display_name: display_name_c,
                        display_dir: Some(dir_ab.inverted()),
                        dir: InternalPinDirection::Custom,
                        name: get_cached_name(&NAMES_C, i, |i| format!("c{i}").into()).into(),
                        pos: [x_c, 2 - y_ab].into(),
                    });
                }
            }
        }

        let size = Self::size(ty, groups);

        let dir_normalized = dir.rotate_counterclockwise_by(Direction4::Left);
        let size_rotated = if dir.is_horizontal() {
            size
        } else {
            size.swapped()
        };

        let process_dir = |dir: Direction4| {
            let d = if flip { dir.inverted_ud() } else { dir };
            d.rotate_clockwise_by(dir_normalized)
        };

        let process_pos = |p: Vec2u| {
            if flip {
                crate::circuits::rotate_pos([p.x, 2 - p.y], size_rotated.into(), dir_normalized)
                    .into()
            } else {
                crate::circuits::rotate_pos(p.into(), size_rotated.into(), dir_normalized).into()
            }
        };

        let pins = pins
            .into_iter()
            .map(|p| CircuitPinDescription {
                active: true,
                display_name: p.display_name,
                display_dir: p.display_dir.map(process_dir),
                dir: p.dir,
                name: p.name,
                pos: process_pos(p.pos),
            })
            .collect();

        DynCircuitDescription {
            size: size_rotated,
            pins,
        }
    }

    fn get_closed(ctx: &CircuitStateContext) -> bool {
        ctx.read_circuit_internal_state(|s: &RelayState| s.closed).unwrap_or(false)
    }

    fn set_closed(ctx: &CircuitStateContext, closed: bool) {
        ctx.set_circuit_internal_state(Some(RelayState { closed }))
    }
}

impl CircuitImpl for Relay {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        let closed = Self::get_closed(state_ctx);
        let coil_color = self.coil.wire_or_self_color(state_ctx, paint_ctx.style);

        let mut iter = self
            .pins
            .iter()
            .map(|p| p.wire_or_self_color(state_ctx, paint_ctx.style));

        Relay::draw(
            &mut iter,
            self.groups,
            (closed, coil_color),
            self.dir,
            self.ty,
            self.flip,
            paint_ctx,
        );
    }

    fn create_pins(&mut self, circ: &Arc<Circuit>) -> Box<[CircuitPinInfo]> {
        let description = Self::describe_props(&circ.props);
        let pins: Box<[_]> = description
            .pins
            .iter()
            .filter_map(|p| p.to_active_info())
            .collect();

        self.coil = pins[0].clone();
        self.pins = pins[1..].iter().cloned().collect();

        pins
    }

    fn update_signals(&self, state_ctx: &CircuitStateContext, changed_pin: Option<usize>) {
        let coil = self.coil.get_state(state_ctx);

        if matches!(coil, WireState::Error) {
            return;
        }

        let closed = matches!(coil, WireState::True);

        Self::set_closed(state_ctx, closed);

        if matches!(changed_pin, None | Some(0)) {
            match self.ty {
                Type::SingleThrow => {
                    for [a, b] in self.pins.iter().chunks() {
                        a.set_raw_state(state_ctx, WireState::None);
                        b.set_raw_state(state_ctx, WireState::None);
                        let connected_a = a.connected_wire();
                        let connected_b = b.connected_wire();

                        if closed {
                            let wire = connected_a.or(connected_b);
                            if let Some(wire) = wire {
                                state_ctx.global_state.update_wire(wire, true);
                            }
                        } else {
                            if let Some(a) = connected_a {
                                state_ctx.global_state.update_wire(a, true);
                            }
                            if let Some(b) = connected_b {
                                state_ctx.global_state.update_wire(b, true);
                            }
                        }
                    }
                }
                Type::DoubleThrow | Type::DoubleThrowAlternating => {
                    for [a, b, c] in self.pins.iter().chunks() {
                        a.set_raw_state(state_ctx, WireState::None);
                        b.set_raw_state(state_ctx, WireState::None);
                        c.set_raw_state(state_ctx, WireState::None);
                        let connected_a = a.connected_wire();
                        let connected_b = b.connected_wire();

                        if let Some(a) = connected_a {
                            state_ctx.global_state.update_wire(a, true);
                        }
                        if let Some(b) = connected_b {
                            state_ctx.global_state.update_wire(b, true);
                        }
                        if connected_a.is_none() && connected_b.is_none()
                            || closed && connected_a.is_none()
                            || !closed && connected_b.is_none()
                        {
                            if let Some(c) = c.connected_wire() {
                                state_ctx.global_state.update_wire(c, true);
                            }
                        }
                    }
                }
            }
        }

        if let Some(pin) = changed_pin.filter(|p| *p != 0) {
            // update_signals shouldn't be usually called on custom pins
            // only outside of custom pin handling
            if let Some(wire) = self.pins[pin - 1].connected_wire() {
                state_ctx.global_state.update_wire(wire, false);
            } else {
                let other_pin = match self.ty {
                    Type::SingleThrow => {
                        if !closed {
                            None
                        } else {
                            let group = (pin - 1) / 2;
                            let group_pin = (pin - 1) % 2;

                            Some(group * 2 + 1 - group_pin)
                        }
                    }
                    Type::DoubleThrow | Type::DoubleThrowAlternating => 't: {
                        let group = (pin - 1) / 3;
                        let group_pin = (pin - 1) % 3;

                        let other_pin = match (group_pin, closed) {
                            (0, false) | (1, true) => None,
                            (0, true) | (1, false) => Some(2),
                            (_, false) => Some(1),
                            (_, true) => Some(0),
                        };

                        other_pin.map(|o| group * 3 + o)
                    }
                };

                if let Some(pin) = other_pin {
                    if let Some(wire) = self.pins[pin].connected_wire() {
                        state_ctx.global_state.update_wire(wire, false);
                    }
                }
            }
        }
    }

    fn custom_pin_apply_state(
        &self,
        ctx: &CircuitStateContext,
        pin: usize,
        state: &WireState,
        visited_items: &mut VisitedList,
    ) {
        if pin == 0 {
            return;
        }
        let closed = Self::get_closed(ctx);

        match self.ty {
            Type::SingleThrow => {
                if !closed {
                    return;
                }

                let group = (pin - 1) / 2;
                let group_pin = (pin - 1) % 2;

                let other_pin = &self.pins[group * 2 + 1 - group_pin];

                other_pin.apply_wire_state(ctx, state, false, visited_items);
            }
            Type::DoubleThrow | Type::DoubleThrowAlternating => 't: {
                let group = (pin - 1) / 3;
                let group_pin = (pin - 1) % 3;

                let other_pin = match (group_pin, closed) {
                    (0, false) | (1, true) => break 't,
                    (0, true) | (1, false) => 2,
                    (_, false) => 1,
                    (_, true) => 0,
                };

                let other_pin = &self.pins[group * 3 + other_pin];
                other_pin.apply_wire_state(ctx, state, false, visited_items);
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
        if pin == 0 {
            return;
        }
        let closed = Self::get_closed(ctx);

        match self.ty {
            Type::SingleThrow => {
                if !closed {
                    return;
                }

                let group = (pin - 1) / 2;
                let group_pin = (pin - 1) % 2;

                let other_pin = &self.pins[group * 2 + 1 - group_pin];

                other_pin.mutate_wire_state(ctx, state, visited_items);
            }
            Type::DoubleThrow | Type::DoubleThrowAlternating => 't: {
                let group = (pin - 1) / 3;
                let group_pin = (pin - 1) % 3;

                let other_pin = match (group_pin, closed) {
                    (0, false) | (1, true) => break 't,
                    (0, true) | (1, false) => 2,
                    (_, false) => 1,
                    (_, true) => 0,
                };

                let other_pin = &self.pins[group * 3 + other_pin];
                other_pin.mutate_wire_state(ctx, state, visited_items);
            }
        }
    }

    fn size(&self, circ: &Arc<Circuit>) -> Vec2u {
        Self::describe_props(&circ.props).size
    }

    fn prop_changed(&self, prop_id: &str, resize: &mut bool, recreate_pins: &mut bool) {
        if prop_id == "dir" || prop_id == "ty" || prop_id == "groups" {
            *resize = true;
            *recreate_pins = true;
        }
        if prop_id == "flip" {
            *recreate_pins = true;
        }
    }

    fn apply_props(&mut self, circ: &Arc<Circuit>, _: Option<&str>) {
        self.dir = circ.props.read_clone("dir").unwrap_or(Direction4::Left);
        self.flip = circ.props.read_clone("flip").unwrap_or(false);
        self.ty = circ
            .props
            .read_clone("ty")
            .unwrap_or(Type::DoubleThrowAlternating);
        self.groups = circ
            .props
            .read_clone("groups")
            .unwrap_or(RangedValue::new_from(1usize.., 1, 2))
            .get();
    }

    fn load_internal(
        &self,
        ctx: &CircuitStateContext,
        data: &serde_intermediate::Intermediate,
        paste: bool,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn InternalCircuitState>> {
        serde_intermediate::from_intermediate::<RelayState>(data)
            .ok()
            .map(|s| Box::new(s) as Box<_>)
    }
}

pub struct RelayPreview {}

impl CircuitPreviewImpl for RelayPreview {
    fn type_name(&self) -> DynStaticStr {
        "relay".into()
    }

    fn display_name(&self) -> DynStaticStr {
        "Relay".into()
    }

    fn description(&self) -> DynStaticStr {
        "\
        A component, that connects different wires, depending on Coil input state.\n\
        \n\
        In Single Throw mode, will connect contact pairs together on True coil input,\n\
        In Double Throw, will switch middle (C) contact between A on True and B on False coil input.\n\
        On Error coil input, will remain in last switch state.\
        ".into()
    }

    fn draw_preview(&self, props: &CircuitPropertyStore, ctx: &PaintContext, _in_world: bool) {
        let dir = props.read_clone("dir").unwrap_or(Direction4::Left);
        let flip = props.read_clone("flip").unwrap_or(false);
        let ty = props
            .read_clone("ty")
            .unwrap_or(Type::DoubleThrowAlternating);
        let groups = props
            .read_clone("groups")
            .unwrap_or(RangedValue::new_from(1usize.., 1, 2))
            .get();

        let pins_per_group = match ty {
            Type::SingleThrow => 2,
            Type::DoubleThrow => 3,
            Type::DoubleThrowAlternating => 3,
        };
        let mut iter = (0..pins_per_group * groups).map(|_| ctx.style.wire_colors.false_color());

        Relay::draw(
            &mut iter,
            groups,
            (false, ctx.style.wire_colors.false_color()),
            dir,
            ty,
            flip,
            ctx,
        );
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Relay::new())
    }

    fn load_copy_data(
        &self,
        imp: &serde_intermediate::Intermediate,
        internal: &serde_intermediate::Intermediate,
        ctx: &Arc<SimulationContext>,
        errors: &mut ErrorList,
    ) -> Option<Box<dyn CircuitPreviewImpl>> {
        Some(Box::new(RelayPreview {}))
    }

    fn default_props(&self) -> CircuitPropertyStore {
        CircuitPropertyStore::new([
            CircuitProperty::new("dir", "Direction", Direction4::Left),
            CircuitProperty::new("flip", "Flip", false),
            CircuitProperty::new("ty", "Type", Type::DoubleThrowAlternating),
            CircuitProperty::new(
                "groups",
                "Group count",
                RangedValue::new_from(1usize.., 1, 2),
            ),
        ])
    }

    fn describe(&self, props: &CircuitPropertyStore) -> DynCircuitDescription {
        Relay::describe_props(props)
    }
}

pub fn calc_relay_width(
    double_throw: bool,
    group_count: usize,
    alternate_double_throw: bool,
) -> usize {
    if double_throw {
        if alternate_double_throw {
            1 + (3 * (group_count / 2) + 2 * (group_count % 2))
        } else {
            1 + group_count * 2
        }
    } else {
        2 + group_count
    }
}

fn get_cached_name(
    k: &'static LocalKey<Mutex<FixedVec<Weak<str>>>>,
    index: usize,
    create: impl FnOnce(usize) -> Arc<str>,
) -> Arc<str> {
    k.with(|names| {
        let mut names = names.lock();
        if let Some(weak_name) = names.get(index) {
            if let Some(name) = weak_name.upgrade() {
                return name;
            }
        }

        let name = create(index);
        names.set(index, Arc::downgrade(&name));
        name
    })
}
