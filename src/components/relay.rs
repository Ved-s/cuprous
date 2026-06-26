use std::{
    any::{Any, TypeId},
    io,
    num::NonZeroUsize,
    ops::Deref,
    sync::{
        Arc, Weak,
        atomic::{AtomicBool, Ordering},
    },
};

use eframe::egui::{Color32, ComboBox, DragValue, Stroke, StrokeKind, Ui, Widget};
use parking_lot::Mutex;
use smoldata::{SmolRead, SmolReadWrite, SmolWrite, raw::RawValue};

use crate::{
    Direction4, Direction8,
    components::{
        ComponentImpl, ComponentPin, FlipType, PinType, PropertyChangedParams, RealizedPin,
        props::{PropertyInfo, PropertyValue},
    },
    multiwire::{MultiwireRoute, MultiwireRouter, MultiwireTargetState},
    pool::get_pooled,
    state::wires::WireState,
    str::ArcStaticStr,
    vector::Vec2usize,
    vertex_renderer::ColoredTriangleBuffer,
};

use super::{
    Component, ComponentCtx, ComponentFlipSupport, ComponentRenderingContext,
    ComponentRotationSupport, ComponentTransform, ComponentTransformSupport, ComponentUpdateReason,
    PinDescription, TransformSupport,
};

#[derive(SmolReadWrite)]
struct RelayStateSerialized {
    active: bool,
}

#[derive(Default)]
pub struct RelayState {
    active: Arc<AtomicBool>,
}

pub struct RelayInstance {
    pin_coil: Arc<ComponentPin>,
    pins: RelayPins,
}

struct RelayTogglePins {
    pin_com: Arc<ComponentPin>,
    pin_no: Arc<ComponentPin>,
    pin_nc: Arc<ComponentPin>,
}

struct RelaySwitchOnPins {
    pin_com: Arc<ComponentPin>,
    pin_no: Arc<ComponentPin>,
}

struct RelaySwitchOffPins {
    pin_com: Arc<ComponentPin>,
    pin_nc: Arc<ComponentPin>,
}

#[derive(Clone)]
enum RelayPins {
    Toggle(Arc<[RelayTogglePins]>),
    SwitchOn(Arc<[RelaySwitchOnPins]>),
    SwitchOff(Arc<[RelaySwitchOffPins]>),
}

pub struct RelayMultiwireRouter {
    active: Arc<AtomicBool>,
    pins: RelayPins,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, SmolReadWrite)]
enum RelayType {
    #[default]
    Toggle,
    SwitchOn,
    SwitchOff,
    ToggleThin,
}

impl RelayType {
    const ALL: &[Self] = &[
        Self::Toggle,
        Self::SwitchOn,
        Self::SwitchOff,
        Self::ToggleThin,
    ];

    fn label(self) -> &'static str {
        match self {
            RelayType::Toggle => "Toggle",
            RelayType::SwitchOn => "Switch-Off",
            RelayType::SwitchOff => "Switch-On",
            RelayType::ToggleThin => "Toggle (Thin)",
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct RelaySwitchCount(NonZeroUsize);

impl Default for RelaySwitchCount {
    fn default() -> Self {
        Self(NonZeroUsize::MIN)
    }
}

impl SmolWrite for RelaySwitchCount {
    fn write(&self, writer: smoldata::writer::ValueWriter) -> io::Result<()> {
        self.0.get().write(writer)
    }
}

impl SmolRead for RelaySwitchCount {
    fn read(reader: smoldata::reader::ValueReader) -> smoldata::reader::ReadResult<Self> {
        let count = usize::read(reader)?;
        Ok(Self(NonZeroUsize::new(count).unwrap_or(NonZeroUsize::MIN)))
    }
}

#[derive(Default, Clone, SmolReadWrite)]
struct RelayConfig {
    ty: RelayType,
    switch_count: RelaySwitchCount,
}

#[derive(Clone, Default)]
pub struct Relay {
    config: RelayConfig,
}

impl RelayState {
    fn serialize(&self) -> RelayStateSerialized {
        RelayStateSerialized {
            active: self.active.load(Ordering::Relaxed),
        }
    }

    fn from_serialized(ser: RelayStateSerialized) -> Self {
        Self {
            active: Arc::new(AtomicBool::new(ser.active)),
        }
    }
}

impl ComponentImpl for Relay {
    type State = RelayState;

    type Instance = RelayInstance;

    fn id(&self) -> ArcStaticStr {
        "relay".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Relay".into()
    }

    fn transform_support(&self) -> ComponentTransformSupport {
        ComponentTransformSupport {
            rotation: Some(ComponentRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Right,
            }),
            flip: Some(ComponentFlipSupport {
                support: TransformSupport::Automatic,
                ty: FlipType::Vertical,
            }),
        }
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        let switches = self.config.switch_count.0.get();
        let height = match self.config.ty {
            RelayType::Toggle => 1 + switches * 3,
            RelayType::SwitchOn => 2 + switches,
            RelayType::SwitchOff => 2 + switches,
            RelayType::ToggleThin => 1 + switches * 2,
        };

        [3, height].into()
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        if qpos.x == 0 || qpos.y == 0 || qpos.x == 5 {
            return false;
        }

        let switches = self.config.switch_count.0.get();
        let height = match self.config.ty {
            RelayType::Toggle => 1 + switches * 3,
            RelayType::SwitchOn => 2 + switches,
            RelayType::SwitchOff => 2 + switches,
            RelayType::ToggleThin => 1 + switches * 2,
        };

        qpos.y != height * 2 - 1
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        let pin_count = 1 + match self.config.ty {
            RelayType::Toggle => 3,
            RelayType::SwitchOn => 2,
            RelayType::SwitchOff => 2,
            RelayType::ToggleThin => 3,
        } * self.config.switch_count.0.get();

        let mut pins = Vec::with_capacity(pin_count);

        pins.push(PinDescription {
            pos: [1, 0].into(),
            id: "coil".into(),
            display_name: "Coil".into(),
            dir: Some(Direction8::Up),
            ty: PinType::Inside,
        });

        let single_switch = self.config.switch_count.0.get() == 1;

        let get_string = |num: usize, ty: RelayPinType, display_name: bool| -> ArcStaticStr {
            if single_switch && num == 0 {
                generate_pin_string(None, ty, display_name)
            } else {
                get_pin_string(num, ty, display_name).into()
            }
        };

        for i in 0..self.config.switch_count.0.get() {
            match self.config.ty {
                RelayType::Toggle => {
                    let y = i * 3 + 1;
                    pins.push(PinDescription {
                        pos: [0, y + 1].into(),
                        id: get_string(i, RelayPinType::Common, false),
                        display_name: get_string(i, RelayPinType::Common, true),
                        dir: Some(Direction8::Left),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y].into(),
                        id: get_string(i, RelayPinType::NormallyOpen, false),
                        display_name: get_string(i, RelayPinType::NormallyOpen, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y + 2].into(),
                        id: get_string(i, RelayPinType::NormallyClosed, false),
                        display_name: get_string(i, RelayPinType::NormallyClosed, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                }
                RelayType::SwitchOn => {
                    let y = i + 2;
                    pins.push(PinDescription {
                        pos: [0, y].into(),
                        id: get_string(i, RelayPinType::Common, false),
                        display_name: get_string(i, RelayPinType::Common, true),
                        dir: Some(Direction8::Left),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y].into(),
                        id: get_string(i, RelayPinType::NormallyOpen, false),
                        display_name: get_string(i, RelayPinType::NormallyOpen, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                }
                RelayType::SwitchOff => {
                    let y = i + 2;
                    pins.push(PinDescription {
                        pos: [0, y].into(),
                        id: get_string(i, RelayPinType::Common, false),
                        display_name: get_string(i, RelayPinType::Common, true),
                        dir: Some(Direction8::Left),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y].into(),
                        id: get_string(i, RelayPinType::NormallyClosed, false),
                        display_name: get_string(i, RelayPinType::NormallyClosed, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                }
                RelayType::ToggleThin => {
                    let y = i * 2 + 1;
                    pins.push(PinDescription {
                        pos: [0, y + 1].into(),
                        id: get_string(i, RelayPinType::Common, false),
                        display_name: get_string(i, RelayPinType::Common, true),
                        dir: Some(Direction8::Left),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y].into(),
                        id: get_string(i, RelayPinType::NormallyOpen, false),
                        display_name: get_string(i, RelayPinType::NormallyOpen, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                    pins.push(PinDescription {
                        pos: [2, y + 1].into(),
                        id: get_string(i, RelayPinType::NormallyClosed, false),
                        display_name: get_string(i, RelayPinType::NormallyClosed, true),
                        dir: Some(Direction8::Right),
                        ty: PinType::Multiwire,
                    });
                }
            }
        }

        pins.into_boxed_slice()
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        render.paint.rect(
            render.screen_rect.expand(render.paint.screen.scale * -0.5),
            render.paint.screen.scale * 0.25,
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_gray(92)),
            StrokeKind::Middle,
        );

        return;

        let mut buffer = get_pooled::<ColoredTriangleBuffer>();

        /*
        let active = component
            .as_ref()
            .and_then(|c| c.read_internal_state())
            .map(|s| s.active.load(Ordering::Relaxed))
            .unwrap_or(false);

        let (contact_colors, coil_color) = match component {
            None => {
                let color = render.paint.style.wire_colors.r#false;
                ([color, color], color)
            }
            Some(component) => {
                let inst = component.instance;
                let (open_pin, closed_pin) = if active {
                    (&inst.pin_nc, &inst.pin_no)
                } else {
                    (&inst.pin_no, &inst.pin_nc)
                };

                let coil_color = inst
                    .pin_coil
                    .wire
                    .read()
                    .as_ref()
                    .map(|w| w.color(&component.state.wires, &render.paint.style))
                    .unwrap_or_else(|| render.paint.style.wire_colors.none);

                let com_wire = match inst.pin_com.wire.read().clone() {
                    Some(wire) => Some(wire),
                    None => closed_pin.wire.read().clone(),
                };

                let com_color = com_wire
                    .map(|w| w.color(&component.state.wires, &render.paint.style))
                    .unwrap_or_else(|| render.paint.style.wire_colors.none);

                let open_color = open_pin
                    .wire
                    .read()
                    .as_ref()
                    .map(|w| w.color(&component.state.wires, &render.paint.style))
                    .unwrap_or_else(|| render.paint.style.wire_colors.none);

                ([com_color, open_color], coil_color)
            }
        };

        let contact_lines: [&[(f32, f32)]; 2] = [
            &[(0.50, 1.50), (1.10, 1.50), (2.50, 2.07), (2.50, 2.50)],
            &[(2.50, 0.50), (2.50, 1.00)],
        ];

        for i in 0..2 {
            let line = contact_lines[i];
            let color = contact_colors[i];

            drawing::path(
                &mut buffer,
                line.iter().map(|&(x, y)| {
                    let y = if active { 3.0 - y } else { y };
                    ColoredVertex::new(
                        render.transform_pos((x, y).into()),
                        color.to_normalized_gamma_f32(),
                    )
                }),
                WIRE_WIDTH * render.paint.screen.scale,
            );
        }

        let coil_direction_angle = render.transform.dir.inverted().into_angle_xp_cw();
        let donut_angle_start = coil_direction_angle + FRAC_PI_2;
        let donut_angle_end = coil_direction_angle - FRAC_PI_2;

        let donut_centers = [(3.10, 0.90), (3.10, 1.30), (3.10, 1.70), (3.10, 2.10)];

        // todo: this only points in 4 direction, use sliced pregenerated circle data instead
        for center in donut_centers {
            drawing::donut_segment(
                &mut buffer,
                render.transform_pos(center.into()),
                0.1 * render.paint.screen.scale,
                0.3 * render.paint.screen.scale,
                donut_angle_start..=donut_angle_end,
                coil_color.to_normalized_gamma_f32(),
            );
        }

        let coil_lines = [
            ((3.10, 0.70), (3.20, 0.70)),
            ((3.10, 1.10), (3.20, 1.10)),
            ((3.10, 1.50), (3.50, 1.50)),
            ((3.10, 1.90), (3.20, 1.90)),
            ((3.10, 2.30), (3.20, 2.30)),
        ];

        for (start, end) in coil_lines {
            buffer.add_quad_line(
                render.transform_pos(start.into()),
                render.transform_pos(end.into()),
                WIRE_WIDTH * render.paint.screen.scale,
                coil_color.to_normalized_gamma_f32(),
            );
        }

        render.paint.custom_draw(move |ctx| {
            let mut vertexes = ColoredVertexRenderer::global(ctx.painter.gl());
            vertexes.draw(
                ctx.painter.gl(),
                ctx.paint_info.screen_size_px,
                buffer.deref(),
            );
        });

        */
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        let pin_coil = pins[0].pin.clone();
        let pins = self.create_pins(&pins);
        RelayInstance { pin_coil, pins }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.pin_coil = pins[0].pin.clone();
        instance.pins = self.create_pins(&pins);
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, reason: ComponentUpdateReason) {
        if matches!(reason, ComponentUpdateReason::ChangedPin(1..)) {
            return;
        }

        let active = matches!(
            ctx.get_pin_input(&ctx.instance.pin_coil),
            WireState::Bool(true)
        );

        // ORDERING: This is the only place where the value is stored
        if ctx
            .read_internal_state()
            .map(|s| s.active.load(Ordering::Relaxed))
            .unwrap_or_default()
            == active
        {
            return;
        }

        ctx.write_internal_state()
            .active
            .store(active, Ordering::Relaxed);

        match &ctx.instance.pins {
            RelayPins::Toggle(switches) => {
                for switch in switches.iter() {
                    let common_wire = switch.pin_com.wire.read().as_ref().map(|w| w.id);
                    let Some(common_wire) = common_wire else {
                        return;
                    };

                    ctx.tasks.add_wire_task(common_wire, false);

                    let disconnected_pin = if active {
                        &switch.pin_nc
                    } else {
                        &switch.pin_no
                    };

                    let disconnected_pin_wire = disconnected_pin.wire.read().as_ref().map(|w| w.id);
                    if let Some(disconnected_pin_wire) = disconnected_pin_wire {
                        ctx.tasks.add_wire_task(disconnected_pin_wire, false);
                    }
                }
            }
            RelayPins::SwitchOn(switches) => {
                for switch in switches.iter() {
                    let common_wire = switch.pin_com.wire.read().as_ref().map(|w| w.id);
                    let Some(common_wire) = common_wire else {
                        return;
                    };

                    ctx.tasks.add_wire_task(common_wire, false);

                    if active {
                        continue;
                    }

                    let disconnected_pin_wire = switch.pin_no.wire.read().as_ref().map(|w| w.id);
                    if let Some(disconnected_pin_wire) = disconnected_pin_wire {
                        ctx.tasks.add_wire_task(disconnected_pin_wire, false);
                    }
                }
            }
            RelayPins::SwitchOff(switches) => {
                for switch in switches.iter() {
                    let common_wire = switch.pin_com.wire.read().as_ref().map(|w| w.id);
                    let Some(common_wire) = common_wire else {
                        return;
                    };

                    ctx.tasks.add_wire_task(common_wire, false);

                    if !active {
                        continue;
                    }

                    let disconnected_pin_wire = switch.pin_nc.wire.read().as_ref().map(|w| w.id);
                    if let Some(disconnected_pin_wire) = disconnected_pin_wire {
                        ctx.tasks.add_wire_task(disconnected_pin_wire, false);
                    }
                }
            }
        }
    }

    fn save_state(
        &self,
        _component: &Component,
        _instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<RawValue> {
        RawValue::write_object(&state.serialize()).ok()
    }

    fn load_state(
        &self,
        _component: &Arc<Component>,
        _instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        data.read_object()
            .map(RelayState::from_serialized)
            .map_err(Into::into)
    }

    fn save_config(&self) -> Option<RawValue> {
        RawValue::write_object(&self.config).ok()
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        self.config = data.read_object()?;
        Ok(())
    }

    fn create_multiwire_router(
        &self,
        _component: &Arc<Component>,
        instance: &Self::Instance,
        state: &Self::State,
    ) -> Box<dyn MultiwireRouter> {
        Box::new(RelayMultiwireRouter {
            active: state.active.clone(),
            pins: instance.pins.clone(),
        })
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        f(&PropertyInfo {
            id: "ty".into(),
            display_name: "Type".into(),
            type_id: TypeId::of::<RelayType>(),
            affects_geometry_or_pins: true,
        });
        f(&PropertyInfo {
            id: "switch_count".into(),
            display_name: "Switch count".into(),
            type_id: TypeId::of::<RelaySwitchCount>(),
            affects_geometry_or_pins: true,
        });
    }

    fn property_changed(
        &self,
        _component_instance: Option<(&Component, &mut Self::Instance)>,
        _prop: &str,
        params: &mut PropertyChangedParams,
    ) {
        params.trigger_update = true;
        params.invalidate_multiwire_router = true;
    }

    fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue> {
        Some(match id {
            "ty" => &mut self.config.ty,
            "switch_count" => &mut self.config.switch_count,
            _ => return None,
        })
    }
}

impl Relay {
    fn create_pins(&self, pins: &[RealizedPin]) -> RelayPins {
        let switches = self.config.switch_count.0.get();
        match self.config.ty {
            RelayType::Toggle | RelayType::ToggleThin => RelayPins::Toggle(
                (0..switches)
                    .map(|i| RelayTogglePins {
                        pin_com: pins[1 + i * 3].pin.clone(),
                        pin_no: pins[1 + i * 3 + 1].pin.clone(),
                        pin_nc: pins[1 + i * 3 + 2].pin.clone(),
                    })
                    .collect(),
            ),
            RelayType::SwitchOn => RelayPins::SwitchOn(
                (0..switches)
                    .map(|i| RelaySwitchOnPins {
                        pin_com: pins[1 + i * 2].pin.clone(),
                        pin_no: pins[1 + i * 2 + 1].pin.clone(),
                    })
                    .collect(),
            ),
            RelayType::SwitchOff => RelayPins::SwitchOff(
                (0..switches)
                    .map(|i| RelaySwitchOffPins {
                        pin_com: pins[1 + i * 2].pin.clone(),
                        pin_nc: pins[1 + i * 2 + 1].pin.clone(),
                    })
                    .collect(),
            ),
        }
    }
}

impl MultiwireRouter for RelayMultiwireRouter {
    fn route(&self, pin: usize, routes: &mut Vec<MultiwireRoute>) {
        let Some(pin) = pin.checked_sub(1) else {
            return;
        };

        let active = self.active.load(Ordering::Relaxed);

        let out_pin = match &self.pins {
            RelayPins::Toggle(switches) => {
                let switch_index = pin / 3;
                let Some(switch) = switches.get(switch_index) else {
                    return;
                };

                let switch_pin = pin % 3;

                match (switch_pin, active) {
                    (0, false) => &switch.pin_nc,
                    (0, true) => &switch.pin_no,
                    (1, true) => &switch.pin_com,
                    (2, false) => &switch.pin_com,
                    _ => return,
                }
            }
            RelayPins::SwitchOn(switches) => {
                let switch_index = pin / 2;
                let Some(switch) = switches.get(switch_index) else {
                    return;
                };

                let switch_pin = pin % 2;

                match (switch_pin, active) {
                    (0, true) => &switch.pin_no,
                    (1, true) => &switch.pin_com,
                    _ => return,
                }
            }
            RelayPins::SwitchOff(switches) => {
                let switch_index = pin / 2;
                let Some(switch) = switches.get(switch_index) else {
                    return;
                };

                let switch_pin = pin % 2;

                match (switch_pin, active) {
                    (0, false) => &switch.pin_nc,
                    (1, false) => &switch.pin_com,
                    _ => return,
                }
            }
        };

        let wire = out_pin.wire.read();
        let Some(wire) = wire.deref() else {
            return;
        };

        routes.push(MultiwireRoute {
            target_state: MultiwireTargetState::CurrentState,
            wire_id: wire.id,
        });
    }
}

impl PropertyValue for RelayType {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(*self)
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut::<Self>() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>> {
        let mut new_value = None;
        ComboBox::new("relay_type", "")
            .selected_text(self.label())
            .show_ui(ui, |ui| {
                for &value in Self::ALL {
                    if ui.selectable_label(*self == value, value.label()).clicked() {
                        new_value = Some(value);
                    }
                }
            });
        new_value.map(|v| Box::new(v) as Box<_>)
    }
}

impl PropertyValue for RelaySwitchCount {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(*self)
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut::<Self>() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>> {
        let mut value = self.0.get();
        let res = DragValue::new(&mut value).ui(ui);
        if !res.changed() {
            return None;
        }
        let nz = NonZeroUsize::new(value)?;
        if nz != self.0 {
            Some(Box::new(Self(nz)))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RelayPinType {
    Common,
    NormallyOpen,
    NormallyClosed,
}

fn get_pin_string(number: usize, ty: RelayPinType, display_name: bool) -> Arc<str> {
    static CACHE_ID_COM: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());
    static CACHE_ID_NO: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());
    static CACHE_ID_NC: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());
    static CACHE_DN_COM: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());
    static CACHE_DN_NO: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());
    static CACHE_DN_NC: Mutex<Vec<Option<Weak<str>>>> = Mutex::new(Vec::new());

    let cache_arr = match display_name {
        true => [&CACHE_DN_COM, &CACHE_DN_NO, &CACHE_DN_NC],
        false => [&CACHE_ID_COM, &CACHE_ID_NO, &CACHE_ID_NC],
    };
    let cache = match ty {
        RelayPinType::Common => cache_arr[0],
        RelayPinType::NormallyOpen => cache_arr[1],
        RelayPinType::NormallyClosed => cache_arr[2],
    };

    let mut cache = cache.lock();
    if let Some(cached) = cache
        .get(number)
        .and_then(|wo| wo.as_ref())
        .and_then(|w| w.upgrade())
    {
        return cached;
    }

    if cache.len() <= number {
        let len = cache.len();
        cache.reserve(number - len + 1);

        while cache.len() <= number {
            cache.push(None);
        }
    }

    let arc = Arc::<str>::from(generate_pin_string(Some(number), ty, display_name));

    cache[number] = Some(Arc::downgrade(&arc));

    arc
}

fn generate_pin_string(
    number: Option<usize>,
    ty: RelayPinType,
    display_name: bool,
) -> ArcStaticStr {
    let Some(number) = number else {
        return match (ty, display_name) {
            (RelayPinType::Common, true) => "Common",
            (RelayPinType::Common, false) => "com",
            (RelayPinType::NormallyOpen, true) => "Norm. Open",
            (RelayPinType::NormallyOpen, false) => "no",
            (RelayPinType::NormallyClosed, true) => "Norm. Closed",
            (RelayPinType::NormallyClosed, false) => "nc",
        }
        .into();
    };
    let str = match (ty, display_name) {
        (RelayPinType::Common, true) => format!("Common {number}"),
        (RelayPinType::Common, false) => format!("com{number}"),
        (RelayPinType::NormallyOpen, true) => format!("Norm. Open {number}"),
        (RelayPinType::NormallyOpen, false) => format!("no{number}"),
        (RelayPinType::NormallyClosed, true) => format!("Norm. Closed {number}"),
        (RelayPinType::NormallyClosed, false) => format!("nc{number}"),
    };
    ArcStaticStr::Arc(str.into())
}
