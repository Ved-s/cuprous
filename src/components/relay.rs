use std::{
    f32::consts::FRAC_PI_2,
    ops::Deref,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use smoldata::{SmolReadWrite, raw::RawValue};

use crate::{
    Direction4, Direction8, WIRE_WIDTH,
    components::{ComponentImpl, ComponentPin, FlipType, PinType},
    drawing,
    multiwire::{MultiwireRoute, MultiwireRouter, MultiwireTargetState},
    pool::get_pooled,
    state::wires::WireState,
    str::ArcStaticStr,
    vector::Vec2usize,
    vertex_renderer::{ColoredTriangleBuffer, ColoredVertex, ColoredVertexRenderer},
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
    pin_com: Arc<ComponentPin>,
    pin_nc: Arc<ComponentPin>,
    pin_no: Arc<ComponentPin>,
    pin_coil: Arc<ComponentPin>,
}

pub struct RelayMultiwireRouter {
    active: Arc<AtomicBool>,
    pin_com: Arc<ComponentPin>,
    pin_nc: Arc<ComponentPin>,
    pin_no: Arc<ComponentPin>,
}

#[derive(Clone, Default)]
pub struct Relay {}

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
        [4, 3].into()
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        !(qpos.x == 0 || qpos.y == 0 || qpos.x == 7 || qpos.y == 5)
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        [
            PinDescription {
                pos: [0, 1].into(),
                id: "com".into(),
                display_name: "Common".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Multiwire,
            },
            PinDescription {
                pos: [2, 2].into(),
                id: "nc".into(),
                display_name: "Norm. Closed".into(),
                dir: Some(Direction8::Down),
                ty: PinType::Multiwire,
            },
            PinDescription {
                pos: [2, 0].into(),
                id: "no".into(),
                display_name: "Norm. Open".into(),
                dir: Some(Direction8::Up),
                ty: PinType::Multiwire,
            },
            PinDescription {
                pos: [3, 1].into(),
                id: "coil".into(),
                display_name: "Coil".into(),
                dir: Some(Direction8::Right),
                ty: PinType::Inside,
            },
        ]
        .into()
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        let mut buffer = get_pooled::<ColoredTriangleBuffer>();

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
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();
        RelayInstance {
            pin_com: pins[0].pin.clone(),
            pin_nc: pins[1].pin.clone(),
            pin_no: pins[2].pin.clone(),
            pin_coil: pins[3].pin.clone(),
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.pin_com = pins[0].pin.clone();
        instance.pin_nc = pins[1].pin.clone();
        instance.pin_no = pins[2].pin.clone();
        instance.pin_coil = pins[3].pin.clone();
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, reason: ComponentUpdateReason) {
        if matches!(reason, ComponentUpdateReason::ChangedPin(0..=2)) {
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

        let common_wire = ctx.instance.pin_com.wire.read().as_ref().map(|w| w.id);
        let Some(common_wire) = common_wire else {
            return;
        };

        ctx.tasks.add_wire_task(common_wire, false);

        let disconnected_pin = if active {
            &ctx.instance.pin_nc
        } else {
            &ctx.instance.pin_no
        };

        let disconnected_pin_wire = disconnected_pin.wire.read().as_ref().map(|w| w.id);
        if let Some(disconnected_pin_wire) = disconnected_pin_wire {
            ctx.tasks.add_wire_task(disconnected_pin_wire, false);
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

    fn create_multiwire_router(
        &self,
        _component: &Arc<Component>,
        instance: &Self::Instance,
        state: &Self::State,
    ) -> Box<dyn MultiwireRouter> {
        Box::new(RelayMultiwireRouter {
            active: state.active.clone(),
            pin_com: instance.pin_com.clone(),
            pin_nc: instance.pin_nc.clone(),
            pin_no: instance.pin_no.clone(),
        })
    }
}

impl MultiwireRouter for RelayMultiwireRouter {
    fn route(&self, pin: usize, routes: &mut Vec<MultiwireRoute>) {
        match pin {
            0 => {
                let pin = match self.active.load(Ordering::Relaxed) {
                    true => &self.pin_no,
                    false => &self.pin_nc,
                };
                let Some(wire) = pin.wire.read().as_ref().map(|w| w.id) else {
                    return;
                };
                routes.push(MultiwireRoute {
                    target_state: MultiwireTargetState::CurrentState,
                    wire_id: wire,
                })
            }

            1 => {
                if self.active.load(Ordering::Relaxed) {
                    return;
                };
                let Some(wire) = self.pin_com.wire.read().as_ref().map(|w| w.id) else {
                    return;
                };
                routes.push(MultiwireRoute {
                    target_state: MultiwireTargetState::CurrentState,
                    wire_id: wire,
                })
            }

            2 => {
                if !self.active.load(Ordering::Relaxed) {
                    return;
                };

                let Some(wire) = self.pin_com.wire.read().as_ref().map(|w| w.id) else {
                    return;
                };
                routes.push(MultiwireRoute {
                    target_state: MultiwireTargetState::CurrentState,
                    wire_id: wire,
                })
            }

            _ => {}
        }
    }
}
