use std::{
    any::{Any, TypeId},
    f32::consts::FRAC_PI_2,
    ops::Div,
    sync::Arc,
    time::Duration,
};

use eframe::{
    egui::{
        Color32, DragValue, Mesh, Stroke, StrokeKind, Widget,
        pos2,
    },
    epaint::Vertex,
};
use smoldata::{SmolRead, SmolReadWrite, SmolWrite, raw::RawValue};

use crate::{
    Direction4, Direction8,
    components::{
        Component, ComponentCtx, ComponentImpl, ComponentPin, ComponentRenderingContext,
        ComponentRotationSupport, ComponentTransform, ComponentTransformSupport,
        ComponentUpdateReason, PinDescription, PinType, TransformSupport,
        props::{PropertyInfo, PropertyValue},
    },
    state::wires::WireState,
    vector::{Vec2f, Vec2usize},
};

#[derive(Default, SmolReadWrite)]
pub struct ClockState {
    enabled: bool,
    output: bool,
    // #[serde(skip, default)]
    // cycle_start_ts: Option<Instant>,
}

pub struct ClockInstance {
    out: Arc<ComponentPin>,
    ctrl: Option<Arc<ComponentPin>>,
}

#[derive(Clone, SmolReadWrite)]
struct ClockConfig {
    frequency: FrequencyProp,
    controlled: bool,
    starting_state: bool,
}

impl Default for ClockConfig {
    fn default() -> Self {
        Self {
            frequency: FrequencyProp(1.0),
            controlled: false,
            starting_state: true,
        }
    }
}

#[derive(Clone, Default)]
pub struct Clock {
    config: ClockConfig,
}

impl Clock {
    fn set_timer(&self, ctx: &mut ComponentCtx<Self>) {
        if self.config.frequency.0 <= 0.0 {
            ctx.reset_timer();
            ctx.write_internal_state().output = false;
            return;
        }

        let interval = Duration::from_secs_f32(0.5 / self.config.frequency.0);
        ctx.set_timer(ctx.time_provider().now() + interval, Some(interval));
    }
}

impl ComponentImpl for Clock {
    type State = ClockState;

    type Instance = ClockInstance;

    fn id(&self) -> crate::str::ArcStaticStr {
        "clock".into()
    }

    fn display_name(&self) -> crate::str::ArcStaticStr {
        "Clock".into()
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        (3, 3).into()
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        qpos.x >= 1 && qpos.x <= 4 && qpos.y >= 1 && qpos.y <= 4
    }

    fn transform_support(&self) -> ComponentTransformSupport {
        ComponentTransformSupport {
            rotation: Some(ComponentRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Right,
            }),
            flip: None,
        }
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        let out = PinDescription {
            pos: (2, 1).into(),
            id: "out".into(),
            display_name: "Out".into(),
            dir: Some(Direction8::Right),
            ty: PinType::Outside,
        };

        let ctrl = if self.config.controlled {
            Some(PinDescription {
                pos: (0, 1).into(),
                id: "ctrl".into(),
                display_name: "Enable".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Inside,
            })
        } else {
            None
        };

        match ctrl {
            Some(ctrl) => Box::new([out, ctrl]) as Box<[_]>,
            None => Box::new([out]),
        }
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        render.paint.rect(
            render.screen_rect.expand(render.paint.screen.scale * -0.5),
            render.paint.screen.scale * 0.25,
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_gray(92)),
            StrokeKind::Middle,
        );

        let transformer = |x: f32, y: f32| -> Vec2f {
            let y = if self.config.starting_state {
                y
            } else {
                3.0 - y
            };
            render
                .screen_rect
                .lerp_inside(Vec2f::new(x, y).div(3.0))
                .into()
        };

        let enabled = component
            .as_ref()
            .map(|c| c.read_internal_state().map(|s| s.enabled).unwrap_or(false))
            .unwrap_or(true);

        let mut mesh = Mesh::default();

        fn draw_line_gradient(
            start: Vec2f,
            end: Vec2f,
            width: f32,
            colors: &[(f32, Color32)],
            mesh: &mut Mesh,
        ) {
            let up = (end - start)
                .with_length(width / 2.0)
                .rotated(FRAC_PI_2, 0.0);
            let tl = start + up;
            let bl = start - up;
            let tr = end + up;
            let br = end - up;

            for (i, (t, color)) in colors.iter().copied().enumerate() {
                mesh.vertices.push(Vertex {
                    pos: tl.lerp_to(tr, t).into(),
                    uv: pos2(0.0, 0.0),
                    color,
                });

                mesh.vertices.push(Vertex {
                    pos: bl.lerp_to(br, t).into(),
                    uv: pos2(0.0, 0.0),
                    color,
                });

                if i > 0 {
                    let len = mesh.vertices.len() as u32;
                    mesh.indices.push(len - 4);
                    mesh.indices.push(len - 3);
                    mesh.indices.push(len - 2);

                    mesh.indices.push(len - 3);
                    mesh.indices.push(len - 2);
                    mesh.indices.push(len - 1);
                }
            }
        }

        let top_color = if enabled {
            if self.config.starting_state {
                render.paint.style.wire_colors.r#true
            } else {
                render.paint.style.wire_colors.r#false
            }
        } else {
            Color32::GRAY
        };
        let bottom_color = if enabled {
            if self.config.starting_state {
                render.paint.style.wire_colors.r#false
            } else {
                render.paint.style.wire_colors.r#true
            }
        } else {
            Color32::GRAY
        };

        draw_line_gradient(
            transformer(1.0, 1.93),
            transformer(1.0, 1.07),
            render.paint.screen.scale * 0.15,
            &[(0.0, bottom_color), (1.0, top_color)],
            &mut mesh,
        );
        draw_line_gradient(
            transformer(1.5, 1.93),
            transformer(1.5, 1.07),
            render.paint.screen.scale * 0.15,
            &[(0.0, bottom_color), (1.0, top_color)],
            &mut mesh,
        );
        draw_line_gradient(
            transformer(2.0, 1.93),
            transformer(2.0, 1.07),
            render.paint.screen.scale * 0.15,
            &[(0.0, bottom_color), (1.0, top_color)],
            &mut mesh,
        );

        draw_line_gradient(
            transformer(0.625, 2.0),
            transformer(1.075, 2.0),
            render.paint.screen.scale * 0.15,
            &[
                (0.0, Color32::TRANSPARENT),
                (0.555555, bottom_color),
                (1.0, bottom_color),
            ],
            &mut mesh,
        );
        draw_line_gradient(
            transformer(1.925, 1.0),
            transformer(2.375, 1.0),
            render.paint.screen.scale * 0.15,
            &[
                (0.0, top_color),
                (0.444444, top_color),
                (1.0, Color32::TRANSPARENT),
            ],
            &mut mesh,
        );

        draw_line_gradient(
            transformer(0.925, 1.0),
            transformer(1.575, 1.0),
            render.paint.screen.scale * 0.15,
            &[(0.0, top_color), (1.0, top_color)],
            &mut mesh,
        );

        draw_line_gradient(
            transformer(1.425, 2.0),
            transformer(2.075, 2.0),
            render.paint.screen.scale * 0.15,
            &[(0.0, bottom_color), (1.0, bottom_color)],
            &mut mesh,
        );

        render.paint.add(mesh);
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        let pins = component.pins.read();

        ClockInstance {
            out: pins[0].pin.clone(),
            ctrl: if self.config.controlled {
                Some(pins[1].pin.clone())
            } else {
                None
            },
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.out = pins[0].pin.clone();
        instance.ctrl = if self.config.controlled {
            Some(pins[1].pin.clone())
        } else {
            None
        };
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, reason: ComponentUpdateReason) {
        match reason {
            ComponentUpdateReason::ChangedPin(1)
            | ComponentUpdateReason::StateReset
            | ComponentUpdateReason::ComponentPlaced
            | ComponentUpdateReason::PropertyChanged(_) => {
                let enabled = match &ctx.instance.ctrl {
                    Some(pin) => matches!(ctx.get_pin_input(pin), WireState::Bool(true)),
                    None => true,
                };

                if enabled {
                    let update_timer = if matches!(reason, ComponentUpdateReason::ChangedPin(_)) {
                        !ctx.read_internal_state()
                            .map(|s| s.enabled)
                            .unwrap_or_default()
                    } else {
                        true
                    };

                    if update_timer {
                        self.set_timer(&mut ctx);
                        let state = ctx.write_internal_state();
                        state.enabled = true;
                        state.output = self.config.starting_state;
                    }
                } else {
                    ctx.reset_timer();
                    let state = ctx.write_internal_state();
                    state.output = false;
                    state.enabled = false;
                }
            }
            ComponentUpdateReason::Timer => {
                let state = ctx.write_internal_state();
                state.output = !state.output;
            }
            _ => {}
        }

        let state = ctx.read_internal_state().map(|s| s.output).unwrap_or(false);
        ctx.set_pin_output(&ctx.instance.out, WireState::Bool(state));
    }

    fn save_config(&self) -> Option<smoldata::raw::RawValue> {
        RawValue::write_object(&self.config).ok()
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        self.config = data.read_object()?;
        Ok(())
    }

    fn save_state(
        &self,
        _component: &Component,
        _instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<RawValue> {
        RawValue::write_object(&state).ok()
    }

    fn load_state(
        &self,
        _component: &Arc<Component>,
        _instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        data.read_object().map_err(Into::into)
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        f(&PropertyInfo {
            id: "frequency".into(),
            display_name: "Frequency".into(),
            type_id: TypeId::of::<FrequencyProp>(),
            affects_geometry_or_pins: false,
        });
        f(&PropertyInfo {
            id: "controlled".into(),
            display_name: "Controlled".into(),
            type_id: TypeId::of::<bool>(),
            affects_geometry_or_pins: true,
        });
        f(&PropertyInfo {
            id: "starting_state".into(),
            display_name: "Starting state".into(),
            type_id: TypeId::of::<bool>(),
            affects_geometry_or_pins: false,
        });
    }

    fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue> {
        Some(match id {
            "frequency" => &mut self.config.frequency,
            "controlled" => &mut self.config.controlled,
            "starting_state" => &mut self.config.starting_state,
            _ => return None,
        })
    }

    fn property_changed(
        &self,
        _component_instance: Option<(&Component, &mut Self::Instance)>,
        prop: &str,
        params: &mut super::PropertyChangedParams,
    ) {
        match prop {
            "controlled" => {
                params.trigger_update = true;
            }
            "starting_state" => {}
            "frequency" => {
                params.trigger_update = true;
            }
            _ => {}
        }
    }
}

#[derive(Clone, Copy)]
struct FrequencyProp(f32);

impl SmolRead for FrequencyProp {
    fn read(reader: smoldata::reader::ValueReader) -> smoldata::reader::ReadResult<Self> {
        <f32 as SmolRead>::read(reader).map(Self)
    }
}

impl SmolWrite for FrequencyProp {
    fn write(&self, writer: smoldata::writer::ValueWriter) -> std::io::Result<()> {
        <f32 as SmolWrite>::write(&self.0, writer)
    }
}

impl PropertyValue for FrequencyProp {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(*self)
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut::<Self>() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut eframe::egui::Ui) -> Option<Box<dyn PropertyValue>> {
        let mut value = self.0;
        let resp = DragValue::new(&mut value)
            .speed(0.1)
            .range(0.0..=f32::MAX)
            .min_decimals(2)
            .suffix(" Hz")
            .ui(ui);
        if resp.changed() {
            return Some(Box::new(Self(value)));
        }

        None
    }
}
