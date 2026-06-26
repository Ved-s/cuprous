use std::{any::TypeId, ops::Deref, sync::Arc};

use smoldata::{SmolReadWrite, raw::RawValue};

use crate::{
    Direction4, Direction8, WIRE_WIDTH,
    components::{
        ComponentFlipSupport, ComponentImpl, ComponentPin, ComponentRotationSupport, ComponentTransformSupport, FlipType, PinType, PropertyChangedParams, TransformSupport, props::PropertyInfo
    },
    drawing,
    pool::get_pooled,
    state::wires::WireState,
    str::ArcStaticStr,
    vector::Vec2usize,
    vertex_renderer::{ColoredTriangleBuffer, ColoredVertex, ColoredVertexRenderer, Triangle},
};

use super::{
    Component, ComponentCtx, ComponentRenderingContext, ComponentTransform, ComponentUpdateReason,
    PinDescription,
};

#[derive(Default, Clone, SmolReadWrite)]
pub struct TransistorConfig {
    inverted: bool,
}

#[derive(Default, Clone)]
pub struct Transistor {
    config: TransistorConfig
}

pub struct TransistorInstance {
    pin_in: Arc<ComponentPin>,
    pin_out: Arc<ComponentPin>,
    pin_ctrl: Arc<ComponentPin>,
}

impl ComponentImpl for Transistor {
    type State = ();

    type Instance = TransistorInstance;

    fn id(&self) -> ArcStaticStr {
        "transistor".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Transistor".into()
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
        Vec2usize::new(2, 3)
    }

    fn occupies_quarter(&self, _transform: ComponentTransform, qpos: Vec2usize) -> bool {
        if qpos.x == 0 || qpos.x == 3 || qpos.y == 0 || qpos.y == 5 {
            return false;
        }

        if qpos.x == 1 && (qpos.y == 1 || qpos.y == 4) {
            return false;
        }

        true
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        [
            PinDescription {
                pos: [1, 0].into(),
                id: "out".into(),
                display_name: "Output".into(),
                dir: Some(Direction8::Up),
                ty: PinType::Outside,
            },
            PinDescription {
                pos: [0, 1].into(),
                id: "ctrl".into(),
                display_name: "Control".into(),
                dir: Some(Direction8::Left),
                ty: PinType::Inside,
            },
            PinDescription {
                pos: [1, 2].into(),
                id: "in".into(),
                display_name: "Input".into(),
                dir: Some(Direction8::Down),
                ty: PinType::Inside,
            },
        ]
        .into()
    }

    fn draw(&self, component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        // todo: handle wires with custom colors
        let (input, output, control) = match component {
            Some(component) => {
                let input = component.get_pin_input(&component.instance.pin_in);
                let control = component.get_pin_input(&component.instance.pin_ctrl);

                let output_wire = component
                    .instance
                    .pin_out
                    .wire
                    .read()
                    .as_ref()
                    .map(|w| w.id);
                let output = match output_wire {
                    None => component.get_pin_input(&component.instance.pin_out),
                    Some(wire) => component.state.wires.get_wire(wire),
                };

                (input, output, control)
            }
            None => (
                WireState::Bool(false),
                WireState::Bool(false),
                WireState::Bool(false),
            ),
        };

        let mut buffer = get_pooled::<ColoredTriangleBuffer>();

        let input_line: [[f32; 2]; _] = [[1.50, 2.50], [1.50, 2.20], [1.30, 1.90], [1.30, 1.66]];

        let output_line: [[f32; 2]; _] = [[1.30, 1.36], [1.30, 1.10], [1.50, 0.80], [1.50, 0.50]];

        let lines = [
            (input_line, render.paint.style.wire_colors.get(&input)),
            (output_line, render.paint.style.wire_colors.get(&output)),
        ];

        // todo: maybe precompute?
        for (line, color) in lines {
            drawing::path(
                &mut buffer,
                line.iter().map(|p| {
                    ColoredVertex::new(
                        render.transform_pos(p.into()),
                        color.to_normalized_gamma_f32(),
                    )
                }),
                WIRE_WIDTH * render.paint.screen.scale,
            );
        }

        let tri_points = [[1.60, 1.67], [1.00, 1.67], [1.30, 1.16]];
        let tri_verts = tri_points.map(|p| {
            ColoredVertex::new(
                render.transform_pos(p.into()),
                render
                    .paint
                    .style
                    .wire_colors
                    .get(&input)
                    .to_normalized_gamma_f32(),
            )
        });

        buffer.push_triangle(Triangle(tri_verts));

        let control_pad_color = if self.config.inverted {
            match &control {
                WireState::Bool(b) => render.paint.style.wire_colors.get(&WireState::Bool(!b)),
                c => render.paint.style.wire_colors.get(c),
            }
        } else {
            render.paint.style.wire_colors.get(&control)
        };

        buffer.add_quad_line(
            render.transform_pos([0.90, 1.00].into()),
            render.transform_pos([0.90, 2.00].into()),
            WIRE_WIDTH * render.paint.screen.scale,
            control_pad_color.to_normalized_gamma_f32(),
        );

        if self.config.inverted {
            let low = render.transform_pos([0.60, 1.50].into());
            let high = render.transform_pos([0.80, 1.50].into());

            let color_low = render.paint.style.wire_colors.get(&control);
            let color_high = control_pad_color;

            buffer.add_donut(
                render.transform_pos([0.70, 1.50].into()),
                0.095 * render.paint.screen.scale,
                0.205 * render.paint.screen.scale,
                |p| {
                    let ab = high - low;
                    let ac = p - low;
                    let t = (ab.dot(ac) / ab.dot(ab)).clamp(0.0, 1.0);

                    let color = color_low.lerp_to_gamma(color_high, t);
                    color.to_normalized_gamma_f32()
                },
            );
        } else {
            buffer.add_quad_line(
                render.transform_pos([0.5, 1.5].into()),
                render.transform_pos([0.9, 1.5].into()),
                WIRE_WIDTH * render.paint.screen.scale,
                render
                    .paint
                    .style
                    .wire_colors
                    .get(&control)
                    .to_normalized_gamma_f32(),
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
        TransistorInstance {
            pin_out: pins[0].pin.clone(),
            pin_ctrl: pins[1].pin.clone(),
            pin_in: pins[2].pin.clone(),
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        let pins = component.pins.read();

        instance.pin_out = pins[0].pin.clone();
        instance.pin_ctrl = pins[1].pin.clone();
        instance.pin_in = pins[2].pin.clone();
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, _reason: ComponentUpdateReason) {
        let control = ctx.get_pin_input(&ctx.instance.pin_ctrl);
        if matches!(control, WireState::Error) {
            ctx.set_pin_output(&ctx.instance.pin_out, WireState::Error);
            return;
        }
        let active = match control {
            WireState::Bool(b) => b ^ self.config.inverted,
            _ => false
        };

        let output = match active {
            false => WireState::None,
            true => ctx.get_pin_input(&ctx.instance.pin_in)
        };
        ctx.set_pin_output(&ctx.instance.pin_out, output);
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        f(&PropertyInfo {
            id: "inverted".into(),
            display_name: "Inverted".into(),
            type_id: TypeId::of::<bool>(),
            affects_geometry_or_pins: false,
        })
    }

    fn property_changed(
            &self,
            _omponent_instance: Option<(&Component, &mut Self::Instance)>,
            _rop: &str,
            params: &mut PropertyChangedParams,
        ) {
        params.trigger_update = true;
    }

    fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn super::props::PropertyValue> {
        Some(match id {
            "inverted" => &mut self.config.inverted,
            _ => return None
        })
    }

    fn save_config(&self) -> Option<RawValue> {
        RawValue::write_object(&self.config).ok()
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        self.config = data.read_object()?;

        Ok(())
    }
}
