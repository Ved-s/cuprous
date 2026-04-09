use std::{any::TypeId, ops::Deref, sync::Arc};

use eframe::egui::{text::LayoutJob, Color32, FontFamily, FontId, TextFormat};
use eyre::Context;
use parking_lot::RwLock;
use smoldata::SmolReadWrite;

use crate::{
    Direction4, Direction8, Style, WIRE_WIDTH, circuits::{
        Circuit, CircuitCtx, CircuitImpl, CircuitPin, CircuitRenderPurpose, CircuitRenderingContext, CircuitRotationSupport, CircuitTransform, CircuitTransformSupport, CircuitUpdateReason, PinDescription, PinType, TransformSupport, props::{PropertyInfo, PropertyValue}
    }, pool::get_pooled, state::wires::WireState, str::{ArcRefStr, ArcStaticStr}, vector::{Vec2f, Vec2usize}, vertex_renderer::{ColoredTriangleBuffer, ColoredVertexRenderer}
};

#[derive(Clone, SmolReadWrite)]
pub struct ConstantConfig {
    value: WireState,
}

impl ConstantConfig {
    fn iter_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        f(&PropertyInfo {
            id: ArcRefStr::Ref("value"),
            display_name: ArcRefStr::Ref("Value"),
            type_id: TypeId::of::<WireState>(),
            affects_geometry_or_pins: false,
        })
    }

    fn get_property_value(&mut self, id: &str) -> Option<&mut dyn PropertyValue> {
        match id {
            "value" => Some(&mut self.value),
            _ => None,
        }
    }
}

pub struct Constant {
    config: ConstantConfig,
    display_name: RwLock<Option<Arc<str>>>,
}

impl Clone for Constant {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            display_name: RwLock::new(None),
        }
    }
}

impl Constant {
    pub const fn new(value: WireState) -> Self {
        Self {
            config: ConstantConfig { value },
            display_name: RwLock::new(None),
        }
    }

    fn append_state_text(
        state: &WireState,
        _short: bool,
        layout: &mut LayoutJob,
        style: &Style,
        font: &FontId,
    ) {
        match state {
            WireState::None => {
                layout.append(
                    "-",
                    0.0,
                    TextFormat::simple(font.clone(), style.wire_colors.none),
                );
            }
            WireState::Bool(true) => {
                layout.append(
                    "1",
                    0.0,
                    TextFormat::simple(font.clone(), style.wire_colors.r#true),
                );
            }
            WireState::Bool(false) => {
                layout.append(
                    "0",
                    0.0,
                    TextFormat::simple(font.clone(), style.wire_colors.r#false),
                );
            }
            WireState::Error => {
                layout.append(
                    "E",
                    0.0,
                    TextFormat::simple(font.clone(), style.wire_colors.error),
                );
            }
        }
    }
}

pub struct ConstantInstance {
    pin: Arc<CircuitPin>,
}

impl CircuitImpl for Constant {
    type State = ();

    type Instance = ConstantInstance;

    fn id(&self) -> ArcStaticStr {
        "const".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        let read = self.display_name.read();
        if let Some(name) = read.as_ref() {
            return name.clone().into();
        }
        drop(read);

        let mut write = self.display_name.write();
        if let Some(name) = write.as_ref() {
            return name.clone().into();
        }
        let name = match &self.config.value {
            WireState::None => "None",
            WireState::Bool(true) => "True",
            WireState::Bool(false) => "False",
            WireState::Error => "Error",
        };
        let arc: Arc<str> = format!("Constant ({name})").into();
        *write = Some(arc.clone());

        arc.into()
    }

    fn size(&self, _transform: CircuitTransform) -> Vec2usize {
        [1, 1].into()
    }

    fn transform_support(&self) -> CircuitTransformSupport {
        CircuitTransformSupport {
            rotation: Some(CircuitRotationSupport {
                support: TransformSupport::Automatic,
                default_dir: Direction4::Right,
            }),
            flip: None,
        }
    }

    fn describe_pins(&self, _transform: CircuitTransform) -> Box<[PinDescription]> {
        [PinDescription {
            pos: [0, 0].into(),
            id: "pin".into(),
            display_name: "".into(),
            dir: Some(Direction8::Right),
            ty: PinType::Outside,
        }]
        .into()
    }

    fn draw_blueprint_pins(&self) -> bool {
        false
    }

    fn draw(&self, circuit: Option<CircuitCtx<Self>>, render: &CircuitRenderingContext) {
        let icon = matches!(render.purpose, CircuitRenderPurpose::Icon);

        let draw_pin = !icon && circuit.is_none_or(|c| c.instance.pin.wire.read().is_none());

        if draw_pin {
            let mut pin_buffer = get_pooled::<ColoredTriangleBuffer>();

            crate::drawing::pin(
                render.screen_rect.center().into(),
                (WIRE_WIDTH / 2.0) * render.paint.screen.scale,
                &render.paint.style.pins,
                Some(render.transform.dir.into()),
                render.paint.style.wire_colors.get(&self.config.value),
                &mut pin_buffer,
            );

            render.paint.custom_draw(move |ctx| {
                let mut renderer = ColoredVertexRenderer::global(ctx.painter.gl());
                renderer.draw(
                    ctx.painter.gl(),
                    ctx.paint_info.screen_size_px,
                    pin_buffer.deref(),
                );
            });
        }

        let mut layout = LayoutJob::default();

        let font = FontId {
            family: FontFamily::Monospace,
            size: render.paint.screen.scale * 0.8,
        };

        if font.size < 2.0 {
            return;
        }

        Self::append_state_text(
            &self.config.value,
            icon,
            &mut layout,
            &render.paint.style,
            &font,
        );

        let galley = render.paint.ui.fonts_mut(|f| f.layout_job(layout));

        if icon {
            let text_pos =
                render.screen_rect.left_top() + (render.screen_rect.size() - galley.size()) / 2.0;
            render.paint.galley(text_pos, galley, Color32::WHITE);
        } else {
            let offset = 0.2 * render.paint.screen.scale;
            let center = render.screen_rect.center();

            let text_align: Vec2f = match render.transform.dir {
                Direction4::Up => [0.5, 0.0],
                Direction4::Right => [1.0, 0.5],
                Direction4::Down => [0.5, 1.0],
                Direction4::Left => [0.0, 0.5],
            }
            .into();

            let text_pos_base = render.transform.dir.inverted().into_dir_f32() * offset + center;
            let text_pos = text_pos_base - (text_align * galley.size());

            render.paint.galley(text_pos.into(), galley, Color32::WHITE);
        }
    }

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance {
        let pins = circuit.pins.read();
        ConstantInstance {
            pin: pins[0].pin.clone(),
        }
    }

    fn pins_changed(&self, circuit: &Circuit, instance: &mut Self::Instance) {
        let pins = circuit.pins.read();
        instance.pin = pins[0].pin.clone();
    }

    fn update(&self, ctx: CircuitCtx<Self>, _reason: CircuitUpdateReason) {
        ctx.instance
            .pin
            .set_output(ctx.state, ctx.tasks, self.config.value.clone());
    }

    fn save_config(&self) -> Option<smoldata::raw::RawValue> {
        smoldata::write_into_raw(&self.config).ok()
    }

    fn load_config(&mut self, data: &smoldata::raw::RawValue) -> Result<(), eyre::Report> {
        self.config = data.read_object().wrap_err("reading ConstantInstance")?;
        Ok(())
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        self.config.iter_properties(f);
    }

    fn get_property_value(&mut self, id: &str) -> Option<&mut dyn PropertyValue> {
        self.config.get_property_value(id)
    }

    fn property_changed(
        &self,
        _circuit_instance: Option<(&Circuit, &mut Self::Instance)>,
        prop: &str,
        params: &mut super::PropertyChangedParams,
    ) {
        if prop == "value" {
            params.trigger_update = true;
            *self.display_name.write() = None;
        }
    }
}
