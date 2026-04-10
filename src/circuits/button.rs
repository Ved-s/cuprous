use std::{
    any::{Any, TypeId},
    sync::Arc,
};

use eframe::egui::{
    Color32, CursorIcon, DragValue, PointerButton, Rect, Sense, Stroke, StrokeKind, Ui, Widget,
    vec2,
};
use smoldata::{SmolRead, SmolReadWrite, SmolWrite, raw::RawValue};

use crate::{
    Direction4, Direction8,
    circuits::{
        CircuitUpdateReason, PropertyChangedParams,
        props::{PropertyInfo, PropertyValue},
    },
    state::wires::WireState,
    str::{ArcRefStr, ArcStaticStr},
    vector::Vec2usize,
};

use super::{
    Circuit, CircuitCtx, CircuitImpl, CircuitPin, CircuitRenderingContext, CircuitRotationSupport,
    CircuitTransform, CircuitTransformSupport, PinDescription, PinType, TransformSupport,
};

#[derive(Default, Clone, SmolReadWrite)]
pub struct ButtonConfig {
    width: ButtonSize,
    height: ButtonSize,
}

#[derive(Default, Clone)]
pub struct Button {
    config: ButtonConfig,
}

#[derive(Default, SmolReadWrite)]
pub struct ButtonState {
    state: bool,
}

pub struct ButtonInstance {
    pin: Arc<CircuitPin>,
}

impl CircuitImpl for Button {
    type State = ButtonState;
    type Instance = ButtonInstance;

    fn id(&self) -> ArcStaticStr {
        "button".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "Button".into()
    }

    fn size(&self, _: CircuitTransform) -> Vec2usize {
        [self.config.width.0, self.config.height.0].into()
    }

    fn occupies_quarter(&self, _: CircuitTransform, qpos: Vec2usize) -> bool {
        qpos.x >= 1
            && qpos.x <= (self.config.width.0 - 1) * 2
            && qpos.y >= 1
            && qpos.y <= (self.config.height.0 - 1) * 2
    }

    fn describe_pins(&self, _: CircuitTransform) -> Box<[PinDescription]> {
        [PinDescription {
            pos: [self.config.width.0 - 1, (self.config.height.0 - 1) / 2].into(),
            id: "out".into(),
            display_name: "Out".into(),
            dir: Some(Direction8::Right),
            ty: PinType::Outside,
        }]
        .into()
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

    fn draw(&self, mut circuit: Option<CircuitCtx<Self>>, render: &CircuitRenderingContext) {
        let semi_transparent = false;
        let color = Color32::from_rgb(0xff, 0x5c, 0x1a);

        render.paint.rect(
            render.screen_rect.expand(render.paint.screen.scale * -0.5),
            render.paint.screen.scale * 0.25,
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_gray(92)),
            StrokeKind::Middle,
        );

        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        let state = circuit
            .as_ref()
            .and_then(|c| c.read_internal_state().map(|s| s.state))
            .unwrap_or_default();
        let color = if state {
            let c = color.linear_multiply(0.77);
            Color32::from_rgba_premultiplied(c.r(), c.g(), c.b(), color.a())
        } else {
            color
        }
        .linear_multiply(color_mul);

        // let rounding = Rounding {
        //     nw: 0.5 * render.paint.screen.scale,
        //     ne: 0.5 * render.paint.screen.scale,
        //     sw: 0.5 * render.paint.screen.scale,
        //     se: 0.5 * render.paint.screen.scale,
        // };

        let stroke = Stroke {
            width: 0.1 * render.paint.screen.scale,
            color: Color32::from_gray(48),
        };

        let diameter = self.config.width.0.min(self.config.height.0) as f32 - 1.5;

        // render.paint.rect(ctx.rect, rounding, color, stroke);
        render.paint.circle(
            render.screen_rect.center(),
            diameter * 0.5 * render.paint.screen.scale,
            color,
            stroke,
        );

        // let font = FontId::monospace(ctx.screen.scale * visuals.font_scale);

        // ctx.paint.text(
        //     ctx.rect.center(),
        //     Align2::CENTER_CENTER,
        //     text,
        //     font,
        //     visuals.font_color,
        // );

        if let Some(cir) = &mut circuit {
            let ui = render.paint.ui;

            let id = ui.id().with("buttoninteraction").with(cir.circuit.id);

            let size = diameter * render.paint.screen.scale;

            let rect = Rect::from_center_size(render.screen_rect.center(), vec2(size, size));

            let interaction = ui.interact(rect, id, Sense::drag());
            if interaction.hovered() {
                ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
            }
            let shift = ui.input(|input| input.modifiers.shift);
            if interaction.drag_started_by(PointerButton::Primary)
                || !shift && interaction.drag_stopped_by(PointerButton::Primary)
            {
                let state = cir.write_internal_state();
                state.state = !state.state;
                let state = state.state;
                cir.set_pin_output(&cir.instance.pin, WireState::Bool(state));
            }
        }
    }

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance {
        let pins = circuit.pins.read();
        ButtonInstance {
            pin: pins[0].pin.clone(),
        }
    }

    fn pins_changed(&self, circuit: &Circuit, instance: &mut Self::Instance) {
        let pins = circuit.pins.read();

        instance.pin = pins[0].pin.clone();
    }

    fn update(&self, mut circuit: CircuitCtx<Self>, _: CircuitUpdateReason) {
        let state = circuit
            .read_internal_state()
            .map(|s| s.state)
            .unwrap_or(false);
        circuit.set_pin_output(&circuit.instance.pin, WireState::Bool(state));
    }

    fn save_config(&self) -> Option<RawValue> {
        RawValue::write_object(&self.config).ok()
    }

    fn load_config(&mut self, data: &RawValue) -> Result<(), eyre::Report> {
        self.config = data.read_object()?;
        Ok(())
    }

    fn save_state(
        &self,
        _circuit: &Circuit,
        _instance: &Self::Instance,
        state: &Self::State,
    ) -> Option<smoldata::raw::RawValue> {
        RawValue::write_object(state).ok()
    }

    fn load_state(
        &self,
        _circuit: &Arc<Circuit>,
        _instance: &Self::Instance,
        data: &RawValue,
    ) -> Result<Self::State, eyre::Report> {
        data.read_object().map_err(Into::into)
    }

    fn enum_properties(&self, f: &mut dyn FnMut(&PropertyInfo)) {
        f(&PropertyInfo {
            id: ArcRefStr::Ref("width"),
            display_name: ArcRefStr::Ref("Width"),
            type_id: TypeId::of::<ButtonSize>(),
            affects_geometry_or_pins: true,
        });
        f(&PropertyInfo {
            id: ArcRefStr::Ref("height"),
            display_name: ArcRefStr::Ref("Height"),
            type_id: TypeId::of::<ButtonSize>(),
            affects_geometry_or_pins: true,
        });
    }

    fn get_property_value<'a>(&'a mut self, id: &str) -> Option<&'a mut dyn PropertyValue> {
        match id {
            "width" => Some(&mut self.config.width),
            "height" => Some(&mut self.config.height),
            _ => None,
        }
    }

    fn property_changed(
        &self,
        circuit_instance: Option<(&Circuit, &mut Self::Instance)>,
        prop: &str,
        params: &mut PropertyChangedParams,
    ) {
        if prop == "width" || prop == "height" {
            params.trigger_update = true;

            if let Some((circuit, instance)) = circuit_instance {
                instance.pin = circuit.pins.read()[0].pin.clone();
            }
        }
    }
}

#[derive(Clone, Copy)]
struct ButtonSize(usize);

impl Default for ButtonSize {
    fn default() -> Self {
        Self(3)
    }
}

impl PropertyValue for ButtonSize {
    fn clone_dyn(&self) -> Box<dyn PropertyValue> {
        Box::new(*self)
    }

    fn clone_into_dyn(&self, other: &mut dyn PropertyValue) {
        if let Some(other) = (other as &mut dyn Any).downcast_mut() {
            self.clone_into(other);
        }
    }

    fn ui(&self, ui: &mut Ui) -> Option<Box<dyn PropertyValue>> {
        let mut value = self.0;
        let res = DragValue::new(&mut value).ui(ui);
        if !res.changed() {
            return None;
        }
        value = value.max(3);
        if value != self.0 {
            Some(Box::new(Self(value)))
        } else {
            None
        }
    }
}

impl SmolRead for ButtonSize {
    fn read(reader: smoldata::reader::ValueReader) -> smoldata::reader::ReadResult<Self> {
        Ok(Self(<usize as SmolRead>::read(reader)?.max(3)))
    }
}

impl SmolWrite for ButtonSize {
    fn write(&self, writer: smoldata::writer::ValueWriter) -> std::io::Result<()> {
        <usize as SmolWrite>::write(&self.0, writer)
    }
}
