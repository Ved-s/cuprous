use std::sync::Arc;

use eframe::egui::{Color32, CursorIcon, PointerButton, Rounding, Sense, Stroke};

use crate::{state::WireState, str::ArcStaticStr, vector::Vec2usize, Direction4, Direction8};

use super::{
    Circuit, CircuitCtx, CircuitImpl, CircuitPin, CircuitRenderingContext, CircuitRotationSupport,
    CircuitTransform, CircuitTransformSupport, PinDescription, PinType, TransformSupport,
};

#[derive(Clone)]
pub struct Button;

#[derive(Default)]
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
        [3, 3].into()
    }

    fn occupies_quarter(&self, _: CircuitTransform, qpos: Vec2usize) -> bool {
        qpos.x >= 1 && qpos.x <= 4 && qpos.y >= 1 && qpos.y <= 4
    }

    fn describe_pins(&self, _: CircuitTransform) -> Box<[PinDescription]> {
        [PinDescription {
            pos: [2, 1].into(),
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
            Rounding::same(render.paint.screen.scale * 0.25),
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_gray(92))
        );

        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        let state = circuit
            .as_ref()
            .and_then(|c| c.read_internal_state(|s| s.state))
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

        // render.paint.rect(ctx.rect, rounding, color, stroke);
        render.paint.circle(
            render.screen_rect.center(),
            0.75 * render.paint.screen.scale,
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

            let shrink = 0.75 * render.paint.screen.scale;

            let rect = render.screen_rect.shrink(shrink);

            let interaction = ui.interact(rect, id, Sense::drag());
            if interaction.hovered() {
                ui.ctx().set_cursor_icon(CursorIcon::PointingHand);
            }
            let shift = ui.input(|input| input.modifiers.shift);
            if interaction.drag_started_by(PointerButton::Primary)
                || !shift && interaction.drag_stopped_by(PointerButton::Primary)
            {
                let new_state = cir.write_internal_state(|s| {
                    s.state = !s.state;
                    s.state
                });
                cir.set_pin_output(&cir.instance.pin, WireState::Bool(new_state));
            }
        }
    }

    fn create_instance(&self, circuit: &Arc<Circuit>) -> Self::Instance {
        let pins = circuit.pins.read();
        ButtonInstance {
            pin: pins[0].pin.clone(),
        }
    }

    fn update_signals(&self, mut circuit: CircuitCtx<Self>, _: Option<usize>) {
        let state = circuit.read_internal_state(|s| s.state).unwrap_or(false);
        circuit.set_pin_output(&circuit.instance.pin, WireState::Bool(state));
    }
}
