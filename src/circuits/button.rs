use eframe::{epaint::{Color32, Rounding, FontId}, egui::{Sense, PointerButton}};
use emath::Align2;

use super::*;

struct Circuit {
    out_pin: CircuitPinInfo,
}

impl Circuit {
    fn new() -> Self {
        Self {
            out_pin: CircuitPinInfo::new([2, 1], InternalPinDirection::Outside, "out"),
        }
    }

    fn draw(state: Option<&CircuitStateContext>, ctx: &PaintContext, semi_transparent: bool) {
        let color_mul = if semi_transparent { 0.5 } else { 1.0 };
        ctx.paint.rect_filled(
            ctx.rect.expand(ctx.screen.scale * -0.5),
            Rounding::same(ctx.screen.scale * 0.25),
            Color32::from_gray(100).linear_multiply(color_mul),
        );

        let state = state
            .map(|s| {
                s.read_circuit_internal_state::<State, _>(|state| state.state)
                    .unwrap_or_default()
            })
            .unwrap_or_default();
        let color = if state {
            Color32::from_rgb(175, 20, 20)
        } else {
            Color32::from_rgb(200, 30, 30)
        }
        .linear_multiply(color_mul);
        ctx.paint.circle_filled(ctx.rect.center(), ctx.screen.scale * 0.75, color);
        
        let font = FontId::monospace(ctx.screen.scale * 0.5);

        ctx.paint.text(ctx.rect.center(), Align2::CENTER_CENTER, "PUSH", font, Color32::WHITE);
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, state_ctx: &CircuitStateContext, paint_ctx: &PaintContext) {
        Self::draw(Some(state_ctx), paint_ctx, false);

        // HACK: write proper circuit interactables
        let rect = paint_ctx.rect.expand(paint_ctx.screen.scale * -0.75);
        let interaction = paint_ctx.ui.interact(rect, paint_ctx.ui.auto_id_with(state_ctx.circuit.pos), Sense::drag());
        let shift = paint_ctx.egui_ctx.input(|input| input.modifiers.shift);
        if interaction.drag_started_by(PointerButton::Primary) || !shift && interaction.drag_released_by(PointerButton::Primary) {
            let new_state = state_ctx.write_circuit_internal_state::<State, _>(|s| { s.state = !s.state; s.state });
            self.out_pin.set_output(state_ctx, new_state.into());
        }
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![self.out_pin.clone()].into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = state_ctx
            .read_circuit_internal_state::<State, _>(|state| state.state)
            .unwrap_or_default();
        self.out_pin.set_output(state_ctx, state.into());
    }
}

#[derive(Default, Serialize, Deserialize)]
struct State {
    state: bool,
}

impl InternalCircuitState for State {
    fn serialize(&self) -> serde_intermediate::Intermediate {
        serde_intermediate::to_intermediate(self).unwrap()
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(None, ctx, in_world);
    }

    fn size(&self) -> Vec2u {
        [3, 3].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "button".into()
    }
}
