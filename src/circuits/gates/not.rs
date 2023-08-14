use eframe::epaint::{PathShape, Stroke, Color32};
use emath::vec2;

use crate::{
    circuits::{
        CircuitImpl, CircuitPinInfo, CircuitPreview, CircuitStateContext, InternalPinDirection,
    },
    state::WireState,
    vector::Vec2u,
    DynStaticStr, PaintContext,
};

struct Circuit {
    input: CircuitPinInfo,
    output: CircuitPinInfo,
}

impl Circuit {
    fn new() -> Self {
        Self {
            input: CircuitPinInfo::new([0, 0], InternalPinDirection::Inside, "in"),
            output: CircuitPinInfo::new([1, 0], InternalPinDirection::Outside, "out"),
        }
    }

    fn draw(ctx: &PaintContext, semi_transparent: bool) {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        let points = vec![
            ctx.rect.lerp_inside(vec2(0.25, 0.1)),
            ctx.rect.lerp_inside(vec2(1.32 / 2.0, 0.5)),
            ctx.rect.lerp_inside(vec2(0.25, 0.9)),
        ];
        ctx.paint.add(PathShape {
            points,
            closed: true,
            fill: fill_color,
            stroke: Stroke::new(2.0, border_color),
        });
        ctx.paint.circle(
            ctx.rect.lerp_inside(vec2(1.32 / 2.0, 0.5)),
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(2.0, border_color),
        );
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(paint_ctx, false);
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        vec![self.input.clone(), self.output.clone()].into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = self.input.get_input(state_ctx);
        let state = match state {
            WireState::None => WireState::None,
            WireState::True => WireState::False,
            WireState::False => WireState::True,
            WireState::Error => WireState::Error,
        };
        self.output.set_output(state_ctx, state);
    }
}

pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(ctx, in_world)
    }

    fn size(&self) -> Vec2u {
        [2, 1].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }

    fn type_name(&self) -> DynStaticStr {
        "not".into()
    }
}
