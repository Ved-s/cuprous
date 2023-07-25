use eframe::epaint::{Color32, QuadraticBezierShape, Shape, Stroke};
use emath::pos2;

use crate::circuits::*;

struct Circuit {
    inputs: Box<[CircuitPinInfo]>,
    output: CircuitPinInfo,
}

impl Circuit {
    fn new() -> Self {
        Self {
            inputs: vec![
                CircuitPinInfo::new([0, 0], InternalPinDirection::Inside),
                CircuitPinInfo::new([0, 2], InternalPinDirection::Inside),
            ]
            .into_boxed_slice(),
            output: CircuitPinInfo::new([2, 1], InternalPinDirection::Outside),
        }
    }

    fn draw(ctx: &PaintContext, preview: bool) {
        ctx.paint.add(Shape::QuadraticBezier(QuadraticBezierShape {
            closed: false,
            fill: Color32::TRANSPARENT,
            stroke: Stroke::new(2.0, Color32::BLACK),
            points: [
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.05,
                    ctx.rect.top(),
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.5,
                    ctx.rect.center().y,
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.05,
                    ctx.rect.bottom(),
                ),
            ],
        }));

        ctx.paint.add(Shape::QuadraticBezier(QuadraticBezierShape {
            closed: false,
            fill: Color32::TRANSPARENT,
            stroke: Stroke::new(2.0, Color32::BLACK),
            points: [
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.05,
                    ctx.rect.top(),
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.5,
                    ctx.rect.top(),
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.83,
                    ctx.rect.center().y,
                ),
            ],
        }));

        ctx.paint.add(Shape::QuadraticBezier(QuadraticBezierShape {
            closed: false,
            fill: Color32::TRANSPARENT,
            stroke: Stroke::new(2.0, Color32::BLACK),
            points: [
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.05,
                    ctx.rect.bottom(),
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.5,
                    ctx.rect.bottom(),
                ),
                pos2(
                    ctx.rect.left() + ctx.rect.width() * 0.83,
                    ctx.rect.center().y,
                ),
            ],
        }));
    }
}

impl CircuitImpl for Circuit {
    fn draw(&self, _: &CircuitStateContext, paint_ctx: &PaintContext) {
        Circuit::draw(paint_ctx, false);
    }

    fn create_pins(&self) -> Box<[CircuitPinInfo]> {
        let mut vec = vec![self.output.clone()];
        vec.extend(self.inputs.iter().cloned());
        vec.into_boxed_slice()
    }

    fn update_signals(&mut self, state_ctx: &CircuitStateContext, _: Option<usize>) {
        let state = self
            .inputs
            .iter()
            .map(|i| i.get_input(state_ctx))
            .reduce(|a, b| a.combine_boolean(b, |a, b| a || b))
            .unwrap_or_default();
        self.output.set_output(state_ctx, state);
    }
}

#[derive(Debug)]
pub struct Preview {}

impl CircuitPreview for Preview {
    fn draw_preview(&self, ctx: &PaintContext) {
        Circuit::draw(ctx, true);
    }

    fn size(&self) -> Vec2u {
        [3, 3].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }
}
