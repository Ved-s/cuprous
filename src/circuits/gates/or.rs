use eframe::epaint::{Color32, PathShape, QuadraticBezierShape, Shape, Stroke};
use emath::{pos2, Pos2};

use crate::{circuits::*, vector::Vec2f};

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

    fn draw(ctx: &PaintContext, semi_transparent: bool) {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        let points = [
            Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * 0.83,
                ctx.rect.center().y,
            ]),
            Vec2f::from([ctx.rect.left() + ctx.rect.width() * 0.5, ctx.rect.top() - ctx.rect.height() * 0.05]),
            Vec2f::from([ctx.rect.left() + ctx.rect.width() * 0.05, ctx.rect.top()]),
            Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * 0.5,
                ctx.rect.center().y,
            ]),
            Vec2f::from([ctx.rect.left() + ctx.rect.width() * 0.05, ctx.rect.bottom()]),
            Vec2f::from([ctx.rect.left() + ctx.rect.width() * 0.5, ctx.rect.bottom() + ctx.rect.height() * 0.05]),  
        ];

        let mut poly_points = vec![];

        for i in 0..3 {
            let start = i * 2;

            let a = points[start];
            let b = points[start + 1];
            let c = points[(start + 2) % points.len()];

            poly_points.extend(
                bezier_nd::Bezier::quadratic(&a, &b, &c)
                    .as_points((0.3/(ctx.screen.scale.sqrt())).max(0.01))
                    .map(Into::<Pos2>::into),
            );
        }

        ctx.paint.add(Shape::Path(PathShape {
            points: poly_points,
            closed: true,
            fill: fill_color,
            stroke: Stroke::new(2.0, border_color),
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
    fn draw_preview(&self, ctx: &PaintContext, in_world: bool) {
        Circuit::draw(ctx, in_world);
    }

    fn size(&self) -> Vec2u {
        [3, 3].into()
    }

    fn create_impl(&self) -> Box<dyn CircuitImpl> {
        Box::new(Circuit::new())
    }
}
