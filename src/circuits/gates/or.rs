use eframe::epaint::{Color32, PathShape, Shape, Stroke};
use emath::Pos2;

use crate::vector::Vec2f;

use super::gate::GateTemplate;

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "or",
    bool_combiner: |a, b| a || b,
    drawer: |ctx, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        let points = [
            Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * (7.0/8.0),
                ctx.rect.center().y,
            ]),
            Vec2f::from([ctx.rect.left() + ctx.rect.width() * 0.5, ctx.rect.top() - ctx.rect.height() * 0.05]),

            Vec2f::from([ctx.rect.left() + ctx.rect.width() * (3.0/64.0), ctx.rect.top()]),
            Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * (3.0/8.0),
                ctx.rect.center().y,
            ]),

            Vec2f::from([ctx.rect.left() + ctx.rect.width() * (3.0/64.0), ctx.rect.bottom()]),
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
};
