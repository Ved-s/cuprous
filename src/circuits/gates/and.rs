use eframe::epaint::{Color32, PathShape, Shape, Stroke};
use emath::{Pos2, pos2};

use crate::vector::Vec2f;

use super::gate::GateTemplate;

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "and",
    bool_combiner: |a, b| a && b,
    drawer: |ctx, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);

        let first_half_tile = 1.0 / 8.0;
        let last_half_tile = 7.0 / 8.0;

        let mut poly_points = vec![];

        {
            let a = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * last_half_tile,
                ctx.rect.center().y,
            ]);
            let b = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * last_half_tile,
                ctx.rect.top(),
            ]);
            let c = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * (3.0 / 8.0),
                ctx.rect.top(),
            ]);

            poly_points.extend(
                bezier_nd::Bezier::quadratic(&a, &b, &c)
                    .as_points((0.3 / (ctx.screen.scale.sqrt())).max(0.01))
                    .map(Into::<Pos2>::into),
            );
        }

        poly_points.push(pos2(
            ctx.rect.left() + ctx.rect.width() * first_half_tile,
            ctx.rect.top(),
        ));

        poly_points.push(pos2(
            ctx.rect.left() + ctx.rect.width() * first_half_tile,
            ctx.rect.bottom(),
        ));

        {
            let a = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * (3.0 / 8.0),
                ctx.rect.bottom(),
            ]);
            let b = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * last_half_tile,
                ctx.rect.bottom(),
            ]);
            let c = Vec2f::from([
                ctx.rect.left() + ctx.rect.width() * last_half_tile,
                ctx.rect.center().y,
            ]);
            

            poly_points.extend(
                bezier_nd::Bezier::quadratic(&a, &b, &c)
                    .as_points((0.3 / (ctx.screen.scale.sqrt())).max(0.01))
                    .map(Into::<Pos2>::into),
            );
        }

        ctx.paint.add(Shape::Path(PathShape {
            points: poly_points,
            closed: true,
            fill: fill_color,
            stroke: Stroke::new(2.0, border_color),
        }));
    },
};
