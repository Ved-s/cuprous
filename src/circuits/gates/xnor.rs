use eframe::epaint::{Color32, PathShape, Stroke};
use emath::{pos2, vec2, Pos2};

use crate::{
    path::{PathItem, PathItemIterator},
    vector::Vec2f,
};

use super::gate::GateTemplate;

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "xnor",
    name: "XNOR gate",
    process_inputs: |i| i.iter().filter(|b| **b).count() != 1, // TODO: make =1/is_odd a property 
    drawer: |ctx, angle, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.01);

        let path = [
            PathItem::MoveTo(pos2(3.25, 1.5)),
            PathItem::QuadraticBezier(pos2(2.5, 0.0), pos2(1.15, 0.0)),
            PathItem::LineTo(pos2(0.25, 0.0)),
            PathItem::QuadraticBezier(pos2(1.25, 1.5), pos2(0.25, 3.0)),
            PathItem::LineTo(pos2(1.15, 3.0)),
            PathItem::QuadraticBezier(pos2(2.5, 3.0), pos2(3.25, 1.5)),
            PathItem::ClosePath,
        ];

        let size = vec2(4.0, 3.0);
        let transformer = |p: Pos2| ctx.rect.lerp_inside(Vec2f::from(p.to_vec2() / size).rotated_xy(angle, 0.5).into());
        path.iter().cloned().create_path_shapes(
            fill_color,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );
        ctx.paint.circle(
            transformer(pos2(3.32, 1.5)),
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
        );

        let arc_points: Vec<_> = bezier_nd::Bezier::quadratic(
            &Vec2f::from(transformer(pos2(0.0, 0.0))),
            &Vec2f::from(transformer(pos2(1.0, 1.5))),
            &Vec2f::from(transformer(pos2(0.0, 3.0))),
        )
        .as_points(straightness)
        .map(Pos2::from)
        .collect();

        ctx.paint.add(PathShape {
            points: arc_points,
            closed: false,
            fill: Color32::TRANSPARENT,
            stroke: Stroke::new(0.1 * ctx.screen.scale, border_color),
        });
    },
};
