use eframe::epaint::{Color32, Stroke};
use emath::{pos2, vec2};

use crate::{path::PathItem, vector::Vec2f};

use super::gate::{draw_from_path, GateTemplate};

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "nor",
    process_inputs: |i| !i.iter().any(|b| *b),
    drawer: |ctx, angle, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };
        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let path = [
            PathItem::MoveTo(pos2(3.25, 1.5)),
            PathItem::QuadraticBezier(pos2(2.5, 0.0), pos2(1.15, 0.0)),
            PathItem::LineTo(pos2(0.25, 0.0)),
            PathItem::QuadraticBezier(pos2(1.25, 1.5), pos2(0.25, 3.0)),
            PathItem::LineTo(pos2(1.15, 3.0)),
            PathItem::QuadraticBezier(pos2(2.5, 3.0), pos2(3.25, 1.5)),
            PathItem::ClosePath,
        ];
        draw_from_path(ctx, semi_transparent, vec2(4.0, 3.0), angle, &path);
        let circle_pos = ctx.rect.lerp_inside(Vec2f::from([3.32 / 4.0, 1.5 / 3.0]).rotated_xy(angle, 0.5).into());
        ctx.paint.circle(
            circle_pos,
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(2.0, border_color),
        );
    },
};
