use eframe::epaint::{Color32, Stroke};
use emath::{pos2, vec2};

use crate::path::PathItem;

use super::gate::{GateTemplate, draw_from_path};

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "nand",
    process_inputs: |i| !i.iter().fold(true, |a, b| a && *b),
    drawer: |ctx, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };
        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let path = [
            PathItem::MoveTo(pos2(0.5, 0.0)),
            PathItem::LineTo(pos2(1.5, 0.0)),
            PathItem::CubicBezier(pos2(3.65, 0.0), pos2(3.65, 3.0), pos2(1.5, 3.0)),
            PathItem::LineTo(pos2(0.5, 3.0)),
            PathItem::ClosePath
        ];
        draw_from_path(ctx, semi_transparent, vec2(4.0, 3.0), &path);
        ctx.paint.circle(
            pos2(3.32 / 4.0 * ctx.rect.width(), 1.5 / 3.0 * ctx.rect.height())
                + ctx.rect.left_top().to_vec2(),
            0.2 * ctx.screen.scale,
            fill_color,
            Stroke::new(2.0, border_color),
        );
    },
};
