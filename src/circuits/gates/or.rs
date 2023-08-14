use emath::{pos2, vec2};

use crate::path::PathItem;

use super::gate::{GateTemplate, draw_from_path};

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "or",
    process_inputs: |i| i.iter().any(|b| *b),
    drawer: |ctx, semi_transparent| {
        let path = [
            PathItem::MoveTo(pos2(3.5, 1.5)),
            PathItem::QuadraticBezier(pos2(2.5, 0.0), pos2(1.5, 0.0)),
            PathItem::LineTo(pos2(0.25, 0.0)),
            PathItem::QuadraticBezier(pos2(1.25, 1.5), pos2(0.25, 3.0)),
            PathItem::LineTo(pos2(1.5, 3.0)),
            PathItem::QuadraticBezier(pos2(2.5, 3.0), pos2(3.5, 1.5)),
            PathItem::ClosePath,
        ];
        draw_from_path(ctx, semi_transparent, vec2(4.0, 3.0), &path);
    },
};
