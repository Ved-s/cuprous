use emath::{pos2, vec2};

use crate::path::PathItem;

use super::gate::{GateTemplate, draw_from_path};

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "and",
    name: "AND gate",
    process_inputs: |i| i.iter().all(|b| *b),
    drawer: |ctx, angle, semi_transparent| {
        let path = [
            PathItem::MoveTo(pos2(0.5, 0.0)),
            PathItem::LineTo(pos2(2.0, 0.0)),
            PathItem::CubicBezier(pos2(4.0, 0.0), pos2(4.0, 3.0), pos2(2.0, 3.0)),
            PathItem::LineTo(pos2(0.5, 3.0)),
            PathItem::ClosePath
        ];
        draw_from_path(ctx, semi_transparent, vec2(4.0, 3.0), angle, &path);
    },
};
