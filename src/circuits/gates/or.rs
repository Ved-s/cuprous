use eframe::epaint::{Color32, Stroke};
use emath::{pos2, vec2};

use crate::path::{PathItem, PathItemIterator};

use super::gate::GateTemplate;

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "or",
    bool_combiner: |a, b| a || b,
    drawer: |ctx, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.01);

        /*
         M 3.5 1.5
         Q 2.5 0 1.5 0
         L 0.25 0
         Q 1.25 1.5 0.25 3
         L 1.5 3
         Q 2.5 3 3.5 1.5
         Z
        */
        let path = [
            PathItem::MoveTo(pos2(3.5, 1.5)),
            PathItem::QuadraticBezier(pos2(2.5, 0.0), pos2(1.5, 0.0)),
            PathItem::LineTo(pos2(0.25, 0.0)),
            PathItem::QuadraticBezier(pos2(1.25, 1.5), pos2(0.25, 3.0)),
            PathItem::LineTo(pos2(1.5, 3.0)),
            PathItem::QuadraticBezier(pos2(2.5, 3.0), pos2(3.5, 1.5)),
            PathItem::ClosePath,
        ];

        path.into_iter()
            .create_path_shapes(
                ctx.rect.left_top().to_vec2(),
                vec2(1.0 / 4.0 * ctx.rect.width(), 1.0 / 3.0 * ctx.rect.height()),
                fill_color,
                Stroke::new(2.0, border_color),
                straightness,
                |s| { ctx.paint.add(s); }
            );
    },
};
