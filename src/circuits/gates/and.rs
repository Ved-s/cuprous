use eframe::epaint::{Color32, Stroke};
use emath::{pos2, vec2};

use crate::path::{PathItemIterator, PathItem};

use super::gate::GateTemplate;

pub const TEMPLATE: GateTemplate = GateTemplate {
    id: "and",
    bool_combiner: |a, b| a && b,
    drawer: |ctx, semi_transparent| {
        let opacity = if semi_transparent { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let straightness = (0.3 / (ctx.screen.scale.sqrt())).max(0.01);

        /*
         M 0.5 0 
         L 2 0 
         C 4 0 4 3 2 3
         L 0.5 3
         Z
        */
        let path = [
            PathItem::MoveTo(pos2(0.5, 0.0)),
            PathItem::LineTo(pos2(2.0, 0.0)),
            PathItem::CubicBezier(pos2(4.0, 0.0), pos2(4.0, 3.0), pos2(2.0, 3.0)),
            PathItem::LineTo(pos2(0.5, 3.0)),
            PathItem::ClosePath
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
