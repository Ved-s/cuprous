use std::ops::Div;

use eframe::epaint::{Stroke, Color32};
use emath::{pos2, Pos2};

use crate::{
    path::{PathItem, PathItemIterator},
    vector::{Vec2f, Vec2u},
};

use super::gate::{calc_size_from_inputs, GateImpl, GateWireColors};

pub struct And;

impl GateImpl for And {
    const TYPE: &'static str = "and";
    const OP_DESC: &'static str = "Output is True only when all inputs are True, otherwise False.";

    fn process(inputs: &[bool], _: bool) -> bool {
        inputs.iter().all(|&v| v)
    }

    fn draw(
        wires: GateWireColors,
        angle: f32,
        in_world_preview: bool,
        _: bool,
        ctx: &crate::PaintContext,
    ) {
        let size: Vec2u = calc_size_from_inputs(wires.count() as u32).into();
        let size_f = size.convert(|v| v as f32);

        let width = size_f.x;
        let height = size_f.y;

        let transformer = |p: Pos2| {
            ctx.rect
                .lerp_inside(Vec2f::from(p).div(size_f).rotated_xy(angle, 0.5).into())
        };

        let opacity = if in_world_preview { 0.6 } else { 1.0 };

        let border_color = Color32::BLACK.linear_multiply(opacity);
        let fill_color = Color32::from_gray(200).linear_multiply(opacity);
        let straightness = (0.3 / (ctx.screen.scale.sqrt())).div(height).max(0.02);

        let fill = [
            PathItem::MoveTo(pos2(0.5, 0.0)),
            PathItem::LineTo(pos2(width * 0.4, 0.0)),
            PathItem::QuadraticBezier(pos2(width - 0.5, 0.0), pos2(width - 0.5, height / 2.0)),
            PathItem::QuadraticBezier(pos2(width - 0.5, height), pos2(width * 0.4, height)),
            PathItem::LineTo(pos2(0.5, height)),
            PathItem::ClosePath,
        ];

        let outer = [
            PathItem::MoveTo(pos2(0.48, 0.0)),
            PathItem::LineTo(pos2(width * 0.4, 0.0)),
            PathItem::QuadraticBezier(pos2(width - 0.5, 0.0), pos2(width - 0.5, height / 2.0)),
            PathItem::QuadraticBezier(pos2(width - 0.5, height), pos2(width * 0.4, height)),
            PathItem::LineTo(pos2(0.48, height)),
        ];

        fill.into_iter().create_path_shapes(
            fill_color,
            Stroke::NONE,
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );

        outer.into_iter().create_path_shapes(
            Color32::TRANSPARENT,
            Stroke::new(0.15 * ctx.screen.scale, border_color),
            straightness,
            transformer,
            |_, s| {
                ctx.paint.add(s);
            },
        );

        ctx.paint.line_segment([
            transformer(pos2(0.53, 0.0)),
            transformer(pos2(0.53, height)),
        ], Stroke::new(0.1 * ctx.screen.scale, border_color));
    }
}