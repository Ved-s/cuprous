use std::f32::consts::TAU;

use eframe::epaint::{PathShape, Stroke, Color32};
use emath::{pos2, vec2, Pos2};

use crate::{
    circuits::{pin::ControlPinPosition, transistor::TransistorType},
    vector::Vec2f,
    PaintContext, ui::editor::CircuitBoardEditor,
};

#[allow(clippy::too_many_arguments)] // TODO: cleanup
pub fn transistor(
    ty: TransistorType,
    angle: f32,
    flip: bool,
    collector: Color32,
    base: Color32,
    emitter: Color32,
    open: bool,
    ctx: &PaintContext,
) {
    let middle_color = match open {
        true => collector,
        false => emitter,
    };

    let thickness = CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale;

    let collector_stroke = Stroke::new(thickness, collector);
    let base_stroke = Stroke::new(thickness, base);
    let emitter_stroke = Stroke::new(thickness, emitter);
    let middle_stroke = Stroke::new(thickness, middle_color);

    let size = vec2(2.0, 3.0);
    let trf = |x: f32, y: f32| {
        let y = y / size.y;
        let y = if flip { 1.0 - y } else { y };
        ctx.rect
            .lerp_inside(Vec2f::from([x / size.x, y]).rotated_xy(angle, 0.5).into())
    };

    let painter = ctx.paint;

    painter.line_segment([trf(0.5, 1.5), trf(0.9, 1.5)], base_stroke);

    painter.line_segment([trf(1.5, 0.5), trf(1.5, 0.8)], collector_stroke);
    painter.line_segment([trf(0.92, 1.33), trf(1.526, 0.73)], collector_stroke);

    painter.line_segment([trf(1.5, 2.5), trf(1.5, 2.2)], emitter_stroke);
    painter.line_segment([trf(0.92, 1.66), trf(1.526, 2.27)], emitter_stroke);

    painter.line_segment([trf(0.9, 1.0), trf(0.9, 2.0)], middle_stroke);

    let verts = match ty {
        TransistorType::NPN => [trf(1.35, 1.70), trf(1.45, 2.19), trf(0.96, 2.09)],
        TransistorType::PNP => [trf(1.08, 0.78), trf(1.00, 1.26), trf(1.47, 1.17)],
    };

    let arrow_color = match ty {
        TransistorType::NPN => emitter,
        TransistorType::PNP => collector,
    };

    painter.add(PathShape {
        points: verts.into(),
        closed: true,
        fill: arrow_color,
        stroke: Stroke::NONE,
    });
}

/// Returns center position
pub fn inside_pin(
    ctl: Option<(ControlPinPosition, Color32)>,
    outside_color: Color32,
    color: Color32,
    is_pico: Option<bool>,
    angle: f32,
    ctx: &PaintContext,
) -> Pos2 {
    let (size, center) = match ctl {
        None => ((2, 1), (0, 0)),
        Some((ControlPinPosition::Left, _)) => ((2, 2), (0, 1)),
        Some((ControlPinPosition::Right, _)) => ((2, 2), (0, 0)),
        Some((ControlPinPosition::Behind, _)) => ((3, 1), (1, 0)),
    };
    let size = vec2(size.0 as f32, size.1 as f32);
    let transformer = |p: Pos2| {
        ctx.rect.lerp_inside(
            Vec2f::from(p.to_vec2() / size)
                .rotated_xy(angle, 0.5)
                .into(),
        )
    };

    let scale = ctx.screen.scale;
    let center_off = vec2(center.0 as f32, center.1 as f32);
    let center = transformer(pos2(center.0 as f32 + 0.5, center.1 as f32 + 0.5));

    ctx.paint.line_segment(
        [
            transformer(pos2(0.95, 0.5) + center_off),
            transformer(pos2(1.5, 0.5) + center_off),
        ],
        Stroke::new(CircuitBoardEditor::WIRE_THICKNESS * scale, color),
    );

    if let Some(is_pico) = is_pico {
        let points = if is_pico {
            [pos2(1.12, 0.75), pos2(1.42, 0.5), pos2(1.12, 0.25)] // Right
        } else {
            [pos2(1.3, 0.75), pos2(1.0, 0.5), pos2(1.3, 0.25)] // Left
        };
        let points = points
            .into_iter()
            .map(|p| transformer(p + center_off))
            .collect();
        ctx.paint.add(PathShape {
            points,
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        });
    }
    ctx.paint
        .circle_filled(center, 0.3 * scale, outside_color);
    ctx.paint.circle_stroke(
        center,
        0.45 * scale,
        Stroke::new(0.1 * scale, color),
    );

    if let Some((ctl_dir, ctl_state)) = ctl {
        let ctl_angle = match ctl_dir {
            ControlPinPosition::Left => TAU * 0.5,
            ControlPinPosition::Behind => TAU * 0.25,
            ControlPinPosition::Right => 0.0,
        };

        let ctl_transformer = |p: Pos2| {
            transformer(Pos2::from(Vec2f::from(p).rotated_xy(ctl_angle, 0.5)) + center_off)
        };

        ctx.paint.line_segment(
            [
                ctl_transformer(pos2(0.5, 1.5)),
                ctl_transformer(pos2(0.5, 1.24)),
            ],
            Stroke::new(
                CircuitBoardEditor::WIRE_THICKNESS * scale,
                ctl_state,
            ),
        );

        ctx.paint.add(PathShape {
            points: vec![
                ctl_transformer(pos2(0.25, 1.25)),
                ctl_transformer(pos2(0.5, 1.0)),
                ctl_transformer(pos2(0.75, 1.25)),
            ],
            closed: true,
            fill: ctl_state,
            stroke: Stroke::NONE,
        });
    }

    center
}

/// size: 1.15x1
pub fn outside_pin(color: Color32, directional: bool, pico: Option<bool>, angle: f32, ctx: &PaintContext) {
    let size = vec2(if directional { 1.15 } else { 1.0 }, 1.0);
    let transformer = |p: Pos2| {
        ctx.rect.lerp_inside(
            Vec2f::from(p.to_vec2() / size)
                .rotated_xy(angle, 0.5)
                .into(),
        )
    };
    let scaled_thickness = CircuitBoardEditor::WIRE_THICKNESS * ctx.screen.scale;

    ctx.paint
        .circle_filled(transformer(pos2(0.5, 0.5)), ctx.screen.scale * 0.25, color);

    if !directional {
        return;
    }
    
    ctx.paint.line_segment(
        [transformer(pos2(0.5, 0.5)), transformer(pos2(1.1, 0.5))],
        Stroke::new(scaled_thickness, color),
    );
    ctx.paint
        .circle_filled(transformer(pos2(1.1, 0.5)), scaled_thickness * 0.5, color);

    if let Some(pico) = pico {
        let points = if pico {
            [pos2(1.1, 0.2), pos2(0.8, 0.5), pos2(1.1, 0.8)]
        } else {
            [pos2(0.85, 0.2), pos2(1.15, 0.5), pos2(0.85, 0.8)]
        };

        let points = points.into_iter().map(transformer).collect();

        ctx.paint.add(PathShape {
            points,
            closed: true,
            fill: color,
            stroke: Stroke::NONE,
        });
    }
}
