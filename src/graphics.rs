use std::f32::consts::TAU;

use eframe::{
    egui::Rounding,
    epaint::{Color32, CubicBezierShape, PathShape, Stroke},
};
use emath::{pos2, vec2, Pos2, Rect};

use crate::{
    circuits::{pin::ControlPinPosition, transistor::TransistorType},
    ui::editor::CircuitBoardEditor,
    vector::Vec2f,
    Direction4, PaintContext,
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
    ctx.paint.circle_filled(center, 0.3 * scale, outside_color);
    ctx.paint
        .circle_stroke(center, 0.45 * scale, Stroke::new(0.1 * scale, color));

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
            Stroke::new(CircuitBoardEditor::WIRE_THICKNESS * scale, ctl_state),
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
pub fn outside_pin(
    color: Color32,
    directional: bool,
    pico: Option<bool>,
    angle: f32,
    ctx: &PaintContext,
) {
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

pub struct RelaySingleThrowColors {
    pub input: Color32,
    pub output: Color32,
}

pub struct RelayDoubleThrowColors {
    pub open: Color32,
    pub closed: Color32,
    pub center: Color32,
}

pub enum RelayGraphicsMode<'a> {
    SingleThrow(&'a mut dyn ExactSizeIterator<Item = RelaySingleThrowColors>),
    DoubleThrow {
        alternate: bool,
        groups: &'a mut dyn ExactSizeIterator<Item = RelayDoubleThrowColors>,
    },
}

pub fn relay(
    coil: Color32,
    closed: bool,
    mode: RelayGraphicsMode,
    flip: bool,
    angle: f32,
    coil_pin_dir: Direction4,
    ctx: &PaintContext,
) {
    let width = match &mode {
        RelayGraphicsMode::SingleThrow(g) => {
            crate::circuits::relay::calc_relay_width(false, g.len(), false)
        }
        RelayGraphicsMode::DoubleThrow { alternate, groups } => {
            crate::circuits::relay::calc_relay_width(true, groups.len(), *alternate)
        }
    };
    let fwidth = width as f32;
    let trf = |x: f32, y: f32| {
        let y = y / 3.0;
        let y = if flip { 1.0 - y } else { y };
        ctx.rect
            .lerp_inside(Vec2f::from([x / fwidth, y]).rotated_xy(angle, 0.5).into())
    };

    let coil_stroke = Stroke::new(0.1 * ctx.screen.scale, coil);

    // ctx.paint
    //     .circle_filled(trf(0.5, 1.5), 0.1 * ctx.screen.scale, coil);

    let rect = Rect::from_two_pos(trf(0.5, 1.4), trf(0.71, 1.6));
    let rounding = ctx.screen.scale * 0.1;
    let rounding = Rounding {
        nw: if matches!(coil_pin_dir, Direction4::Down | Direction4::Right) {
            rounding
        } else {
            0.0
        },
        ne: if matches!(coil_pin_dir, Direction4::Down | Direction4::Left) {
            rounding
        } else {
            0.0
        },
        sw: if matches!(coil_pin_dir, Direction4::Up | Direction4::Right) {
            rounding
        } else {
            0.0
        },
        se: if matches!(coil_pin_dir, Direction4::Up | Direction4::Left) {
            rounding
        } else {
            0.0
        }
    };
    ctx.paint.rect_filled(rect, rounding, coil);

    ctx.paint
        .line_segment([trf(0.5, 1.5), trf(0.81, 1.5)], coil_stroke);

    ctx.paint
        .circle_filled(trf(0.8, 0.75), 0.05 * ctx.screen.scale, coil);
    ctx.paint
        .circle_filled(trf(0.8, 1.125), 0.05 * ctx.screen.scale, coil);
    ctx.paint
        .circle_filled(trf(0.8, 1.875), 0.05 * ctx.screen.scale, coil);
    ctx.paint
        .circle_filled(trf(0.8, 2.25), 0.05 * ctx.screen.scale, coil);

    ctx.paint.add(CubicBezierShape {
        points: [
            trf(0.8, 2.25),
            trf(1.185, 2.25),
            trf(1.185, 1.875),
            trf(0.8, 1.875),
        ],
        closed: false,
        fill: Color32::TRANSPARENT,
        stroke: coil_stroke,
    });

    ctx.paint.add(CubicBezierShape {
        points: [
            trf(0.8, 1.875),
            trf(1.185, 1.875),
            trf(1.185, 1.5),
            trf(0.8, 1.5),
        ],
        closed: false,
        fill: Color32::TRANSPARENT,
        stroke: coil_stroke,
    });

    ctx.paint.add(CubicBezierShape {
        points: [
            trf(0.8, 1.5),
            trf(1.185, 1.5),
            trf(1.185, 1.125),
            trf(0.8, 1.125),
        ],
        closed: false,
        fill: Color32::TRANSPARENT,
        stroke: coil_stroke,
    });

    ctx.paint.add(CubicBezierShape {
        points: [
            trf(0.8, 1.125),
            trf(1.185, 1.125),
            trf(1.185, 0.75),
            trf(0.8, 0.75),
        ],
        closed: false,
        fill: Color32::TRANSPARENT,
        stroke: coil_stroke,
    });

    let rect_width = match &mode {
        RelayGraphicsMode::SingleThrow(g) => 1.0 + g.len() as f32,
        RelayGraphicsMode::DoubleThrow { alternate, groups } => {
            crate::circuits::relay::calc_relay_width(true, groups.len(), *alternate) as f32 - 0.5
        }
    };

    match mode {
        RelayGraphicsMode::SingleThrow(groups) => {
            for (i, g) in groups.enumerate() {
                let offset = i as f32;

                ctx.paint
                    .circle_filled(trf(1.5 + offset, 1.0), 0.12 * ctx.screen.scale, g.output);
                ctx.paint
                    .circle_filled(trf(1.5 + offset, 2.0), 0.12 * ctx.screen.scale, g.input);

                let input_stroke = Stroke::new(0.2 * ctx.screen.scale, g.input);
                let switch_stroke = Stroke::new(0.16 * ctx.screen.scale, g.input);
                let output_stroke = Stroke::new(0.2 * ctx.screen.scale, g.output);

                ctx.paint.line_segment(
                    [trf(1.5 + offset, 0.5), trf(1.5 + offset, 1.0)],
                    output_stroke,
                );
                ctx.paint.line_segment(
                    [trf(1.5 + offset, 2.5), trf(1.5 + offset, 2.0)],
                    input_stroke,
                );

                if closed {
                    ctx.paint.line_segment(
                        [trf(1.5 + offset, 2.0), trf(1.5 + offset, 1.0)],
                        switch_stroke,
                    );
                } else {
                    ctx.paint.line_segment(
                        [trf(1.5 + offset, 2.0), trf(2.0 + offset, 1.25)],
                        switch_stroke,
                    );
                }
            }
        }
        RelayGraphicsMode::DoubleThrow { alternate, groups } => {
            let len = groups.len();
            for (i, g) in groups.enumerate() {
                let trf = |x: f32, y: f32| {
                    if alternate {
                        if i % 2 == 0 {
                            trf(1.0 + x + (i / 2) as f32 * 3.0, y)
                        } else {
                            trf(2.0 + (2.0 - x) + (i / 2) as f32 * 3.0, 3.0 - y)
                        }
                    } else {
                        trf(1.0 + x + i as f32 * 2.0, y)
                    }
                };

                let cyo = if !alternate || len == 1 { 0.0 } else { -0.125 };

                let (closed_color, open_color) = if alternate && i % 2 == 1 {
                    (g.open, g.closed) // Since x is mirrored, mirror these back
                } else {
                    (g.closed, g.open)
                };

                ctx.paint.circle_filled(
                    trf(0.5, 1.125 + cyo),
                    0.12 * ctx.screen.scale,
                    closed_color,
                );
                ctx.paint
                    .circle_filled(trf(1.5, 1.125 + cyo), 0.12 * ctx.screen.scale, open_color);
                ctx.paint
                    .circle_filled(trf(1.0, 1.875 + cyo), 0.12 * ctx.screen.scale, g.center);

                let closed_wire_stroke = Stroke::new(0.2 * ctx.screen.scale, closed_color);
                let open_wire_stroke = Stroke::new(0.2 * ctx.screen.scale, open_color);
                let switch_stroke = Stroke::new(0.16 * ctx.screen.scale, g.center);

                ctx.paint
                    .line_segment([trf(0.5, 0.5), trf(0.5, 1.125 + cyo)], closed_wire_stroke);
                ctx.paint
                    .line_segment([trf(1.5, 0.5), trf(1.5, 1.125 + cyo)], open_wire_stroke);

                let center_middle_wire_y =
                    ((1.875 + cyo + 2.5) * 0.5 * ctx.screen.scale).round() / ctx.screen.scale;

                let top_tl = trf(0.9, 1.875 + cyo);
                let middle_tl = trf(0.4, center_middle_wire_y - 0.1);
                let middle_br = trf(1.1, center_middle_wire_y + 0.1);
                let bottom_br = trf(0.6, 2.5);

                let top_rect = Rect::from_two_pos(top_tl, middle_br);
                let middle_rect = Rect::from_two_pos(middle_tl, middle_br);
                let bottom_rect = Rect::from_two_pos(middle_tl, bottom_br);

                ctx.paint.rect_filled(top_rect, Rounding::ZERO, g.center);
                ctx.paint.rect_filled(middle_rect, Rounding::ZERO, g.center);
                ctx.paint.rect_filled(bottom_rect, Rounding::ZERO, g.center);

                if closed ^ (alternate && i % 2 == 1) {
                    ctx.paint.line_segment(
                        [trf(1.0, 1.875 + cyo), trf(0.5, 1.125 + cyo)],
                        switch_stroke,
                    );
                } else {
                    ctx.paint.line_segment(
                        [trf(1.0, 1.875 + cyo), trf(1.5, 1.125 + cyo)],
                        switch_stroke,
                    );
                }
            }
        }
    }

    let rect = Rect::from_two_pos(trf(0.5, 0.5), trf(rect_width + 0.5, 2.5));
    ctx.paint.rect_stroke(
        rect,
        Rounding::ZERO,
        Stroke::new(0.1 * ctx.screen.scale, Color32::BLACK),
    );
}
