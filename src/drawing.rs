use std::f32::consts::TAU;

use eframe::{
    egui::{Color32, Rect, Rounding, Stroke},
    epaint::PathShape,
};

use crate::{vector::Vec2f, vertex_renderer::{ColoredTriangleBuffer, ColoredVertex}, Direction8, PinStyle};

pub fn rotated_rect(
    rect: Rect,
    origin: Vec2f,
    angle: f32,
    rounding: Rounding,
    fill: Color32,
    stroke: Stroke,
) -> PathShape {
    let mut points = vec![];
    eframe::epaint::tessellator::path::rounded_rectangle(&mut points, rect, rounding);

    for p in &mut points {
        *p = Vec2f::from(*p).rotated(angle, origin).into()
    }

    PathShape {
        points,
        closed: true,
        fill,
        stroke,
    }
}

pub fn pin(
    pos: Vec2f,
    radius: f32,
    style: &PinStyle,
    dir: Option<Direction8>,
    color: Color32,
    buffer: &mut ColoredTriangleBuffer,
) {
    match style {
        PinStyle::Circle => buffer.add_circle(pos, radius, color.to_normalized_gamma_f32()),
        PinStyle::NGon {
            n,
            angle,
            directional,
        } => {
            if *n < 3 {
                return;
            }

            let dir_angle = dir.filter(|_| *directional).map(|d| d.into_angle_xp_cw()).unwrap_or(0.0);
            let angle = angle + dir_angle;
            let one_angle = TAU / *n as f32;

            let length = radius / (one_angle / 2.0).cos();

            let iter = (0..*n).map(|i| {
                let angle = angle + one_angle * i as f32;
                let vec = pos + Vec2f::from_angle_length(angle, length);
                ColoredVertex::new(vec, color.to_normalized_gamma_f32())
            });

            buffer.add_filled_path(iter);
        }
    }
}
