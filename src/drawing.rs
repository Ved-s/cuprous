use eframe::{
    egui::{Color32, Rect, Rounding, Stroke},
    epaint::PathShape,
};

use crate::vector::Vec2f;

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
