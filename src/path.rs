use eframe::epaint::{Color32, PathShape, Stroke};
use emath::{pos2, Pos2, Vec2};

use crate::vector::Vec2f;

#[derive(Debug, Clone, Copy)]
pub enum PathItem {
    MoveTo(Pos2),
    LineTo(Pos2),
    QuadraticBezier(Pos2, Pos2),
    CubicBezier(Pos2, Pos2, Pos2),
    ClosePath,
}

pub trait PathItemIterator: Iterator<Item = PathItem> {
    fn create_path_shapes(
        &mut self,
        offset: Vec2,
        scale: Vec2,
        fill: impl Into<Color32>,
        stroke: impl Into<Stroke>,
        bezier_straightness: f32,
        consumer: impl Fn(usize, PathShape),
    ) {
        let fill = fill.into();
        let stroke = stroke.into();

        let mut pos = pos2(0., 0.);
        let mut points = vec![];
        let mut path_index = 0;

        for item in self {
            match item {
                PathItem::MoveTo(p) => {
                    let p = (offset + p.to_vec2() * scale).to_pos2();
                    if !points.is_empty() {
                        consumer(path_index, PathShape {
                            points,
                            closed: false,
                            fill,
                            stroke,
                        });
                        path_index += 1;
                        points = vec![];
                    }
                    pos = p;
                }
                PathItem::LineTo(p) => {
                    let p = (offset + p.to_vec2() * scale).to_pos2();
                    if points.is_empty() {
                        points.push(pos);
                    }
                    points.push(p);
                    pos = p;
                }
                PathItem::QuadraticBezier(a, b) => {
                    let a = offset + a.to_vec2() * scale;
                    let b = offset + b.to_vec2() * scale;
                    if points.is_empty() {
                        points.push(pos);
                    }
                    let points_empty = points.is_empty();
                    points.extend(
                        bezier_nd::Bezier::quadratic(
                            &Vec2f::from(pos),
                            &Vec2f::from(a),
                            &Vec2f::from(b),
                        )
                        .as_points(bezier_straightness)
                        .map(Pos2::from)
                        .enumerate()
                        // Remove first point if it's equal to current position
                        .filter(|(i, p)| *i > 0 || points_empty || *p != pos)
                        .map(|(_, p)| p),
                    );
                    pos = b.to_pos2();
                }
                PathItem::CubicBezier(a, b, c) => {
                    let a = offset + a.to_vec2() * scale;
                    let b = offset + b.to_vec2() * scale;
                    let c = offset + c.to_vec2() * scale;
                    if points.is_empty() {
                        points.push(pos);
                    }
                    points.extend(
                        bezier_nd::Bezier::cubic(
                            &Vec2f::from(pos),
                            &Vec2f::from(a),
                            &Vec2f::from(b),
                            &Vec2f::from(c),
                        )
                        .as_points(bezier_straightness)
                        .map(Pos2::from)
                        .enumerate()
                        // Remove first point if it's equal to current position
                        .filter(|(i, p)| *i > 0 || *p != pos)
                        .map(|(_, p)| p),
                    );
                    pos = b.to_pos2();
                }
                PathItem::ClosePath => {
                    if !points.is_empty() {
                        pos = points[0];
                        consumer(path_index, PathShape {
                            points,
                            closed: true,
                            fill,
                            stroke,
                        });
                        path_index += 1;
                        points = vec![];
                    }
                }
            }
        }

        if !points.is_empty() {
            consumer(path_index, PathShape {
                points,
                closed: false,
                fill,
                stroke,
            });
        }
    }
}

impl<I: Iterator<Item = PathItem>> PathItemIterator for I {}
