use std::{
    f32::consts::{FRAC_PI_2, TAU},
    ops::RangeInclusive,
};

use eframe::{
    egui::{Color32, Rect},
    epaint::{CornerRadiusF32, PathShape, PathStroke},
};

use crate::{
    Direction8, PinStyle,
    vector::Vec2f,
    vertex_renderer::{
        ColoredTriangleBuffer, ColoredVertex, PositionedVertex, Triangle, TriangleBuffer,
    },
};

#[derive(Clone, Copy)]
// ax + by + c = 0
struct Line {
    a: f32,
    b: f32,
    c: f32,
}

impl Line {
    pub fn from_two_points(a: Vec2f, b: Vec2f) -> Self {
        Self {
            a: a.y - b.y,
            b: b.x - a.x,
            c: a.x * b.y - a.y * b.x,
        }
    }

    pub fn intersect(self, other: Self) -> Vec2f {
        fn det(a: f32, b: f32, c: f32, d: f32) -> f32 {
            a * d - b * c
        }

        let zn = det(self.a, self.b, other.a, other.b);
        let x = -det(self.c, self.b, other.c, other.b) / zn;
        let y = -det(self.a, self.c, other.a, other.c) / zn;

        Vec2f::new(x, y)
    }
}

struct ThickOutlinePointsIterator<I: Iterator<Item = Vec2f>> {
    prev: Vec2f,
    cur: Vec2f,
    next: Option<Vec2f>,
    first: bool,

    points: I,
    last_outline_point: Vec2f,
    halfwidth: f32,
    positive: bool,
    done: bool,
}

impl<I: Iterator<Item = Vec2f>> ThickOutlinePointsIterator<I> {
    pub fn new(mut points: I, width: f32, positive: bool) -> Option<Self> {
        let first = points.next()?;
        let second = points.next()?;

        let pnfrac = if positive { FRAC_PI_2 } else { -FRAC_PI_2 };

        let lop = first + Vec2f::from_angle_length((second - first).angle() + pnfrac, width / 2.0);

        Some(Self {
            prev: first,
            cur: second,
            next: points.next(),
            first: true,
            points,
            last_outline_point: lop,
            halfwidth: width / 2.0,
            positive,
            done: false,
        })
    }
}

impl<I: Iterator<Item = Vec2f>> Iterator for ThickOutlinePointsIterator<I> {
    type Item = Vec2f;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        if self.first {
            self.first = false;
            return Some(self.last_outline_point);
        }

        let pnfrac = if self.positive { FRAC_PI_2 } else { -FRAC_PI_2 };

        match self.next {
            Some(next) => {
                let prev_angle = (self.cur - self.prev).angle_to_xp();
                let next_angle = (next - self.cur).angle_to_xp();

                let prev_offset = Vec2f::from_angle_length(prev_angle + pnfrac, self.halfwidth);
                let prev_line =
                    Line::from_two_points(self.cur + prev_offset, self.prev + prev_offset);

                let next_offset = Vec2f::from_angle_length(next_angle + pnfrac, self.halfwidth);
                let next_line = Line::from_two_points(self.cur + next_offset, next + next_offset);

                let intersect = prev_line.intersect(next_line);

                // let intersect = if intersect.x.is_nan() || intersect.y.is_nan() {
                //     prev_line_pos
                // } else {
                //     intersect
                // };

                self.last_outline_point = intersect;

                self.prev = self.cur;
                self.cur = next;
                self.next = self.points.next();

                Some(intersect)
            }
            None => {
                let fin = self.cur
                    + Vec2f::from_angle_length(
                        (self.cur - self.prev).angle() + pnfrac,
                        self.halfwidth,
                    );
                self.done = true;

                Some(fin)
            }
        }
    }
}

pub fn rotated_rect(
    rect: Rect,
    origin: Vec2f,
    angle: f32,
    rounding: CornerRadiusF32,
    fill: Color32,
    stroke: PathStroke,
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

            let dir_angle = dir
                .filter(|_| *directional)
                .map(|d| d.into_angle_xp_cw())
                .unwrap_or(0.0);
            let angle = angle + dir_angle;
            let one_angle = TAU / *n as f32;

            let length = radius / (one_angle / 2.0).cos();

            let iter = (0..*n).map(|i| {
                let angle = angle + one_angle * i as f32;
                let vec = pos + Vec2f::from_angle_length(angle, length);
                ColoredVertex::new(vec, color.to_normalized_gamma_f32())
            });

            buffer.add_filled_polygon(iter);
        }
    }
}

pub fn path<V: PositionedVertex>(
    buf: &mut TriangleBuffer<V>,
    points: impl Iterator<Item = V> + Clone,
    width: f32,
) {
    let piter =
        ThickOutlinePointsIterator::new(points.clone().map(|v| v.into_parts().0), width, true)
            .unwrap()
            .peekable();
    let niter =
        ThickOutlinePointsIterator::new(points.clone().map(|v| v.into_parts().0), width, false)
            .unwrap()
            .peekable();

    let mut prev_verts = None::<(V, V)>;

    for ((p, n), v) in piter.zip(niter).zip(points) {
        let v = v.into_parts().1;
        let p = V::new(p, v);
        let n = V::new(n, v);

        if let Some((prevp, prevn)) = prev_verts {
            buf.push_triangle(Triangle([prevp, prevn, p]));
            buf.push_triangle(Triangle([prevn, p, n]));
        }

        prev_verts = Some((p, n));
    }
}

pub fn generate_circle_points(
    count: usize,
    angle_range: Option<RangeInclusive<f32>>,
) -> impl Iterator<Item = Vec2f> {
    let (start, end, include_end) = match angle_range {
        Some(range) => (*range.start(), *range.end(), true),
        None => (0.0, TAU, false),
    };

    let points_excluding_end = if include_end {
        count.saturating_sub(1)
    } else {
        count
    };

    let step_size = (end - start) / points_excluding_end as f32;

    (0..count).map(move |i| {
        let angle = if i > 0 {
            if i == points_excluding_end {
                end
            } else {
                step_size * i as f32 + start
            }
        } else {
            start
        };

        Vec2f::new(angle.cos(), angle.sin())
    })
}

pub fn donut_segment<V: PositionedVertex>(
    buf: &mut TriangleBuffer<V>,
    center: Vec2f,
    radius1: f32,
    radius2: f32,
    angle_range: RangeInclusive<f32>,
    extra_data: V::ExtraData,
) {
    let max_rad = radius1.max(radius2);
    let count = if max_rad <= 2.0 {
        8
    } else if max_rad <= 5.0 {
        16
    } else if max_rad < 18.0 {
        32
    } else if max_rad < 50.0 {
        64
    } else {
        128
    };

    let circle = generate_circle_points(count, Some(angle_range));
    let donut_points = circle.flat_map(|c| [c * radius1 + center, c * radius2 + center]);
    let verts = donut_points.map(|p| V::new(p, extra_data));
    buf.add_triangle_strip(verts);
}
