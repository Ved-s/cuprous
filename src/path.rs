use bezier_nd::{Bezier, BezierPointIter};
use num_traits::Zero;

use crate::vector::Vec2f;

pub trait Path: Sized {
    type PointsIter: Iterator<Item = Vec2f>;

    fn line_to(self, x: f32, y: f32) -> PointLine<Self> {
        PointLine {
            before: self,
            to: Vec2f::new(x, y),
        }
    }

    fn quadratic_bezier(
        self,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        straightness: f32,
    ) -> BezierLine<Self> {
        BezierLine {
            before: self,
            points: [
                Vec2f::new(x1, y1),
                Vec2f::new(x2, y2),
                Vec2f::zero()
            ],
            quadratic: true,
            straightness,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn cubic_bezier(
        self,
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        x3: f32,
        y3: f32,
        straightness: f32,
    ) -> BezierLine<Self> {
        BezierLine {
            before: self,
            points: [
                Vec2f::new(x1, y1),
                Vec2f::new(x2, y2),
                Vec2f::new(x3, y3),
            ],
            quadratic: true,
            straightness,
        }
    }

    fn iter_points(&self, pre_transform: impl FnMut(Vec2f) -> Vec2f) -> Self::PointsIter;
}

pub struct PointPath {
    start: Vec2f,
}

impl PointPath {
    pub fn new(x: f32, y: f32) -> Self {
        Self {
            start: Vec2f::new(x, y),
        }
    }
}

impl Path for PointPath {
    type PointsIter = std::option::IntoIter<Vec2f>;

    fn iter_points(&self, mut pre_transform: impl FnMut(Vec2f) -> Vec2f) -> Self::PointsIter {
        Some(pre_transform(self.start)).into_iter()
    }
}

pub struct PointLine<P: Path> {
    before: P,
    to: Vec2f,
}

impl<P: Path> Path for PointLine<P> {
    type PointsIter = std::iter::Chain<P::PointsIter, std::option::IntoIter<Vec2f>>;

    fn iter_points(&self, mut pre_transform: impl FnMut(Vec2f) -> Vec2f) -> Self::PointsIter {
        self.before
            .iter_points(&mut pre_transform)
            .chain(Some(pre_transform(self.to)))
    }
}

pub struct BezierLine<P: Path> {
    before: P,
    quadratic: bool,
    points: [Vec2f; 3],
    straightness: f32,
}

pub enum BezierLineIter<I: Iterator<Item = Vec2f>> {
    Before {
        before: I,
        last_point: Option<Vec2f>,
        quadratic: bool,
        points: [Vec2f; 3],
        straightness: f32,
    },
    Bezier(BezierPointIter<f32, Vec2f, 2>),
    Done,
}

impl<P: Path> Path for BezierLine<P> {
    type PointsIter = BezierLineIter<P::PointsIter>;

    fn iter_points(&self, mut pre_transform: impl FnMut(Vec2f) -> Vec2f) -> Self::PointsIter {
        let points = std::array::from_fn(|i| {
            if i < 2 || !self.quadratic {
                pre_transform(self.points[i])
            } else {
                Vec2f::zero()
            }
        });

        BezierLineIter::Before {
            before: self.before.iter_points(pre_transform),
            last_point: None,
            points,
            quadratic: self.quadratic,
            straightness: self.straightness,
        }
    }
}

impl<I: Iterator<Item = Vec2f>> Iterator for BezierLineIter<I> {
    type Item = Vec2f;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            BezierLineIter::Before {
                before,
                last_point,
                quadratic,
                points,
                straightness,
            } => match before.next() {
                Some(v) => {
                    *last_point = Some(v);
                    return Some(v);
                }
                None => {
                    let Some(a) = last_point else {
                        *self = Self::Done;
                        return None;
                    };

                    let bez = if *quadratic {
                        Bezier::quadratic(&*a, &points[0], &points[1])
                    } else {
                        Bezier::cubic(&*a, &points[0], &points[1], &points[2])
                    };

                    let mut points = bez.as_points(*straightness);
                    points.next();
                    let res = points.next();
                    *self = Self::Bezier(points);

                    res
                }
            },
            BezierLineIter::Bezier(bez) => {
                let point = bez.next();
                if point.is_none() {
                    *self = Self::Done;
                }
                point
            }
            BezierLineIter::Done => None,
        }
    }
}
