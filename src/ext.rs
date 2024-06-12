use std::ops::Range;

pub trait Intersect {
    fn intersect(&self, other: &Self) -> Self;
}

impl<T: Ord + Copy> Intersect for Range<T> {
    fn intersect(&self, other: &Self) -> Self {
        Self {
            start: self.start.max(other.start),
            end: self.end.min(other.end),
        }
    }
}

pub trait IteratorProduct: Iterator + Sized {
    fn product_clone<I>(self, other: I) -> impl Iterator<Item = (Self::Item, I::Item)>
    where
        I: Iterator + Clone,
        Self::Item: Clone,
    {
        self.flat_map(move |v| other.clone().map(move |o| (v.clone(), o)))
    }
}

impl<T: Iterator> IteratorProduct for T {} 