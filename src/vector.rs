#![allow(dead_code)]

use std::{ops::{Add, Mul, Div}, fmt::Display, f32::consts::TAU};

use crate::{r#const::*, SizeCalc};

pub trait VectorValue: Sized + Copy + Default {}
impl<T> VectorValue for T where T: Sized + Copy + Default{}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct Vector<const SIZE: usize, T: VectorValue> (pub [T; SIZE]);

pub type Vec2f = Vector<2, f32>;
pub type Vec3f = Vector<3, f32>;

pub type Vec2d = Vector<2, f64>;
pub type Vec3d = Vector<3, f64>;

pub type Vec2i = Vector<2, i32>;
pub type Vec3i = Vector<3, i32>;

pub type Vec2u = Vector<2, u32>;
pub type Vec3u = Vector<3, u32>;

pub type Vec2isize = Vector<2, isize>;
pub type Vec3isize = Vector<3, isize>;

pub type Vec2usize = Vector<2, usize>;
pub type Vec3usize = Vector<3, usize>;

impl<const SIZE: usize, T: VectorValue> Vector<SIZE, T> {
    pub const fn single_value(value: T) -> Self {
        Self([value; SIZE])
    }

    pub fn length_squared(&self) -> T where T: Add<Output = T> + Mul<Output = T> + Default {
        let mut len_sq = T::default();

        for i in 0..SIZE {
            let val = self.0[i];
            len_sq = len_sq + val * val;
        }

        len_sq
    }

    pub fn length(&self) -> T where T: Add<Output = T> + Mul<Output = T> + Default + FloatMath {
        self.length_squared().sqrt()
    }

    pub fn at<const INDEX: usize>(&self) -> T where Bool<{SIZE > INDEX}>: True {
        self.0[INDEX]
    }

    pub fn with<const INDEX: usize>(&self, value: T) -> Self where Bool<{SIZE > INDEX}>: True {
        let mut c = *self;
        c.0[INDEX] = value;
        c
    }

    pub fn into_type<I>(self) -> Vector<SIZE, I> where T: Into<I>, I: VectorValue {
        let mut v = [I::default(); SIZE];
        (0..SIZE).for_each(|i| {
            v[i] = self.0[i].into();
        });
        v.into()
    }

    pub fn convert<I: VectorValue>(&self, converter: impl Fn(T) -> I) -> Vector<SIZE, I> {
        let mut v = [I::default(); SIZE];
        (0..SIZE).for_each(|i| {
            v[i] = converter(self.0[i]);
        });
        v.into()
    }

    pub fn combine_with<I: VectorValue, O: VectorValue>(&self, other: Vector<SIZE, I>, combiner: impl Fn(T, I) -> O) -> Vector<SIZE, O> {
        let mut v = [O::default(); SIZE];
        (0..SIZE).for_each(|i| {
            v[i] = combiner(self.0[i], other.0[i]);
        });
        v.into()
    }
}

impl<const SIZE: usize, T: VectorValue + FloatMath + Add<Output = T> + Mul<Output = T> + Div<Output = T>> Vector<SIZE, T> {
    pub fn angle_to(&self, other: Self) -> T {
        let mut a = T::default();
        for i in 0..SIZE {
            a = a + self.0[i] * other.0[i];
        }
        let b = self.length() * other.length();
        (a/b).acos()
    }
}

macro_rules! impl_vec_component {
    ($name:ident, $index:literal) => {
        impl<const SIZE: usize, T: VectorValue> Vector<SIZE, T> where Bool<{SIZE > $index}>: True {
            #[inline]
            pub fn $name(&self) -> T {
                self.0[$index]
            }
            
            paste::paste! {
                #[inline]
                pub fn [< with_ $name >](&self, x: T) -> Self {
                    let mut n = self.clone();
                    n.0[$index] = x;
                    n
                }

                #[inline]
                pub fn [< $name _mut >](&mut self) -> &mut T {
                    &mut self.0[$index]
                }
            }


        }
    };
}

impl_vec_component!(x, 0);
impl_vec_component!(y, 1);
impl_vec_component!(z, 2);
impl_vec_component!(w, 3);

impl<const SIZE: usize, T: VectorValue> Default for Vector<SIZE, T> {
    fn default() -> Self {
        Self([Default::default(); SIZE])
    }
}

impl<const SIZE: usize, T: VectorValue + SizeCalc> SizeCalc for Vector<SIZE, T> {
    fn calc_size_inner(&self) -> usize {
        (0..SIZE).map(|i| self.0[i].calc_size_inner() ).sum()
    }
}

impl<const SIZE: usize, T: VectorValue + std::fmt::Display> std::fmt::Display for Vector<SIZE, T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        for i in 0..SIZE {
            if i > 0 {
                f.write_str(", ")?;
            }
            self.0[i].fmt(f)?;
        }
        f.write_str(" }")
    }
}


macro_rules! impl_op {
    ($trait:ident, $fn:ident, $op:tt) => {
        impl<const SIZE: usize, T: VectorValue + std::ops::$trait<Output = O>, O: VectorValue, R: Into<Vector<SIZE, T>>> std::ops::$trait<R> for Vector<SIZE, T> {
            type Output = Vector::<SIZE, O>;
        
            fn $fn(self, rhs: R) -> Self::Output {
                let mut res = Vector::<SIZE, O>::default();
                let rhs_v = rhs.into();
                for i in 0..SIZE {
                    res.0[i] = self.0[i] $op rhs_v.0[i]
                }
                res
            }
        }
        paste::paste!{
            impl<const SIZE: usize, T: VectorValue + std::ops::$trait<Output = T>, R: Into<Vector<SIZE, T>>>  std::ops::[<$trait Assign>]<R> for Vector<SIZE, T> {
                fn [<$fn _assign>](&mut self, rhs: R) {
                    let rhs_v = rhs.into();
                    for i in 0..SIZE {
                        self.0[i] = self.0[i] $op rhs_v.0[i]
                    }
                }
            }
        }
    };
}

impl_op!(Add, add, +);
impl_op!(Sub, sub, -);
impl_op!(Mul, mul, *);
impl_op!(Div, div, /);
impl_op!(Rem, rem, %);

impl<const SIZE: usize, T: VectorValue + std::ops::Neg<Output = O>, O: VectorValue> std::ops::Neg for Vector<SIZE, T> {
    type Output = Vector::<SIZE, O>;

    fn neg(self) -> Self::Output {
        let mut res = Vector::<SIZE, O>::default();
        for i in 0..SIZE {
            res.0[i] = -self.0[i];
        }
        res
    }
}

impl<const SIZE: usize, T: VectorValue> From<T> for Vector<SIZE, T> {
    fn from(value: T) -> Self {
        Self::single_value(value)
    }
}

impl<const SIZE: usize, T: VectorValue> From<[T; SIZE]> for Vector<SIZE, T> {
    fn from(value: [T; SIZE]) -> Self {
        Self(value)
    }
}

impl<const SIZE: usize, T: VectorValue> From<Vector<SIZE, T>> for [T; SIZE] {
    fn from(val: Vector<SIZE, T>) -> Self {
        val.0
    }
}

macro_rules! float_math_trait {
    ($($fnname:ident),+) => {
        pub trait FloatMath {
            $(fn $fnname(self) -> Self;)+
        }

        impl FloatMath for f32 {
            $(fn $fnname(self) -> Self {
                self.$fnname()
            })+
        }

        impl FloatMath for f64 {
            $(fn $fnname(self) -> Self {
                self.$fnname()
            })+
        }
    };
}

float_math_trait!(sqrt, sin, cos, asin, acos);

impl Vector<2, f32> {

    /// 0 -> 2PI countercloclwise from +X axis
    pub fn angle_to_x(&self) -> f32 {
        let v = (-self.y()).atan2(self.x());
        if v < 0.0 {
            TAU + v
        }
        else {
            v.abs() // remove weird -0.0
        }
    }
}

impl From<Vec2f> for emath::Pos2 {
    fn from(val: Vec2f) -> Self {
        emath::pos2(val.x(), val.y())
    }
}

impl From<Vec2f> for emath::Vec2 {
    fn from(val: Vec2f) -> Self {
        emath::vec2(val.x(), val.y())
    }
}

impl From<emath::Pos2> for Vec2f {
    fn from(value: emath::Pos2) -> Self {
        Self([value.x, value.y])
    }
}

impl From<emath::Vec2> for Vec2f {
    fn from(value: emath::Vec2) -> Self {
        Self([value.x, value.y])
    }
}

pub trait IsZero {
    fn is_zero(&self) -> bool;
}

macro_rules! impl_number_is_zero {
    ($zero:literal, $($t:ty),+) => {
        $(impl IsZero for $t {
            fn is_zero(&self) -> bool {
                *self == $zero
            }
        })+
    };
}

impl_number_is_zero!(0, u8, i8, u16, i16, u32, i32, u128, i128, usize, isize);
impl_number_is_zero!(0.0, f32, f64);

impl<const SIZE: usize, T: VectorValue + IsZero> IsZero for Vector<SIZE, T> {
    fn is_zero(&self) -> bool {
        for i in 0..SIZE {
            if !self.0[i].is_zero() {
                return false;
            }
        }
        true
    }
}