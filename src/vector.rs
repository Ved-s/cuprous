#![allow(dead_code)]

use std::{
    fmt::Display,
    ops::{Add, Mul}, marker::PhantomData,
};

use num_traits::{Float, FloatConst, Zero};
use serde::{de::Visitor, Deserialize};

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash, Default)]
pub struct Vector2<T> {
    pub x: T,
    pub y: T,
}

pub type Vec2f = Vector2<f32>;
pub type Vec2d = Vector2<f64>;
pub type Vec2i = Vector2<i32>;
pub type Vec2u = Vector2<u32>;
pub type Vec2isize = Vector2<isize>;
pub type Vec2usize = Vector2<usize>;

impl<T> Vector2<T> {
    pub const fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    pub fn from_angle_length(angle: T, length: T) -> Self
    where
        T: Float
    {
        Self::new(angle.cos() * length, angle.sin() * length)
    }

    pub const fn single_value(value: T) -> Self
    where
        T: Copy,
    {
        Self::new(value, value)
    }

    pub fn length_squared(self) -> T
    where
        T: Add<Output = T> + Mul<Output = T> + Copy,
    {
        self.x * self.x + self.y * self.y
    }

    pub fn length(self) -> T
    where
        T: Float,
    {
        self.length_squared().sqrt()
    }

    pub fn set_length(&mut self, length: T)
    where 
        T: Float + FloatConst
    {
        let angle = self.angle();
        *self = Self::from_angle_length(angle, length);
    }

    pub fn with_length(self, length: T)
    where 
        T: Float + FloatConst
    {
        let angle = self.angle();
        Self::from_angle_length(angle, length);
    }

     /// 0 -> 2PI cloclwise from +X axis (default for operations)
    pub fn angle(self) -> T
    where 
        T: Float + FloatConst
    {
        let a = self.y.atan2(self.x);
        if a.is_sign_negative() {
            a + T::TAU()
        } else {
            a
        }
    }

    pub fn with_x(self, x: T) -> Self {
        Self { x, ..self }
    }

    pub fn with_y(self, y: T) -> Self {
        Self { y, ..self }
    }

    pub fn into_type<I>(self) -> Vector2<I>
    where
        T: Into<I>,
    {
        Vector2::new(self.x.into(), self.y.into())
    }

    pub fn convert<I>(self, converter: impl Fn(T) -> I) -> Vector2<I> {
        Vector2::new(converter(self.x), converter(self.y))
    }

    pub fn combine_with<I, O>(self, other: Vector2<I>, combiner: impl Fn(T, I) -> O) -> Vector2<O> {
        Vector2::new(combiner(self.x, other.x), combiner(self.y, other.y))
    }

    pub fn angle_to(self, other: Self) -> T
    where
        T: Float,
    {
        let a = self.x * other.x + self.y * other.y;
        let b = self.length() * other.length();
        (a / b).acos()
    }

    pub fn rotated_xy(self, angle: T, origin: impl Into<Self>) -> Self
    where
        T: Float,
    {
        let origin = origin.into();
        let sin = angle.sin();
        let cos = angle.cos();
        let off = self - origin;

        let x = off.x * cos - off.y * sin;
        let y = off.x * sin + off.y * cos;

        Self::new(x, y) + origin
    }

    /// 0 -> 2PI countercloclwise from +X axis
    pub fn angle_to_x(&self) -> T
    where 
        T: Float + FloatConst
    {
        let v = (-self.y).atan2(self.x);

        if v.is_sign_negative() {
            T::TAU() + v
        } else {
            v.abs() // remove weird -0.0
        }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Vector2<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{{ {}, {} }}", self.x, self.y))
    }
}

macro_rules! impl_op {
    ($trait:ident, $fn:ident) => {
        impl<T: std::ops::$trait<Output = O>, O, R: Into<Vector2<T>>> std::ops::$trait<R> for Vector2<T> {
            type Output = Vector2::<O>;

            fn $fn(self, rhs: R) -> Self::Output {
                let rhs = rhs.into();
                Vector2::new(self.x.$fn(rhs.x), self.y.$fn(rhs.y))
            }
        }
        paste::paste!{
            impl<T: std::ops::[<$trait Assign>], R: Into<Vector2<T>>> std::ops::[<$trait Assign>]<R> for Vector2<T> {
                fn [<$fn _assign>](&mut self, rhs: R) {
                    let rhs = rhs.into();
                    self.x.[<$fn _assign>](rhs.x);
                    self.y.[<$fn _assign>](rhs.y);
                }
            }
        }
    };
}

impl_op!(Add, add);
impl_op!(Sub, sub);
impl_op!(Mul, mul);
impl_op!(Div, div);
impl_op!(Rem, rem);

impl<T: std::ops::Neg<Output = O>, O> std::ops::Neg
    for Vector2<T>
{
    type Output = Vector2<O>;

    fn neg(self) -> Self::Output {
        Vector2::new(-self.x, -self.y)
    }
}

impl<T: Copy> From<T> for Vector2<T> {
    fn from(value: T) -> Self {
        Self::single_value(value)
    }
}

impl<T> From<[T; 2]> for Vector2<T> {
    fn from(value: [T; 2]) -> Self {
        let [x, y] = value;
        Self::new(x, y)
    }
}

impl<T> From<Vector2<T>> for [T; 2] {
    fn from(val: Vector2<T>) -> Self {
        [val.x, val.y]
    }
}

impl From<Vec2f> for emath::Pos2 {
    fn from(val: Vec2f) -> Self {
        emath::pos2(val.x, val.y)
    }
}

impl From<Vec2f> for emath::Vec2 {
    fn from(val: Vec2f) -> Self {
        emath::vec2(val.x, val.y)
    }
}

impl From<emath::Pos2> for Vec2f {
    fn from(value: emath::Pos2) -> Self {
        Self::new(value.x, value.y)
    }
}

impl From<emath::Vec2> for Vec2f {
    fn from(value: emath::Vec2) -> Self {
        Self::new(value.x, value.y)
    }
}

impl<T: Zero> Zero for Vector2<T> {
    fn zero() -> Self {
        Self::new(T::zero(), T::zero())
    }

    fn is_zero(&self) -> bool {
        self.x.is_zero() && self.y.is_zero()
    }

    fn set_zero(&mut self) {
        self.x.set_zero();
        self.y.set_zero();
    }
}

impl<T> std::convert::AsRef<[T; 2]> for Vector2<T> {
    fn as_ref(&self) -> &[T; 2] {
        unsafe { std::mem::transmute(self) }
    }
}

impl<T> std::convert::AsMut<[T; 2]> for Vector2<T> {
    fn as_mut(&mut self) -> &mut [T; 2] {
        unsafe { std::mem::transmute(self) }
    }
}

impl<T> std::convert::AsRef<[T]> for Vector2<T> {
    fn as_ref(&self) -> &[T] {
        AsRef::<[T; 2]>::as_ref(self)
    }
}

impl<T> std::convert::AsMut<[T]> for Vector2<T> {
    fn as_mut(&mut self) -> &mut [T] {
        AsMut::<[T; 2]>::as_mut(self)
    }
}
impl<T> std::ops::Index<usize> for Vector2<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        match index {
            0 => &self.x,
            1 => &self.y,
            _ => panic!("component index {index} is out of bounds for 2d vector")
        }
    }
}
impl<T> std::ops::IndexMut<usize> for Vector2<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match index {
            0 => &mut self.x,
            1 => &mut self.y,
            _ => panic!("component index {index} is out of bounds for 2d vector")
        }
    }
}

impl<T: serde::Serialize> serde::Serialize for Vector2<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        AsRef::<[T; 2]>::as_ref(self).serialize(serializer)
    }
}

impl<'de, T: serde::Deserialize<'de> + Zero> serde::Deserialize<'de>
    for Vector2<T>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(Vector2Visitor::<T>(PhantomData))
    }
}

// Backwards compatibility, to deserialize both [T, T] and (T, T)
struct Vector2Visitor<T>(PhantomData<T>);

impl<'de, T: Deserialize<'de> + Zero> Visitor<'de> for Vector2Visitor<T> {
    type Value = Vector2<T>;

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>, {
        let x = seq.next_element()?.unwrap_or(T::zero());
        let y = seq.next_element()?.unwrap_or(T::zero());
        Ok(Vector2::new(x, y))
    }

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a 2d vector, as [T, T] or (T, T)")
    }
}

impl<T: geo_nd::Float + Default> geo_nd::Vector<T, 2> for Vector2<T>
{
    fn from_array(data: [T; 2]) -> Self {
        data.into()
    }

    fn zero() -> Self {
        Zero::zero()
    }

    fn into_array(self) -> [T; 2] {
        *self.as_ref()
    }

    fn is_zero(&self) -> bool {
        Zero::is_zero(self)
    }

    fn set_zero(&mut self) {
        Zero::set_zero(self)
    }

    fn reduce_sum(&self) -> T {
        self.x + self.y
    }

    fn mix(self, other: &Self, t: T) -> Self {
        let x = self.x + (other.x - self.x) * t;
        let y = self.y + (other.y - self.y) * t;
        Self::new(x, y)
    }

    fn dot(&self, other: &Self) -> T {
        self.x * other.x + self.y * other.y
    }
}
