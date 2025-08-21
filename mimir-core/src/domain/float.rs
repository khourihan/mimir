use std::{
    f64::{self, consts::LOG10_2},
    fmt::{Debug, Display, LowerExp},
    hash::Hash,
    marker::PhantomData,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
    str::FromStr,
};

use crate::{EuclideanDomain, Field, Integer, InternalOrdering, Rational, Ring};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FloatField<T> {
    _phantom: PhantomData<T>,
}

impl Default for FloatField<F64> {
    fn default() -> Self {
        Self {
            _phantom: Default::default(),
        }
    }
}

impl FloatField<F64> {
    pub fn new() -> Self {
        FloatField { _phantom: PhantomData }
    }
}

impl<T> Display for FloatField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl<T: NumericalFloatLike + SingleFloat + Hash + Eq + InternalOrdering> Ring for FloatField<T> {
    type Element = T;

    #[inline(always)]
    fn add(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a.clone() + b.clone()
    }

    #[inline(always)]
    fn sub(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a.clone() - b.clone()
    }

    #[inline(always)]
    fn mul(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a.clone() * b.clone()
    }

    #[inline(always)]
    fn add_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a += b;
    }

    #[inline(always)]
    fn sub_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a -= b;
    }

    #[inline(always)]
    fn mul_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a *= b;
    }

    #[inline(always)]
    fn mul_add_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        *a = b.mul_add(c, a);
    }

    #[inline(always)]
    fn mul_sub_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        *a = b.mul_add(&-c.clone(), a);
    }

    #[inline(always)]
    fn neg(&self, a: &Self::Element) -> Self::Element {
        -a.clone()
    }

    #[inline(always)]
    fn zero(&self) -> Self::Element {
        T::zero()
    }

    #[inline(always)]
    fn one(&self) -> Self::Element {
        T::one()
    }

    #[inline(always)]
    fn nth(&self, n: Integer) -> Self::Element {
        T::from_rational(&n.into())
    }

    #[inline(always)]
    fn pow(&self, b: &Self::Element, e: u64) -> Self::Element {
        b.pow(e)
    }

    #[inline(always)]
    fn is_zero(&self, a: &Self::Element) -> bool {
        a.is_zero()
    }

    #[inline(always)]
    fn is_one(&self, a: &Self::Element) -> bool {
        a.is_one()
    }

    #[inline(always)]
    fn gcd_unit_eq_one() -> bool {
        true
    }

    #[inline(always)]
    fn characteristic(&self) -> Integer {
        Integer::zero()
    }

    #[inline(always)]
    fn size(&self) -> Integer {
        Integer::zero()
    }

    #[inline(always)]
    fn try_div(&self, a: &Self::Element, b: &Self::Element) -> Option<Self::Element> {
        Some(a.clone() / b)
    }
}

impl<T: NumericalFloatLike + SingleFloat + Hash + Eq + InternalOrdering> EuclideanDomain for FloatField<T> {
    #[inline(always)]
    fn rem(&self, a: &Self::Element, _: &Self::Element) -> Self::Element {
        T::zero()
    }

    #[inline(always)]
    fn quot_rem(&self, a: &Self::Element, b: &Self::Element) -> (Self::Element, Self::Element) {
        (a.clone() / b, T::zero())
    }

    #[inline(always)]
    fn gcd(&self, _: &Self::Element, _: &Self::Element) -> Self::Element {
        T::one()
    }
}

impl<T: NumericalFloatLike + SingleFloat + Hash + Eq + InternalOrdering> Field for FloatField<T> {
    #[inline(always)]
    fn div(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a.clone() / b
    }

    #[inline(always)]
    fn div_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a /= b;
    }

    #[inline(always)]
    fn inv(&self, a: &Self::Element) -> Self::Element {
        a.inv()
    }
}

/// A number that may be floating point.
pub trait NumericalFloatLike:
    PartialEq
    + Clone
    + Debug
    + Neg<Output = Self>
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self, Output = Self>
    + Div<Self, Output = Self>
    + for<'a> Add<&'a Self, Output = Self>
    + for<'a> Sub<&'a Self, Output = Self>
    + for<'a> Mul<&'a Self, Output = Self>
    + for<'a> Div<&'a Self, Output = Self>
    + AddAssign<Self>
    + SubAssign<Self>
    + MulAssign<Self>
    + DivAssign<Self>
    + for<'a> AddAssign<&'a Self>
    + for<'a> SubAssign<&'a Self>
    + for<'a> MulAssign<&'a Self>
    + for<'a> DivAssign<&'a Self>
{
    /// Computes `self * a + b`
    fn mul_add(&self, a: &Self, b: &Self) -> Self;

    /// Computes `-self`
    fn neg(&self) -> Self;

    fn zero() -> Self;

    fn one() -> Self;

    /// Computes `self ^ e`
    fn pow(&self, e: u64) -> Self;

    fn inv(&self) -> Self;

    fn from_usize(a: usize) -> Self;

    fn from_i64(a: i64) -> Self;

    fn epsilon() -> f64;
}

/// A single number.
pub trait SingleFloat: NumericalFloatLike {
    fn is_zero(&self) -> bool;

    fn is_one(&self) -> bool;

    fn is_negative_one(&self) -> bool;

    fn is_finite(&self) -> bool;

    fn from_rational(rat: &Rational) -> Self;
}

/// A number that can be rounded.
pub trait RoundableFloat: SingleFloat {
    fn to_usize_clamped(&self) -> usize;

    fn to_f64(&self) -> f64;

    fn round_to_nearest_integer(&self) -> Integer;
}

/// A real number.
pub trait Real: NumericalFloatLike {
    /// The constant π.
    fn pi() -> Self;

    /// Euler's number.
    fn e() -> Self;

    /// The Euler-Mascheroni constant.
    fn euler() -> Self;

    /// The golden ratio.
    fn phi() -> Self;

    /// The imaginary unit, if it exists.
    fn i() -> Option<Self>;

    fn norm(&self) -> Self;

    fn sqrt(&self) -> Self;

    fn log(&self) -> Self;

    fn exp(&self) -> Self;

    fn sin(&self) -> Self;

    fn cos(&self) -> Self;

    fn tan(&self) -> Self;

    fn asin(&self) -> Self;

    fn acos(&self) -> Self;

    fn atan2(&self, x: &Self) -> Self;

    fn sinh(&self) -> Self;

    fn cosh(&self) -> Self;

    fn tanh(&self) -> Self;

    fn asinh(&self) -> Self;

    fn acosh(&self) -> Self;

    fn atanh(&self) -> Self;

    fn powf(&self, e: &Self) -> Self;
}

#[derive(Debug, Clone, Copy)]
pub struct F64(pub f64);

impl F64 {
    pub fn into_inner(self) -> f64 {
        self.0
    }

    #[inline]
    pub fn is_negative(&self) -> bool {
        self.0.is_sign_negative()
    }

    #[inline]
    pub fn is_positive(&self) -> bool {
        self.0.is_sign_positive()
    }

    #[inline]
    pub fn abs(self) -> Self {
        F64(self.0.abs())
    }
}

impl NumericalFloatLike for F64 {
    #[inline(always)]
    fn mul_add(&self, a: &Self, b: &Self) -> Self {
        self.0.mul_add(a.0, b.0).into()
    }

    #[inline(always)]
    fn neg(&self) -> Self {
        (-self.0).into()
    }

    #[inline(always)]
    fn zero() -> Self {
        (0.0).into()
    }

    #[inline(always)]
    fn one() -> Self {
        (1.0).into()
    }

    #[inline(always)]
    fn pow(&self, e: u64) -> Self {
        self.0.powi(e as i32).into()
    }

    #[inline(always)]
    fn inv(&self) -> Self {
        self.0.recip().into()
    }

    #[inline(always)]
    fn from_usize(a: usize) -> Self {
        (a as f64).into()
    }

    #[inline(always)]
    fn from_i64(a: i64) -> Self {
        (a as f64).into()
    }

    #[inline(always)]
    fn epsilon() -> f64 {
        f64::EPSILON / 2.0
    }
}

impl Neg for F64 {
    type Output = Self;

    #[inline]
    fn neg(self) -> Self::Output {
        self.0.neg().into()
    }
}

macro_rules! impl_f64_ops {
    ($trait:ident, $f:ident, $op:tt) => {
        impl $trait<&F64> for F64 {
            type Output = Self;

            #[inline]
            fn $f(self, rhs: &Self) -> Self::Output {
                (self.0 $op rhs.0).into()
            }
        }

        impl $trait<F64> for F64 {
            type Output = Self;

            #[inline]
            fn $f(self, rhs: Self) -> Self::Output {
                (self.0 $op rhs.0).into()
            }
        }
    }
}

impl_f64_ops!(Add, add, +);
impl_f64_ops!(Sub, sub, -);
impl_f64_ops!(Mul, mul, *);
impl_f64_ops!(Div, div, /);

macro_rules! impl_f64_op_assign {
    ($trait:ident, $f:ident, $op:tt) => {
        impl $trait<&F64> for F64 {
            #[inline]
            fn $f(&mut self, rhs: &F64) {
                self.0 $op rhs.0;
            }
        }

        impl $trait<F64> for F64 {
            #[inline]
            fn $f(&mut self, rhs: F64) {
                self.0 $op rhs.0
            }
        }
    }
}

impl_f64_op_assign!(AddAssign, add_assign, +=);
impl_f64_op_assign!(SubAssign, sub_assign, -=);
impl_f64_op_assign!(MulAssign, mul_assign, *=);
impl_f64_op_assign!(DivAssign, div_assign, /=);

impl SingleFloat for F64 {
    #[inline(always)]
    fn is_zero(&self) -> bool {
        self.0 == 0.0
    }

    #[inline(always)]
    fn is_one(&self) -> bool {
        self.0 == 1.0
    }

    #[inline(always)]
    fn is_negative_one(&self) -> bool {
        self.0 == -1.0
    }

    #[inline(always)]
    fn is_finite(&self) -> bool {
        self.0.is_finite()
    }

    #[inline(always)]
    fn from_rational(rat: &Rational) -> Self {
        rat.to_f64().into()
    }
}

impl RoundableFloat for F64 {
    #[inline]
    fn to_usize_clamped(&self) -> usize {
        self.0 as usize
    }

    #[inline]
    fn to_f64(&self) -> f64 {
        self.0
    }

    #[inline(always)]
    fn round_to_nearest_integer(&self) -> Integer {
        if self.0 < 0.0 {
            Integer::from_f64(self.0 - 0.5)
        } else {
            Integer::from_f64(self.0 + 0.5)
        }
    }
}

impl Real for F64 {
    #[inline(always)]
    fn pi() -> Self {
        std::f64::consts::PI.into()
    }

    #[inline(always)]
    fn e() -> Self {
        std::f64::consts::E.into()
    }

    #[inline(always)]
    fn euler() -> Self {
        0.577_215_664_901_532_9.into()
    }

    #[inline(always)]
    fn phi() -> Self {
        1.618_033_988_749_895.into()
    }

    #[inline(always)]
    fn i() -> Option<Self> {
        None
    }

    #[inline(always)]
    fn norm(&self) -> Self {
        self.0.abs().into()
    }

    #[inline(always)]
    fn sqrt(&self) -> Self {
        self.0.sqrt().into()
    }

    #[inline(always)]
    fn log(&self) -> Self {
        self.0.ln().into()
    }

    #[inline(always)]
    fn exp(&self) -> Self {
        self.0.exp().into()
    }

    #[inline(always)]
    fn sin(&self) -> Self {
        self.0.sin().into()
    }

    #[inline(always)]
    fn cos(&self) -> Self {
        self.0.cos().into()
    }

    #[inline(always)]
    fn tan(&self) -> Self {
        self.0.tan().into()
    }

    #[inline(always)]
    fn asin(&self) -> Self {
        self.0.asin().into()
    }

    #[inline(always)]
    fn acos(&self) -> Self {
        self.0.acos().into()
    }

    #[inline(always)]
    fn atan2(&self, x: &Self) -> Self {
        self.0.atan2(x.0).into()
    }

    #[inline(always)]
    fn sinh(&self) -> Self {
        self.0.sinh().into()
    }

    #[inline(always)]
    fn cosh(&self) -> Self {
        self.0.cosh().into()
    }

    #[inline(always)]
    fn tanh(&self) -> Self {
        self.0.tanh().into()
    }

    #[inline(always)]
    fn asinh(&self) -> Self {
        self.0.asinh().into()
    }

    #[inline(always)]
    fn acosh(&self) -> Self {
        self.0.acosh().into()
    }

    #[inline(always)]
    fn atanh(&self) -> Self {
        self.0.atanh().into()
    }

    #[inline(always)]
    fn powf(&self, e: &Self) -> Self {
        self.0.powf(e.0).into()
    }
}

impl FromStr for F64 {
    type Err = <f64 as FromStr>::Err;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        f64::from_str(s).map(|x| x.into())
    }
}

impl From<f64> for F64 {
    #[inline(always)]
    fn from(value: f64) -> Self {
        F64(value)
    }
}

impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() && other.0.is_nan() {
            true
        } else {
            self.0 == other.0
        }
    }
}

impl PartialOrd for F64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl InternalOrdering for F64 {
    fn internal_cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}

impl Display for F64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl LowerExp for F64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        LowerExp::fmt(&self.0, f)
    }
}

impl Eq for F64 {}

impl Hash for F64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            state.write_u64(0x7ff8000000000000);
        } else if self.0 == 0. {
            state.write_u64(0);
        } else {
            state.write_u64(self.0.to_bits());
        }
    }
}
