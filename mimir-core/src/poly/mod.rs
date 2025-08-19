use std::cmp::Ordering;
use std::ops::{Add as OpAdd, Mul as OpMul, Sub};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter::Sum,
    ops::{AddAssign, Div, Rem},
};

use crate::{Ring, gcd_signed, gcd_unsigned};

mod add;
mod mul;
mod multivariate;

pub use multivariate::*;

pub trait Exponent:
    Hash
    + Debug
    + Display
    + Ord
    + OpMul<Output = Self>
    + Div<Output = Self>
    + Rem<Output = Self>
    + Sub<Output = Self>
    + OpAdd<Output = Self>
    + Sum<Self>
    + AddAssign
    + Clone
    + Copy
    + PartialEq
    + Eq
{
    fn zero() -> Self;

    fn one() -> Self;

    fn to_i32(&self) -> i32;

    fn from_i32(n: i32) -> Option<Self>;

    fn is_zero(&self) -> bool;

    fn checked_add(&self, other: &Self) -> Option<Self>;

    fn gcd(&self, other: &Self) -> Self;
}

impl Exponent for u32 {
    #[inline]
    fn zero() -> Self {
        0
    }

    #[inline]
    fn one() -> Self {
        1
    }

    #[inline]
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    #[inline]
    fn from_i32(n: i32) -> Option<Self> {
        if n < 0 { None } else { Some(n as u32) }
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == 0
    }

    #[inline]
    fn checked_add(&self, other: &Self) -> Option<Self> {
        i32::checked_add(*self as i32, *other as i32).map(|x| x as u32)
    }

    #[inline]
    fn gcd(&self, other: &Self) -> Self {
        gcd_unsigned(*self as u64, *other as u64) as Self
    }
}

impl Exponent for i32 {
    #[inline]
    fn zero() -> Self {
        0
    }

    #[inline]
    fn one() -> Self {
        1
    }

    #[inline]
    fn to_i32(&self) -> i32 {
        *self
    }

    #[inline]
    fn from_i32(n: i32) -> Option<Self> {
        Some(n)
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == 0
    }

    #[inline]
    fn checked_add(&self, other: &Self) -> Option<Self> {
        i32::checked_add(*self, *other)
    }

    #[inline]
    fn gcd(&self, other: &Self) -> Self {
        gcd_signed(*self as i64, *other as i64) as Self
    }
}

impl Exponent for u16 {
    #[inline]
    fn zero() -> Self {
        0
    }

    #[inline]
    fn one() -> Self {
        1
    }

    #[inline]
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    #[inline]
    fn from_i32(n: i32) -> Option<Self> {
        if n >= 0 && n <= u16::MAX as i32 {
            Some(n as u16)
        } else {
            None
        }
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == 0
    }

    #[inline]
    fn checked_add(&self, other: &Self) -> Option<Self> {
        u16::checked_add(*self, *other)
    }

    #[inline]
    fn gcd(&self, other: &Self) -> Self {
        gcd_unsigned(*self as u64, *other as u64) as Self
    }
}

impl Exponent for i16 {
    #[inline]
    fn zero() -> Self {
        0
    }

    #[inline]
    fn one() -> Self {
        1
    }

    #[inline]
    fn to_i32(&self) -> i32 {
        *self as i32
    }

    #[inline]
    fn from_i32(n: i32) -> Option<Self> {
        if n >= i16::MIN as i32 && n <= i16::MAX as i32 {
            Some(n as i16)
        } else {
            None
        }
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == 0
    }

    #[inline]
    fn checked_add(&self, other: &Self) -> Option<Self> {
        i16::checked_add(*self, *other)
    }

    #[inline]
    fn gcd(&self, other: &Self) -> Self {
        gcd_signed(*self as i64, *other as i64) as Self
    }
}

/// A positive exponent.
pub trait PositiveExponent: Exponent {
    fn from_u32(n: u32) -> Option<Self> {
        if n > i32::MAX as u32 {
            panic!("Exponent {} too large for i32", n);
        }

        Self::from_i32(n as i32)
    }

    fn to_u32(&self) -> u32;
}

impl PositiveExponent for u16 {
    #[inline]
    fn to_u32(&self) -> u32 {
        *self as u32
    }
}

impl PositiveExponent for u32 {
    #[inline]
    fn to_u32(&self) -> u32 {
        *self
    }
}

/// A well-ordering of monomials.
pub trait MonomialOrder: Clone {
    fn cmp<E: Exponent>(a: &[E], b: &[E]) -> Ordering;
}

/// Graded reveres lexicographic ordering of monomials.
#[derive(Clone)]
pub struct GrevLexOrder;

impl MonomialOrder for GrevLexOrder {
    #[inline]
    fn cmp<E: Exponent>(a: &[E], b: &[E]) -> Ordering {
        let deg: E = a.iter().cloned().sum();
        let deg2: E = b.iter().cloned().sum();

        match deg.cmp(&deg2) {
            Ordering::Equal => (),
            x => {
                return x;
            },
        }

        for (a1, a2) in a.iter().rev().zip(b.iter().rev()) {
            match a1.cmp(a2) {
                Ordering::Equal => (),
                x => {
                    return x.reverse();
                },
            }
        }

        Ordering::Equal
    }
}

#[derive(Clone)]
pub struct LexOrder;

impl MonomialOrder for LexOrder {
    fn cmp<E: Exponent>(a: &[E], b: &[E]) -> Ordering {
        a.cmp(b)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PolynomialRing<R: Ring> {
    pub ring: R,
}
