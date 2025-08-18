use std::{
    borrow::Cow,
    fmt::Display,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
};

use crate::{EuclideanDomain, Field, Integer, IntegerRing, InternalOrdering, Ring, UpgradeToField, Z};

/// The field of rational numbers.
pub type Q = FractionField<IntegerRing>;
/// The field of rational numbers.
pub const Q: Q = FractionField::new(Z);

/// The fraction field of ring `R`.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FractionField<R: Ring> {
    ring: R,
}

impl<R: Ring> FractionField<R> {
    pub const fn new(ring: R) -> FractionField<R> {
        FractionField { ring }
    }
}

impl<R: Ring> Display for FractionField<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

pub trait FractionNormalization: Ring {
    /// Get the factor that normalizes the element `a`.
    fn normalization_factor(&self, a: &Self::Element) -> Self::Element;
}

impl FractionNormalization for Z {
    fn normalization_factor(&self, a: &Self::Element) -> Self::Element {
        if *a < 0 { (-1).into() } else { 1.into() }
    }
}

impl<T: Field> FractionNormalization for T {
    fn normalization_factor(&self, a: &Self::Element) -> Self::Element {
        self.inv(a)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Fraction<R: Ring> {
    numerator: R::Element,
    denominator: R::Element,
}

impl<R: Ring> InternalOrdering for Fraction<R> {
    fn internal_cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.numerator
            .internal_cmp(&other.numerator)
            .then_with(|| self.denominator.internal_cmp(&other.denominator))
    }
}

impl<R: EuclideanDomain + FractionNormalization> FractionField<R> {
    pub fn to_element_numerator(&self, numerator: R::Element) -> <Self as Ring>::Element {
        Fraction {
            numerator,
            denominator: self.ring.one(),
        }
    }

    pub fn to_element(
        &self,
        mut numerator: R::Element,
        mut denominator: R::Element,
        do_gcd: bool,
    ) -> <Self as Ring>::Element {
        if do_gcd {
            let g = self.ring.gcd(&numerator, &denominator);
            if !self.ring.is_one(&g) {
                numerator = self.ring.quot_rem(&numerator, &g).0;
                denominator = self.ring.quot_rem(&denominator, &g).0;
            }
        }

        let f = self.ring.normalization_factor(&denominator);

        if self.ring.is_one(&f) {
            Fraction { numerator, denominator }
        } else {
            Fraction {
                numerator: self.ring.mul(&numerator, &f),
                denominator: self.ring.mul(&denominator, &f),
            }
        }
    }
}

impl<R: EuclideanDomain + FractionNormalization> Ring for FractionField<R> {
    type Element = Fraction<R>;

    fn add(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        let r = &self.ring;

        if a.denominator == b.denominator {
            let num = r.add(&a.numerator, &b.numerator);
            let g = r.gcd(&num, &a.denominator);
            if !r.is_one(&g) {
                return Fraction {
                    numerator: r.quot_rem(&num, &g).0,
                    denominator: r.quot_rem(&a.denominator, &g).0,
                };
            } else {
                return Fraction {
                    numerator: num,
                    denominator: a.denominator.clone(),
                };
            }
        }

        let denom_gcd = r.gcd(&a.denominator, &b.denominator);

        let mut a_den_red = Cow::Borrowed(&a.denominator);
        let mut b_den_red = Cow::Borrowed(&b.denominator);

        if !r.is_one(&denom_gcd) {
            a_den_red = Cow::Owned(r.quot_rem(&a.denominator, &denom_gcd).0);
            b_den_red = Cow::Owned(r.quot_rem(&b.denominator, &denom_gcd).0);
        }

        let num1 = r.mul(&a.numerator, &b_den_red);
        let num2 = r.mul(&b.numerator, &a_den_red);
        let mut num = r.add(&num1, &num2);

        let mut den = r.mul(b_den_red.as_ref(), &a.denominator);

        let g = r.gcd(&num, &denom_gcd);

        if !r.is_one(&g) {
            num = r.quot_rem(&num, &g).0;
            den = r.quot_rem(&den, &g).0;
        }

        Fraction {
            numerator: num,
            denominator: den,
        }
    }

    fn sub(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        self.add(a, &self.neg(b))
    }

    fn mul(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        let r = &self.ring;
        let gcd1 = r.gcd(&a.numerator, &b.denominator);
        let gcd2 = r.gcd(&a.denominator, &b.numerator);

        if r.is_one(&gcd1) {
            if r.is_one(&gcd2) {
                Fraction {
                    numerator: r.mul(&a.numerator, &b.numerator),
                    denominator: r.mul(&a.denominator, &b.denominator),
                }
            } else {
                Fraction {
                    numerator: r.mul(&a.numerator, &r.quot_rem(&b.numerator, &gcd2).0),
                    denominator: r.mul(&r.quot_rem(&a.denominator, &gcd2).0, &b.denominator),
                }
            }
        } else if r.is_one(&gcd2) {
            Fraction {
                numerator: r.mul(&r.quot_rem(&a.numerator, &gcd1).0, &b.numerator),
                denominator: r.mul(&a.denominator, &r.quot_rem(&b.denominator, &gcd1).0),
            }
        } else {
            Fraction {
                numerator: r.mul(&r.quot_rem(&a.numerator, &gcd1).0, &r.quot_rem(&b.numerator, &gcd2).0),
                denominator: r.mul(
                    &r.quot_rem(&a.denominator, &gcd2).0,
                    &r.quot_rem(&b.denominator, &gcd1).0,
                ),
            }
        }
    }

    fn add_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a = self.add(a, b);
    }

    fn sub_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a = self.sub(a, b);
    }

    fn mul_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a = self.mul(a, b);
    }

    fn mul_add_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        self.add_assign(a, &self.mul(b, c));
    }

    fn mul_sub_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        self.sub_assign(a, &self.mul(b, c));
    }

    fn neg(&self, a: &Self::Element) -> Self::Element {
        Fraction {
            numerator: self.ring.neg(&a.numerator),
            denominator: a.denominator.clone(),
        }
    }

    fn zero(&self) -> Self::Element {
        Fraction {
            numerator: self.ring.zero(),
            denominator: self.ring.one(),
        }
    }

    fn one(&self) -> Self::Element {
        Fraction {
            numerator: self.ring.one(),
            denominator: self.ring.one(),
        }
    }

    fn nth(&self, n: super::Integer) -> Self::Element {
        Fraction {
            numerator: self.ring.nth(n),
            denominator: self.ring.one(),
        }
    }

    fn pow(&self, b: &Self::Element, e: u64) -> Self::Element {
        Fraction {
            numerator: self.ring.pow(&b.numerator, e),
            denominator: self.ring.pow(&b.denominator, e),
        }
    }

    fn is_zero(&self, a: &Self::Element) -> bool {
        self.ring.is_zero(&a.numerator)
    }

    fn is_one(&self, a: &Self::Element) -> bool {
        self.ring.is_one(&a.numerator) && self.ring.is_one(&a.denominator)
    }

    fn gcd_unit_eq_one() -> bool {
        false
    }

    fn characteristic(&self) -> super::Integer {
        self.ring.characteristic()
    }

    fn size(&self) -> super::Integer {
        self.ring.size() * self.ring.size()
    }

    fn try_div(&self, a: &Self::Element, b: &Self::Element) -> Option<Self::Element> {
        if self.is_zero(b) { None } else { Some(self.div(a, b)) }
    }
}

impl<R: EuclideanDomain + FractionNormalization> EuclideanDomain for FractionField<R> {
    fn rem(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        self.zero()
    }

    fn quot_rem(&self, a: &Self::Element, b: &Self::Element) -> (Self::Element, Self::Element) {
        (self.div(a, b), self.zero())
    }

    fn gcd(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        let gcd_num = self.ring.gcd(&a.numerator, &b.numerator);
        let gcd_den = self.ring.gcd(&a.denominator, &b.denominator);

        let d1 = self.ring.quot_rem(&a.denominator, &gcd_den).0;
        let lcm = self.ring.mul(&d1, &b.denominator);

        Fraction {
            numerator: gcd_num,
            denominator: lcm,
        }
    }
}

impl<R: EuclideanDomain + FractionNormalization> Field for FractionField<R> {
    fn div(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        self.mul(a, &self.inv(b))
    }

    fn div_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a = self.div(a, b);
    }

    fn inv(&self, a: &Self::Element) -> Self::Element {
        if self.ring.is_zero(&a.numerator) {
            panic!("Division by 0");
        }

        let f = self.ring.normalization_factor(&a.numerator);

        Fraction {
            numerator: self.ring.mul(&a.denominator, &f),
            denominator: self.ring.mul(&a.numerator, &f),
        }
    }
}

/// A rational number.
pub type Rational = Fraction<IntegerRing>;

impl UpgradeToField for IntegerRing {
    type Upgraded = Q;

    fn upgrade(self) -> Self::Upgraded {
        Q
    }

    fn upgrade_element(&self, element: <Self as Ring>::Element) -> <Self::Upgraded as Ring>::Element {
        Rational::from(element)
    }
}

impl Default for Rational {
    fn default() -> Self {
        Rational::zero()
    }
}

impl From<f64> for Rational {
    #[inline]
    fn from(f: f64) -> Self {
        assert!(f.is_finite());

        let bits: u64 = f.to_bits();
        let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
        let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
        let mantissa = if exponent == 0 {
            (bits & 0xfffffffffffff) << 1
        } else {
            (bits & 0xfffffffffffff) | 0x10000000000000
        };

        exponent -= 1023 + 52;

        if exponent < 0 {
            (
                (sign as i64 * mantissa as i64).into(),
                Integer::from(2).pow(-exponent as u32),
            )
                .into()
        } else {
            (
                &Integer::from(sign as i64 * mantissa as i64) * &Integer::from(2).pow(exponent as u32),
                1.into(),
            )
                .into()
        }
    }
}

impl<T: Into<Integer>> From<T> for Rational {
    #[inline]
    fn from(value: T) -> Self {
        Rational {
            numerator: value.into(),
            denominator: 1.into(),
        }
    }
}

impl From<&Integer> for Rational {
    #[inline]
    fn from(value: &Integer) -> Self {
        Rational {
            numerator: value.clone(),
            denominator: 1.into(),
        }
    }
}

impl<T: Into<Integer>> From<(T, T)> for Rational {
    #[inline]
    fn from((num, den): (T, T)) -> Self {
        Q.to_element(num.into(), den.into(), true)
    }
}

impl Rational {
    pub fn new<T: Into<Integer>>(num: T, den: T) -> Rational {
        Q.to_element(num.into(), den.into(), true)
    }

    pub fn from_unchecked<T: Into<Integer>>(num: T, den: T) -> Rational {
        Q.to_element(num.into(), den.into(), false)
    }

    pub fn is_negative(&self) -> bool {
        self.numerator < 0
    }

    pub fn is_integer(&self) -> bool {
        self.denominator.is_one()
    }

    pub fn zero() -> Rational {
        Rational {
            numerator: 0.into(),
            denominator: 1.into(),
        }
    }

    pub fn one() -> Rational {
        Rational {
            numerator: 1.into(),
            denominator: 1.into(),
        }
    }

    pub fn abs(&self) -> Rational {
        if self.is_negative() {
            self.clone().neg()
        } else {
            self.clone()
        }
    }

    pub fn is_zero(&self) -> bool {
        self.numerator.is_zero()
    }

    pub fn is_one(&self) -> bool {
        self.numerator.is_one() && self.denominator.is_one()
    }

    pub fn pow(&self, e: u64) -> Rational {
        Q.pow(self, e)
    }

    pub fn inv(&self) -> Rational {
        Q.inv(self)
    }

    pub fn neg(&self) -> Rational {
        Q.neg(self)
    }

    pub fn gcd(&self, other: &Rational) -> Rational {
        Q.gcd(self, other)
    }

    pub fn to_f64(&self) -> f64 {
        rug::Rational::from((
            self.numerator.clone().into_bigint(),
            self.denominator.clone().into_bigint(),
        ))
        .to_f64()
    }

    pub fn into_bigint(self) -> rug::Rational {
        rug::Rational::from((self.numerator.into_bigint(), self.denominator.into_bigint()))
    }

    /// Round to the rational with the smallest numerator or denominator in the interval
    /// `[self * (1 - relative_error), self * (1 + relative_error)]`, where `0 < relative_error < 1`.
    pub fn round(&self, relative_error: &Rational) -> Rational {
        if self.is_zero() {
            return Rational::zero();
        }

        if self.is_negative() {
            self.round_in_interval(
                self.clone() * (Rational::one() + relative_error),
                self.clone() * (Rational::one() - relative_error),
            )
        } else {
            self.round_in_interval(
                self.clone() * (Rational::one() - relative_error),
                self.clone() * (Rational::one() + relative_error),
            )
        }
    }

    /// Round to the rational with the smallest numerator or denominator in the interval `[l, u]`,
    /// where `l < u`.
    pub fn round_in_interval(&self, mut l: Rational, mut u: Rational) -> Rational {
        assert!(l < u);

        let mut flip = false;
        if l.is_negative() && u.is_negative() {
            flip = true;
            (l, u) = (-u, -l);
        } else if l.is_negative() {
            return Rational::zero();
        }

        let (mut ln, mut ld) = (l.numerator, l.denominator);
        let (mut un, mut ud) = (u.numerator, u.denominator);

        let (mut h1, mut h0, mut k1, mut k0): (Integer, Integer, Integer, Integer) =
            (1.into(), 0.into(), 0.into(), 1.into());

        loop {
            let a = &(&ln - &1.into()) / &ld;
            (ld, ud, ln, un) = (&un - &a * &ud, &ln - &a * &ld, ud, ld);
            (h1, h0) = (&a * &h1 + &h0, h1);
            (k1, k0) = (&a * &k1 + &k0, k1);

            if ln <= ld {
                let res: Rational = (h1 + &h0, k1 + &k0).into();

                if flip {
                    return -res;
                } else {
                    return res;
                }
            }
        }
    }

    /// Round to the nearest integer towards zero.
    pub fn floor(&self) -> Integer {
        &self.numerator / &self.denominator
    }

    /// Round to the nearest integer away from zero.
    pub fn ceil(&self) -> Integer {
        if self.is_negative() {
            (self.numerator.clone() + 1) / &self.denominator - 1
        } else {
            ((self.numerator.clone() - 1) / &self.denominator) + 1
        }
    }

    /// Round to the nearest integer.
    pub fn round_to_nearest_integer(&self) -> Integer {
        if self.is_negative() {
            (self - &(1, 2).into()).floor()
        } else {
            (self + &(1, 2).into()).floor()
        }
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.denominator == other.denominator {
            return self.numerator.cmp(&other.numerator);
        }

        let a = &self.numerator * &other.denominator;
        let b = &self.denominator * &other.numerator;

        a.cmp(&b)
    }
}

macro_rules! impl_rat_op {
    ($trait:ident, $f:ident) => {
        impl $trait<Rational> for Rational {
            type Output = Rational;

            fn $f(self, other: Rational) -> Self::Output {
                Q.$f(&self, &other)
            }
        }

        impl<'a> $trait<&'a Rational> for Rational {
            type Output = Rational;

            fn $f(self, other: &'a Rational) -> Self::Output {
                Q.$f(&self, other)
            }
        }

        impl<'a> $trait<&'a Rational> for &Rational {
            type Output = Rational;

            fn $f(self, other: &'a Rational) -> Self::Output {
                Q.$f(self, other)
            }
        }
    };
}

impl_rat_op!(Add, add);
impl_rat_op!(Sub, sub);
impl_rat_op!(Mul, mul);
impl_rat_op!(Div, div);

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Q.neg(&self)
    }
}

macro_rules! impl_rat_op_assign {
    ($trait:ident, $f:ident) => {
        impl<'a> $trait<&'a Rational> for Rational {
            fn $f(&mut self, other: &'a Rational) {
                Q.$f(self, other)
            }
        }

        impl $trait<Rational> for Rational {
            fn $f(&mut self, other: Rational) {
                Q.$f(self, &other)
            }
        }
    };
}

impl_rat_op_assign!(AddAssign, add_assign);
impl_rat_op_assign!(SubAssign, sub_assign);
impl_rat_op_assign!(MulAssign, mul_assign);
impl_rat_op_assign!(DivAssign, div_assign);

impl<'a> std::iter::Sum<&'a Self> for Rational {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(Rational::zero(), |a, b| a + b)
    }
}
