use std::hash::Hash;

mod integer;

pub use integer::*;

/// A set with two binary operations, addition and multiplicative.
pub trait Ring: Clone + PartialEq + Eq + Hash {
    type Element: Clone + PartialEq + Eq + Hash;

    /// Computes `a + b`.
    fn add(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;

    /// Computes `a - b`.
    fn sub(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;

    /// Computes `a * b`.
    fn mul(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;

    /// Performs `a += b`.
    fn add_assign(&self, a: &mut Self::Element, b: &Self::Element);

    /// Performs `a -= b`.
    fn sub_assign(&self, a: &mut Self::Element, b: &Self::Element);

    /// Performs `a *= b`.
    fn mul_assign(&self, a: &mut Self::Element, b: &Self::Element);

    /// Performs `a += b * c`.
    fn mul_add_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element);

    /// Performs `a -= b * c`.
    fn mul_sub_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element);

    /// Computes `-a`, or the additive inverse of `a`.
    fn neg(&self, a: &Self::Element) -> Self::Element;

    /// Returns the additive identity of this ring.
    fn zero(&self) -> Self::Element;

    /// Returns the multiplicative identity of this ring.
    fn one(&self) -> Self::Element;

    /// Computes the `n`th ring element.
    fn nth(&self, n: Integer) -> Self::Element;

    /// Computes `b ^ e`.
    fn pow(&self, b: &Self::Element, e: u64) -> Self::Element;

    /// Tests whether or not `a` is the additive identity of this ring.
    fn is_zero(&self, a: &Self::Element) -> bool;

    /// Tests whether or not `a` is the multiplicative identity of this ring.
    fn is_one(&self, a: &Self::Element) -> bool;

    /// Tests whether or not the GCD unit of this ring is one. That is, `gcd(1, x) = 1`.
    fn gcd_unit_eq_one() -> bool;

    /// Returns the characteristic of this ring.
    ///
    /// The characteristic of a ring is the smallest possible integer `n` such that adding the
    /// ring's multiplicative identity to itself `n` times results in the additive identity. If no
    /// such `n` exists, the characteristic is zero.
    fn characteristic(&self) -> Integer;

    /// The number of elements in the ring, or zero if ring is of infinite size.
    fn size(&self) -> Integer;

    /// Computes `a / b` if possible and if the result is unique.
    fn try_div(&self, a: &Self::Element, b: &Self::Element) -> Option<Self::Element>;
}

/// A ring that supports division with remainder, quotients, and gcds.
pub trait EuclideanDomain: Ring {
    /// Computes the least nonnegative remainder of `a (mod b)`.
    fn rem(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;

    /// Divides `a` by `b`, returning the quotient and remainder.
    fn quot_rem(&self, a: &Self::Element, b: &Self::Element) -> (Self::Element, Self::Element);

    /// Computes the greatest common demoninator of `a` and `b`.
    fn gcd(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;
}

/// A ring that supports division and inversion.
pub trait Field: EuclideanDomain {
    /// Computes `a / b`.
    fn div(&self, a: &Self::Element, b: &Self::Element) -> Self::Element;

    /// Performs `a /= b`.
    fn div_assign(&self, a: &mut Self::Element, b: &Self::Element) -> Self::Element;

    /// Computes `1 / a`, or the multiplicative inverse of `a`.
    fn inv(&self, a: &Self::Element) -> Self::Element;
}
