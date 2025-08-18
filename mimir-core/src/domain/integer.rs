use rug::ops::RemRounding;
use rug::{Complete, Integer as BigInteger, ops::Pow};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, Sub, SubAssign};
use std::{cmp::Ordering, fmt::Display, str::FromStr};

use crate::InternalOrdering;
use crate::domain::{EuclideanDomain, Ring};
/// The integer ring.
pub type Z = IntegerRing;
/// The integer ring.
pub const Z: IntegerRing = IntegerRing::new();

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IntegerRing;

impl Default for IntegerRing {
    fn default() -> Self {
        IntegerRing
    }
}

impl IntegerRing {
    pub const fn new() -> IntegerRing {
        IntegerRing
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Integer {
    Natural(i64),
    Double(i128),
    Large(BigInteger),
}

impl InternalOrdering for Integer {
    fn internal_cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(self, other)
    }
}

macro_rules! cast_integer {
    ($base:ty) => {
        impl From<$base> for Integer {
            #[inline]
            fn from(value: $base) -> Self {
                Integer::Natural(value as i64)
            }
        }

        impl PartialEq<$base> for Integer {
            #[inline]
            fn eq(&self, other: &$base) -> bool {
                match self {
                    Integer::Natural(n) => *n == *other as i64,
                    _ => false,
                }
            }
        }

        impl PartialEq<Integer> for $base {
            #[inline]
            fn eq(&self, other: &Integer) -> bool {
                other == self
            }
        }

        impl PartialOrd<$base> for Integer {
            #[inline]
            fn partial_cmp(&self, other: &$base) -> Option<Ordering> {
                match self {
                    Integer::Natural(n) => n.partial_cmp(&(*other as i64)),
                    x => {
                        if x.is_negative() {
                            Some(Ordering::Less)
                        } else {
                            Some(Ordering::Greater)
                        }
                    },
                }
            }
        }

        impl PartialOrd<Integer> for $base {
            #[inline]
            fn partial_cmp(&self, other: &Integer) -> Option<Ordering> {
                other.partial_cmp(self).map(|x| x.reverse())
            }
        }
    };
}

cast_integer!(i8);
cast_integer!(i16);
cast_integer!(i32);
cast_integer!(i64);
cast_integer!(u8);
cast_integer!(u16);
cast_integer!(u32);
cast_integer!(usize);

macro_rules! cmp_cast {
    ($base:ty) => {
        impl PartialEq<$base> for Integer {
            #[inline]
            fn eq(&self, other: &$base) -> bool {
                self == &Integer::from(*other)
            }
        }

        impl PartialOrd<$base> for Integer {
            #[inline]
            fn partial_cmp(&self, other: &$base) -> Option<Ordering> {
                self.partial_cmp(&Integer::from(*other))
            }
        }

        impl PartialEq<Integer> for $base {
            #[inline]
            fn eq(&self, other: &Integer) -> bool {
                other == self
            }
        }

        impl PartialOrd<Integer> for $base {
            #[inline]
            fn partial_cmp(&self, other: &Integer) -> Option<Ordering> {
                other.partial_cmp(self).map(|x| x.reverse())
            }
        }
    };
}

cmp_cast!(u64);
cmp_cast!(u128);
cmp_cast!(i128);

impl From<i128> for Integer {
    #[inline]
    fn from(value: i128) -> Self {
        Integer::from_double(value)
    }
}

impl From<u64> for Integer {
    #[inline]
    fn from(value: u64) -> Self {
        if value <= i64::MAX as u64 {
            Integer::Natural(value as i64)
        } else {
            Integer::Double(value as i128)
        }
    }
}

impl From<u128> for Integer {
    #[inline]
    fn from(value: u128) -> Self {
        if value <= i128::MAX as u128 {
            Integer::from_double(value as i128)
        } else {
            Integer::Large(value.into())
        }
    }
}

impl From<BigInteger> for Integer {
    #[inline]
    fn from(value: BigInteger) -> Self {
        if let Some(n) = value.to_i64() {
            Integer::Natural(n)
        } else if let Some(n) = value.to_i128() {
            Integer::Double(n)
        } else {
            Integer::Large(value)
        }
    }
}

impl FromStr for Integer {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() <= 20
            && let Ok(n) = s.parse::<i64>()
        {
            return Ok(Integer::Natural(n));
        }

        if s.len() <= 40
            && let Ok(n) = s.parse::<i128>()
        {
            return Ok(Integer::Double(n));
        }

        if let Ok(n) = s.parse::<BigInteger>() {
            Ok(Integer::Large(n))
        } else {
            Err("Could not parse integer")
        }
    }
}

impl std::fmt::Debug for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Natural(n) => std::fmt::Display::fmt(n, f),
            Self::Double(n) => std::fmt::Display::fmt(n, f),
            Self::Large(n) => std::fmt::Display::fmt(n, f),
        }
    }
}

impl Integer {
    pub fn new(num: i64) -> Integer {
        Integer::Natural(num)
    }

    #[inline]
    pub fn simplify(&mut self) -> &mut Self {
        match self {
            Integer::Double(n) => {
                *self = Integer::from_double(*n);
            },
            Integer::Large(n) => {
                if let Some(n) = n.to_i64() {
                    *self = Integer::Natural(n);
                } else if let Some(n) = n.to_i128() {
                    *self = Integer::Double(n);
                }
            },
            _ => (),
        }

        self
    }

    #[inline]
    pub fn from_double(n: i128) -> Integer {
        if n >= i64::MIN as i128 && n <= i64::MAX as i128 {
            Integer::Natural(n as i64)
        } else {
            Integer::Double(n)
        }
    }

    #[inline]
    pub fn from_f64(f: f64) -> Integer {
        Self::from(BigInteger::from_f64(f).unwrap())
    }

    #[inline]
    pub fn to_f64(&self) -> f64 {
        match self {
            Integer::Natural(n) => *n as f64,
            Integer::Double(n) => *n as f64,
            Integer::Large(n) => n.to_f64(),
        }
    }

    #[inline]
    pub fn to_u32(&self) -> Option<u32> {
        match self {
            Integer::Natural(n) => (*n).try_into().ok(),
            _ => None,
        }
    }

    #[inline]
    pub fn into_bigint(self) -> BigInteger {
        match self {
            Integer::Natural(n) => n.into(),
            Integer::Double(n) => n.into(),
            Integer::Large(n) => n,
        }
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        match self {
            Integer::Natural(n) => *n == 0,
            _ => false,
        }
    }

    #[inline]
    pub fn is_one(&self) -> bool {
        match self {
            Integer::Natural(n) => *n == 1,
            _ => false,
        }
    }

    #[inline]
    pub fn is_negative(&self) -> bool {
        match self {
            Integer::Natural(n) => *n < 0,
            Integer::Double(n) => *n < 0,
            Integer::Large(n) => BigInteger::from(n.signum_ref()) == -1,
        }
    }

    #[inline]
    pub const fn zero() -> Integer {
        Integer::Natural(0)
    }

    #[inline]
    pub const fn one() -> Integer {
        Integer::Natural(1)
    }

    #[inline]
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Integer::Natural(n) => Some(*n),
            _ => None,
        }
    }

    #[inline]
    pub fn abs(&self) -> Integer {
        match self {
            Integer::Natural(n) => {
                if *n == i64::MIN {
                    Integer::Double((*n as i128).abs())
                } else {
                    Integer::Natural(n.abs())
                }
            },
            Integer::Double(n) => {
                if *n == i128::MIN {
                    Integer::Large(BigInteger::from(*n).abs())
                } else {
                    Integer::Double(n.abs())
                }
            },
            Integer::Large(n) => Integer::Large(n.clone().abs()),
        }
    }

    #[inline]
    pub fn abs_cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Integer::Large(n1), Integer::Large(n2)) => n1.as_abs().cmp(&n2.as_abs()),
            (Integer::Natural(n1), Integer::Large(n2)) => n2
                .as_abs()
                .partial_cmp(&n1.unsigned_abs())
                .unwrap_or(Ordering::Equal)
                .reverse(),
            (Integer::Double(n1), Integer::Large(n2)) => n2
                .as_abs()
                .partial_cmp(&n1.unsigned_abs())
                .unwrap_or(Ordering::Equal)
                .reverse(),
            (Integer::Large(n1), Integer::Natural(n2)) => {
                n1.as_abs().partial_cmp(&n2.unsigned_abs()).unwrap_or(Ordering::Equal)
            },
            (Integer::Large(n1), Integer::Double(n2)) => {
                n1.as_abs().partial_cmp(&n2.unsigned_abs()).unwrap_or(Ordering::Equal)
            },
            (_, _) => Ord::cmp(&self.abs(), &other.abs()),
        }
    }

    /// Computes `n!`.
    pub fn factorial(n: u32) -> Integer {
        if n <= 20 {
            let mut f: i64 = 1;
            for x in 2..=n as i64 {
                f *= x;
            }

            Integer::Natural(f)
        } else {
            Integer::Large(BigInteger::factorial(n).complete())
        }
    }

    /// Computes the binomial coefficient `(n k) = n! / (k! (n - k)!)`
    pub fn binom(n: i64, mut k: i64) -> Integer {
        if n < 0 || k < 0 || k > n {
            return Integer::zero();
        }

        if k > n / 2 {
            k = n - k;
        }

        let mut res = Integer::one();
        for i in 1..=k {
            res *= n - k + i;
            res /= i;
        }

        res
    }

    /// Compute the multinomial coefficient `(k_1 + ... + k_n)! / (k_1! * ... * k_n!)`
    pub fn multinom(k: &[u32]) -> Integer {
        let mut mcr = Integer::one();
        let mut acc = 0i64;

        for v in k {
            if let Some(res) = acc.checked_add(*v as i64) {
                acc = res;
            } else {
                panic!("Sum of occurrences exceeds i64: {:?}", k);
            }

            mcr *= &Self::binom(acc, *v as i64);
        }

        mcr
    }

    /// Computes `self ^ e`.
    pub fn pow(&self, e: u32) -> Integer {
        if e == 0 {
            return Integer::one();
        }

        match self {
            Integer::Natural(b) => {
                if let Some(pn) = b.checked_pow(e) {
                    Integer::Natural(pn)
                } else if let Some(pn) = (*b as i128).checked_pow(e) {
                    Integer::Double(pn)
                } else {
                    Integer::Large(BigInteger::from(*b).pow(e))
                }
            },
            Integer::Double(b) => {
                if let Some(pn) = b.checked_pow(e) {
                    Integer::Double(pn)
                } else {
                    Integer::Large(BigInteger::from(*b).pow(e))
                }
            },
            Integer::Large(b) => Integer::Large(b.pow(e).into()),
        }
    }

    /// Divides `self` by `b`, returning the quotient and remainder.
    pub fn quot_rem(&self, other: &Integer) -> (Integer, Integer) {
        if other.is_zero() {
            panic!("Cannot divide by zero");
        }

        match (self, other) {
            (Integer::Natural(a), Integer::Natural(b)) => {
                if let Some(q) = a.checked_div_euclid(*b) {
                    (Integer::Natural(q), self - &(other * &Integer::Natural(q)))
                } else {
                    (Integer::Double(-(i64::MIN as i128)), Integer::zero())
                }
            },
            (Integer::Natural(a), Integer::Double(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        (Integer::Natural(-1), Integer::from_double(*a as i128 + *b))
                    } else {
                        (Integer::Natural(1), Integer::from_double(*a as i128 - *b))
                    }
                } else {
                    (Integer::zero(), Integer::Natural(*a))
                }
            },
            (Integer::Double(a), Integer::Natural(b)) => {
                if let Some(q) = a.checked_div_euclid(*b as i128) {
                    let q = Integer::from_double(q);
                    (q.clone(), self - &(other * &q))
                } else {
                    (Integer::Large(BigInteger::from(i128::MIN).neg()), Integer::zero())
                }
            },
            (Integer::Double(a), Integer::Double(b)) => {
                let q = Integer::from_double(a.div_euclid(*b));
                (q.clone(), self - &(other * &q))
            },
            (Integer::Natural(a), Integer::Large(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        (Integer::Natural(-1), Integer::from((a + b).complete()))
                    } else {
                        (Integer::Natural(1), Integer::from((a - b).complete()))
                    }
                } else {
                    (Integer::zero(), Integer::Natural(*a))
                }
            },
            (Integer::Large(a), Integer::Natural(b)) => {
                let r = a.clone().div_rem_euc(BigInteger::from(*b));
                (Integer::from(r.0), Integer::from(r.1))
            },
            (Integer::Large(a), Integer::Large(b)) => {
                let r = a.clone().div_rem_euc(b.clone());
                (Integer::from(r.0), Integer::from(r.1))
            },
            (Integer::Double(a), Integer::Large(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        (Integer::Natural(-1), Integer::from((a + b).complete()))
                    } else {
                        (Integer::Natural(1), Integer::from((a - b).complete()))
                    }
                } else {
                    (Integer::zero(), Integer::Double(*a))
                }
            },
            (Integer::Large(a), Integer::Double(b)) => {
                let r = a.clone().div_rem_euc(BigInteger::from(*b));
                (Integer::from(r.0), Integer::from(r.1))
            },
        }
    }

    /// Computes the greatest common denominator of `self` and `b`.
    pub fn gcd(&self, b: &Integer) -> Integer {
        match (self, b) {
            (Integer::Natural(n1), Integer::Natural(n2)) => {
                let gcd = gcd_signed(*n1, *n2);
                if gcd == i64::MAX as u64 + 1 {
                    Integer::Double(gcd as i128)
                } else {
                    Integer::Natural(gcd as i64)
                }
            },
            (Integer::Natural(n1), Integer::Large(n2)) | (Integer::Large(n2), Integer::Natural(n1)) => {
                let r1 = BigInteger::from(*n1);
                Integer::from(r1.gcd(n2))
            },
            (Integer::Large(n1), Integer::Large(n2)) => Integer::from(n1.clone().gcd(n2)),
            (Integer::Natural(n1), Integer::Double(n2)) | (Integer::Double(n2), Integer::Natural(n1)) => {
                Integer::from_double(gcd_signed_i128(*n1 as i128, *n2) as i128)
            },
            (Integer::Double(n1), Integer::Double(n2)) => {
                let gcd = gcd_signed_i128(*n1, *n2);
                if gcd == i128::MAX as u128 + 1 {
                    Integer::Large(BigInteger::from(gcd))
                } else {
                    Integer::from_double(gcd as i128)
                }
            },
            (Integer::Double(n1), Integer::Large(n2)) => Integer::from(BigInteger::from(*n1).clone().gcd(n2)),
            (Integer::Large(n1), Integer::Double(n2)) => Integer::from(n1.clone().gcd(&BigInteger::from(*n2))),
        }
    }

    /// Computes the greatest common denominator of `self` and `b` in addition to the coefficients
    /// of Bezout's identity.
    pub fn extended_gcd(&self, b: &Integer) -> (Integer, Integer, Integer) {
        match (self, b) {
            (Integer::Natural(n1), Integer::Natural(n2)) => {
                let (gcd, s, t) = extended_gcd(*n1, *n2);

                if gcd == i64::MAX as u64 + 1 {
                    (Integer::Double(gcd as i128), Integer::Natural(s), Integer::Natural(t))
                } else {
                    (Integer::Natural(gcd as i64), Integer::Natural(s), Integer::Natural(t))
                }
            },
            (Integer::Natural(n1), Integer::Large(n2)) | (Integer::Large(n2), Integer::Natural(n1)) => {
                let r1 = BigInteger::from(*n1);
                let (g, s, t) = r1.extended_gcd(n2.clone(), BigInteger::new());
                (Integer::from(g), Integer::from(s), Integer::from(t))
            },
            (Integer::Large(n1), Integer::Large(n2)) => {
                let (g, s, t) = n1.clone().extended_gcd(n2.clone(), BigInteger::new());
                (Integer::from(g), Integer::from(s), Integer::from(t))
            },
            (Integer::Natural(n1), Integer::Double(n2)) | (Integer::Double(n2), Integer::Natural(n1)) => {
                let (g, s, t) = extended_gcd_i128(*n1 as i128, *n2);
                (
                    Integer::from_double(g as i128),
                    Integer::from_double(s),
                    Integer::from_double(t),
                )
            },
            (Integer::Double(n1), Integer::Double(n2)) => {
                let (g, s, t) = extended_gcd_i128(*n1, *n2);
                if g == i128::MAX as u128 + 1 {
                    (
                        Integer::Large(BigInteger::from(g)),
                        Integer::from_double(s),
                        Integer::from_double(t),
                    )
                } else {
                    (
                        Integer::from_double(g as i128),
                        Integer::from_double(s),
                        Integer::from_double(t),
                    )
                }
            },
            (Integer::Double(n1), Integer::Large(n2)) => {
                let (g, s, t) = BigInteger::from(*n1)
                    .clone()
                    .extended_gcd(n2.clone(), BigInteger::new());
                (Integer::from(g), Integer::from(s), Integer::from(t))
            },
            (Integer::Large(n1), Integer::Double(n2)) => {
                let (g, s, t) = n1.clone().extended_gcd(BigInteger::from(*n2), BigInteger::new());
                (Integer::from(g), Integer::from(s), Integer::from(t))
            },
        }
    }

    /// Compute the least common multiple of `self` and `b`.
    pub fn lcm(&self, b: &Integer) -> Integer {
        let g = self.gcd(b);
        if g.is_zero() { Integer::zero() } else { (self / &g) * b }
    }

    /// Compute the symmetric mod `p` on `self`.
    #[inline]
    pub fn symmetric_mod(self, p: &Integer) -> Integer {
        let c = self % p;
        if &c + &c > *p { c - p } else { c }
    }

    /// Compute the modular inverse of `self` in the ring with size `n`.
    /// `self` and `n` must be coprime.
    pub fn mod_inverse(&self, n: &Integer) -> Integer {
        let mut t0 = Integer::zero();
        let mut t1 = Integer::one();
        let mut r0 = n.clone();
        let mut r1 = self.clone();

        while !r1.is_zero() {
            let (q, r) = Z.quot_rem(&r0, &r1);
            (t1, t0) = (&t0 - &(&q * &t1), t1);
            (r1, r0) = (r, r1);
        }

        if r0 > Integer::one() {
            panic!("{} is not invertible in ring {}", self, n);
        }

        if t0.is_negative() {
            t0 += n;
        }

        t0
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Integer::Natural(n) => n.fmt(f),
            Integer::Double(n) => n.fmt(f),
            Integer::Large(n) => n.fmt(f),
        }
    }
}

impl Display for IntegerRing {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Ord for Integer {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Integer::Natural(n1), Integer::Natural(n2)) => n1.cmp(n2),
            (Integer::Natural(n1), Integer::Double(n2)) => (*n1 as i128).cmp(n2),
            (Integer::Natural(n1), Integer::Large(n2)) => n1.partial_cmp(n2).unwrap(),
            (Integer::Double(n1), Integer::Natural(n2)) => n1.cmp(&(*n2 as i128)),
            (Integer::Double(n1), Integer::Double(n2)) => n1.cmp(n2),
            (Integer::Double(n1), Integer::Large(n2)) => n1.partial_cmp(n2).unwrap(),
            (Integer::Large(n1), Integer::Natural(n2)) => n1.partial_cmp(n2).unwrap(),
            (Integer::Large(n1), Integer::Double(n2)) => n1.partial_cmp(n2).unwrap(),
            (Integer::Large(n1), Integer::Large(n2)) => n1.cmp(n2),
        }
    }
}

impl PartialOrd for Integer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ring for IntegerRing {
    type Element = Integer;

    #[inline]
    fn add(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a + b
    }

    #[inline]
    fn sub(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a - b
    }

    #[inline]
    fn mul(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a * b
    }

    #[inline]
    fn add_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a += b;
    }

    #[inline]
    fn sub_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a -= b;
    }

    #[inline]
    fn mul_assign(&self, a: &mut Self::Element, b: &Self::Element) {
        *a *= b;
    }

    #[inline(always)]
    fn mul_add_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        if let Integer::Large(n) = a {
            match (b, c) {
                (Integer::Natural(b), Integer::Large(c)) => n.add_assign(b * c),
                (Integer::Double(b), Integer::Large(c)) => n.add_assign(b * c),
                (Integer::Large(b), Integer::Natural(c)) => n.add_assign(b * c),
                (Integer::Large(b), Integer::Double(c)) => n.add_assign(b * c),
                (Integer::Large(b), Integer::Large(c)) => n.add_assign(b * c),
                _ => {
                    *a += b * c;
                    return;
                },
            }

            a.simplify();
            return;
        }

        *a += b * c;
    }

    #[inline(always)]
    fn mul_sub_assign(&self, a: &mut Self::Element, b: &Self::Element, c: &Self::Element) {
        if let Integer::Large(n) = a {
            match (b, c) {
                (Integer::Natural(b), Integer::Large(c)) => n.sub_assign(b * c),
                (Integer::Double(b), Integer::Large(c)) => n.sub_assign(b * c),
                (Integer::Large(b), Integer::Natural(c)) => n.sub_assign(b * c),
                (Integer::Large(b), Integer::Double(c)) => n.sub_assign(b * c),
                (Integer::Large(b), Integer::Large(c)) => n.sub_assign(b * c),
                _ => {
                    *a -= b * c;
                    return;
                },
            }

            a.simplify();
            return;
        }

        *a -= b * c;
    }

    #[inline]
    fn neg(&self, a: &Self::Element) -> Self::Element {
        -a
    }

    #[inline]
    fn zero(&self) -> Self::Element {
        Integer::zero()
    }

    #[inline]
    fn one(&self) -> Self::Element {
        Integer::one()
    }

    #[inline]
    fn nth(&self, n: Integer) -> Self::Element {
        n
    }

    #[inline]
    fn pow(&self, b: &Self::Element, e: u64) -> Self::Element {
        b.pow(e as u32)
    }

    #[inline]
    fn is_zero(&self, a: &Self::Element) -> bool {
        a.is_zero()
    }

    #[inline]
    fn is_one(&self, a: &Self::Element) -> bool {
        a.is_one()
    }

    #[inline]
    fn gcd_unit_eq_one() -> bool {
        true
    }

    #[inline]
    fn characteristic(&self) -> Integer {
        Integer::zero()
    }

    #[inline]
    fn size(&self) -> Integer {
        Integer::zero()
    }

    #[inline]
    fn try_div(&self, a: &Self::Element, b: &Self::Element) -> Option<Self::Element> {
        if b.is_zero() {
            return None;
        }

        let r = a / b;
        if *a == &r * b { Some(r) } else { None }
    }
}

impl EuclideanDomain for IntegerRing {
    fn rem(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a % b
    }

    fn quot_rem(&self, a: &Self::Element, b: &Self::Element) -> (Self::Element, Self::Element) {
        a.quot_rem(b)
    }

    fn gcd(&self, a: &Self::Element, b: &Self::Element) -> Self::Element {
        a.gcd(b)
    }
}

macro_rules! impl_int_ops {
    ($trait:ident, $f:ident, $check_f:ident, $op:tt) => {
        impl $trait<&Integer> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn $f(self, rhs: &Integer) -> Integer {
                if let Integer::Large(s) = self {
                    match rhs {
                        Integer::Natural(r) => Integer::from(s $op r),
                        Integer::Double(r) => Integer::from(s $op r),
                        Integer::Large(r) => Integer::from(s $op r),
                    }
                } else {
                    &self $op rhs
                }
            }
        }

        impl $trait<Integer> for &Integer {
            type Output = Integer;

            #[inline(always)]
            fn $f(self, rhs: Integer) -> Integer {
                if let Integer::Large(r) = rhs {
                    match self {
                        Integer::Natural(s) => Integer::from(*s $op r),
                        Integer::Double(s) => Integer::from(*s $op r),
                        Integer::Large(s) => Integer::from(s $op r),
                    }
                } else {
                    self $op &rhs
                }
            }
        }

        impl $trait<Integer> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn $f(self, rhs: Integer) -> Integer {
                if let Integer::Large(s) = self {
                    match rhs {
                        Integer::Natural(r) => Integer::from(s $op r),
                        Integer::Double(r) => Integer::from(s $op r),
                        Integer::Large(r) => Integer::from(s $op r),
                    }
                } else if let Integer::Large(r) = rhs {
                    match self {
                        Integer::Natural(s) => Integer::from(s $op r),
                        Integer::Double(s) => Integer::from(s $op r),
                        Integer::Large(s) => Integer::from(s $op r),
                    }
                } else {
                    self $op &rhs
                }
            }
        }

        impl<'a> $trait<&'a Integer> for &Integer {
            type Output = Integer;

            #[inline(always)]
            fn $f(self, rhs: &'a Integer) -> Integer {
                match (self, rhs) {
                    (Integer::Natural(n1), Integer::Natural(n2)) => {
                        if let Some(num) = n1.$check_f(*n2) {
                            Integer::Natural(num)
                        } else {
                            Integer::Double(*n1 as i128 $op *n2 as i128)
                        }
                    },
                    (Integer::Natural(n1), Integer::Double(r2)) => {
                        if let Some(num) = (*n1 as i128).$check_f(*r2) {
                            Integer::from_double(num)
                        } else {
                            Integer::Large(BigInteger::from(*n1) $op *r2)
                        }
                    },
                    (Integer::Double(r1), Integer::Natural(r2)) => {
                        if let Some(num) = r1.$check_f(*r2 as i128) {
                            Integer::from_double(num)
                        } else {
                            Integer::Large(BigInteger::from(*r1) $op *r2)
                        }
                    },
                    (Integer::Double(r1), Integer::Double(r2)) => {
                        if let Some(num) = r1.$check_f(*r2) {
                            Integer::from_double(num)
                        } else {
                            Integer::Large(BigInteger::from(*r1) $op *r2)
                        }
                    },
                    (Integer::Natural(n1), Integer::Large(r2)) => Integer::from((*n1 $op r2).complete()),
                    (Integer::Large(r1), Integer::Natural(n2)) => Integer::from((r1 $op *n2).complete()),
                    (Integer::Double(n1), Integer::Large(r2)) => Integer::from((*n1 $op r2).complete()),
                    (Integer::Large(r1), Integer::Double(n2)) => Integer::from((r1 $op *n2).complete()),
                    (Integer::Large(r1), Integer::Large(r2)) => Integer::from((r1 $op r2).complete()),
                }
            }
        }
    };
}

impl_int_ops!(Add, add, checked_add, +);
impl_int_ops!(Sub, sub, checked_sub, -);
impl_int_ops!(Mul, mul, checked_mul, *);
impl_int_ops!(Div, div, checked_div, /);

macro_rules! impl_prim_ops {
    ($base:ty) => {
        impl Add<$base> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn add(self, rhs: $base) -> Integer {
                self + Integer::from(rhs)
            }
        }

        impl<'a> Add<$base> for &'a Integer {
            type Output = Integer;

            #[inline(always)]
            fn add(self, rhs: $base) -> Integer {
                self + Integer::from(rhs)
            }
        }

        impl<'a> Add<Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn add(self, rhs: Integer) -> Integer {
                rhs + self
            }
        }

        impl<'a> Add<&'a Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn add(self, rhs: &'a Integer) -> Integer {
                rhs + self
            }
        }

        impl Sub<$base> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn sub(self, rhs: $base) -> Integer {
                self - Integer::from(rhs)
            }
        }

        impl<'a> Sub<$base> for &'a Integer {
            type Output = Integer;

            #[inline(always)]
            fn sub(self, rhs: $base) -> Integer {
                self - Integer::from(rhs)
            }
        }

        impl<'a> Sub<Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn sub(self, rhs: Integer) -> Integer {
                (-rhs) + self
            }
        }

        impl<'a> Sub<&'a Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn sub(self, rhs: &'a Integer) -> Integer {
                -(rhs + -(self as i64))
            }
        }

        impl Mul<$base> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn mul(self, rhs: $base) -> Integer {
                self * Integer::from(rhs)
            }
        }

        impl<'a> Mul<$base> for &'a Integer {
            type Output = Integer;

            #[inline(always)]
            fn mul(self, rhs: $base) -> Integer {
                self * Integer::from(rhs)
            }
        }

        impl<'a> Mul<Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn mul(self, rhs: Integer) -> Integer {
                rhs * self
            }
        }

        impl<'a> Mul<&'a Integer> for $base {
            type Output = Integer;

            #[inline(always)]
            fn mul(self, rhs: &'a Integer) -> Integer {
                rhs * self
            }
        }

        impl Div<$base> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn div(self, rhs: $base) -> Integer {
                &self / rhs
            }
        }

        impl<'a> Div<$base> for &'a Integer {
            type Output = Integer;

            #[inline(always)]
            fn div(self, rhs: $base) -> Integer {
                self / Integer::from(rhs)
            }
        }

        impl Rem<$base> for Integer {
            type Output = Integer;

            #[inline(always)]
            fn rem(self, rhs: $base) -> Integer {
                &self % rhs
            }
        }

        impl<'a> Rem<$base> for &'a Integer {
            type Output = Integer;

            #[inline(always)]
            fn rem(self, rhs: $base) -> Integer {
                self % Integer::from(rhs)
            }
        }

        impl AddAssign<$base> for Integer {
            #[inline]
            fn add_assign(&mut self, rhs: $base) {
                *self += Integer::from(rhs);
            }
        }

        impl SubAssign<$base> for Integer {
            #[inline]
            fn sub_assign(&mut self, rhs: $base) {
                *self -= Integer::from(rhs);
            }
        }

        impl MulAssign<$base> for Integer {
            #[inline]
            fn mul_assign(&mut self, rhs: $base) {
                *self *= Integer::from(rhs);
            }
        }

        impl DivAssign<$base> for Integer {
            #[inline]
            fn div_assign(&mut self, rhs: $base) {
                *self /= Integer::from(rhs);
            }
        }
    };
}

impl_prim_ops!(i8);
impl_prim_ops!(i16);
impl_prim_ops!(i32);
impl_prim_ops!(i64);
impl_prim_ops!(i128);
impl_prim_ops!(u8);
impl_prim_ops!(u16);
impl_prim_ops!(u32);
impl_prim_ops!(u64);
impl_prim_ops!(u128);

macro_rules! impl_op_assign {
    ($trait:ident, $f:ident, $op:tt) => {
        impl $trait<Integer> for Integer {
            #[inline(always)]
            fn $f(&mut self, rhs: Integer) {
                if let Integer::Large(l) = self {
                    match rhs {
                        Integer::Natural(r) => l.$f(r),
                        Integer::Double(r) => l.$f(r),
                        Integer::Large(r) => l.$f(r),
                    }

                    self.simplify();
                } else {
                    *self = &*self $op rhs;
                }
            }
        }

        impl<'a> $trait<&'a Integer> for Integer {
            #[inline(always)]
            fn $f(&mut self, rhs: &'a Integer) {
                if let Integer::Large(l) = self {
                    match rhs {
                        Integer::Natural(r) => l.$f(*r),
                        Integer::Double(r) => l.$f(*r),
                        Integer::Large(r) => l.$f(r),
                    }

                    self.simplify();
                } else {
                    *self = &*self $op rhs;
                }
            }
        }
    };
}

impl_op_assign!(AddAssign, add_assign, +);
impl_op_assign!(SubAssign, sub_assign, -);
impl_op_assign!(MulAssign, mul_assign, *);
impl_op_assign!(DivAssign, div_assign, /);

impl Neg for Integer {
    type Output = Integer;

    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Integer::Natural(n) => {
                if let Some(neg) = n.checked_neg() {
                    Integer::Natural(neg)
                } else {
                    Integer::Double((n as i128).neg())
                }
            },
            Integer::Double(n) => {
                if let Some(neg) = n.checked_neg() {
                    Integer::from_double(neg)
                } else {
                    Integer::Large(BigInteger::from(n).neg())
                }
            },
            Integer::Large(n) => Integer::from(-n),
        }
    }
}

impl Neg for &Integer {
    type Output = Integer;

    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Integer::Natural(n) => {
                if let Some(neg) = n.checked_neg() {
                    Integer::Natural(neg)
                } else {
                    Integer::Double((*n as i128).neg())
                }
            },
            Integer::Double(n) => {
                if let Some(neg) = n.checked_neg() {
                    Integer::from_double(neg)
                } else {
                    Integer::Large(BigInteger::from(*n).neg())
                }
            },
            Integer::Large(n) => Integer::from(n.clone().neg()),
        }
    }
}

impl Rem<Integer> for Integer {
    type Output = Integer;

    fn rem(self, rhs: Integer) -> Self::Output {
        self % &rhs
    }
}

impl<'a> Rem<&'a Integer> for Integer {
    type Output = Integer;

    fn rem(self, rhs: &'a Integer) -> Self::Output {
        if rhs.is_zero() {
            panic!("Cannot divide by zero");
        }

        if !self.is_negative() && self < *rhs {
            return self;
        }

        match (self, rhs) {
            (Integer::Large(a), Integer::Natural(b)) => Integer::from(a.rem_euc(b)),
            (Integer::Large(a), Integer::Double(b)) => Integer::from(a.rem_euc(b)),
            (Integer::Large(a), Integer::Large(b)) => Integer::from(a.rem_euc(b)),
            (x, _) => (&x).rem(rhs),
        }
    }
}

impl Rem<Integer> for &Integer {
    type Output = Integer;

    fn rem(self, rhs: Integer) -> Self::Output {
        self % &rhs
    }
}

impl Rem for &Integer {
    type Output = Integer;

    fn rem(self, rhs: Self) -> Self::Output {
        if rhs.is_zero() {
            panic!("Cannot divide by zero");
        }

        if !self.is_negative() && self < rhs {
            return self.clone();
        }

        match (self, rhs) {
            (Integer::Natural(a), Integer::Natural(b)) => {
                if let Some(r) = a.checked_rem_euclid(*b) {
                    Integer::Natural(r)
                } else {
                    Integer::zero()
                }
            },
            (Integer::Natural(a), Integer::Double(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        Integer::from_double(*a as i128 + *b)
                    } else {
                        Integer::from_double(*a as i128 - *b)
                    }
                } else {
                    Integer::Natural(*a)
                }
            },
            (Integer::Natural(a), Integer::Large(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        Integer::from((a + b).complete())
                    } else {
                        Integer::from((a - b).complete())
                    }
                } else {
                    Integer::Natural(*a)
                }
            },
            (Integer::Double(a), Integer::Large(b)) => {
                if *a < 0 {
                    if *b > 0 {
                        Integer::from((a + b).complete())
                    } else {
                        Integer::from((a - b).complete())
                    }
                } else {
                    Integer::Double(*a)
                }
            },
            (Integer::Double(a), Integer::Natural(b)) => {
                if let Some(r) = a.checked_rem_euclid(*b as i128) {
                    Integer::from_double(r)
                } else {
                    Integer::zero()
                }
            },
            (Integer::Double(a), Integer::Double(b)) => {
                if let Some(r) = a.checked_rem_euclid(*b) {
                    Integer::from_double(r)
                } else {
                    Integer::zero()
                }
            },
            (Integer::Large(a), Integer::Natural(b)) => Integer::from(a.rem_euc(b).complete()),
            (Integer::Large(a), Integer::Double(b)) => Integer::from(a.rem_euc(b).complete()),
            (Integer::Large(a), Integer::Large(b)) => Integer::from(a.rem_euc(b).complete()),
        }
    }
}

pub(crate) fn gcd_unsigned(mut a: u64, mut b: u64) -> u64 {
    let mut c;
    while a != 0 {
        c = a;
        a = b % a;
        b = c;
    }
    b
}

pub(crate) fn extended_gcd(mut a: i64, mut b: i64) -> (u64, i64, i64) {
    if a.unsigned_abs() < b.unsigned_abs() {
        let (g, s, t) = extended_gcd(b, a);
        return (g, t, s);
    }

    if a == i64::MIN {
        if b == -1 {
            return (1, 0, -1);
        } else if b == i64::MAX {
            return (1, -1, -1);
        }
    }

    let mut s0 = 1;
    let mut s1 = 0;
    let mut t0 = 0;
    let mut t1 = 1;

    while b != 0 {
        let q = a / b;
        (a, b) = (b, a - q * b);
        (s0, s1) = (s1, s0 - q * s1);
        (t0, t1) = (t1, t0 - q * t1);
    }

    (a.unsigned_abs(), s0, t0)
}

pub(crate) fn extended_gcd_i128(mut a: i128, mut b: i128) -> (u128, i128, i128) {
    if a.unsigned_abs() < b.unsigned_abs() {
        let (g, s, t) = extended_gcd_i128(b, a);
        return (g, t, s);
    }

    if a == i128::MIN {
        if b == -1 {
            return (1, 0, -1);
        } else if b == i128::MAX {
            return (1, -1, -1);
        }
    }

    let mut s0 = 1;
    let mut s1 = 0;
    let mut t0 = 0;
    let mut t1 = 1;

    while b != 0 {
        let q = a / b;
        (a, b) = (b, a - q * b);
        (s0, s1) = (s1, s0 - q * s1);
        (t0, t1) = (t1, t0 - q * t1);
    }

    (a.unsigned_abs(), s0, t0)
}

pub(crate) fn gcd_signed(mut a: i64, mut b: i64) -> u64 {
    let mut c;
    while a != 0 {
        c = a;
        a = b.wrapping_rem(a);
        b = c;
    }
    b.unsigned_abs()
}

pub(crate) fn gcd_signed_i128(mut a: i128, mut b: i128) -> u128 {
    let mut c;
    while a != 0 {
        c = a;
        a = b.wrapping_rem(a);
        b = c;
    }
    b.unsigned_abs()
}
