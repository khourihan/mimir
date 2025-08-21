use std::{
    fmt::{Debug, Display, LowerExp, Write},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
};

use crate::{InternalOrdering, NumericalFloatLike, Rational, Real, SingleFloat};

/// A complex number.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Complex<T> {
    pub re: T,
    pub im: T,
}

impl<T: Default> Default for Complex<T> {
    fn default() -> Self {
        Self {
            re: T::default(),
            im: T::default(),
        }
    }
}

impl<T: InternalOrdering> InternalOrdering for Complex<T> {
    fn internal_cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.re
            .internal_cmp(&other.re)
            .then_with(|| self.im.internal_cmp(&other.im))
    }
}

impl<T: NumericalFloatLike> Complex<T> {
    #[inline]
    pub fn new(re: T, im: T) -> Complex<T> {
        Complex { re, im }
    }

    #[inline]
    pub fn zero() -> Self {
        Complex {
            re: T::zero(),
            im: T::zero(),
        }
    }

    #[inline]
    pub fn i() -> Self {
        Complex {
            re: T::zero(),
            im: T::one(),
        }
    }

    #[inline]
    pub fn one() -> Self {
        Complex {
            re: T::one(),
            im: T::zero(),
        }
    }

    #[inline]
    pub fn conjugate(&self) -> Self {
        Complex {
            re: self.re.clone(),
            im: -self.im.clone(),
        }
    }

    #[inline]
    pub fn length_squared(&self) -> T {
        self.re.clone() * &self.re + self.im.clone() * &self.im
    }
}

impl<T: Real> Complex<T> {
    #[inline]
    pub fn arg(&self) -> T {
        self.im.atan2(&self.re)
    }

    #[inline]
    pub fn to_polar(self) -> (T, T) {
        (self.length_squared().sqrt(), self.arg())
    }

    #[inline]
    pub fn from_polar(r: T, phi: T) -> Complex<T> {
        Complex::new(r.clone() * phi.cos(), r.clone() * phi.sin())
    }
}

impl<T: SingleFloat> Complex<T> {
    pub fn is_real(&self) -> bool {
        self.im.is_zero()
    }

    #[inline]
    pub fn to_real(&self) -> Option<&T> {
        if self.im.is_zero() { Some(&self.re) } else { None }
    }
}

macro_rules! impl_op {
    ($trait:ident, $f:ident, |$self:ident: $selft:ident $(<$selfg:ident>)?, $rhs:ident: $rhst:ident $(<$rhsg:ident>)?| => $e:expr) => {
        impl<T: NumericalFloatLike> $trait<$rhst $(<$rhsg>)?> for $selft $(<$selfg>)? {
            type Output = Complex<T>;

            #[inline]
            fn $f($self, $rhs: $rhst $(<$rhsg>)?) -> Self::Output {
                $e
            }
        }
    };
    ($trait:ident, $f:ident, |$self:ident: $selft:ident $(<$selfg:ident>)?, $rhs:ident: &$rhst:ident $(<$rhsg:ident>)?| => $e:expr) => {
        impl<T: NumericalFloatLike> $trait<&$rhst $(<$rhsg>)?> for $selft $(<$selfg>)? {
            type Output = Complex<T>;

            #[inline]
            fn $f($self, $rhs: &$rhst $(<$rhsg>)?) -> Self::Output {
                $e
            }
        }
    };
    ($trait:ident, $f:ident, |$self:ident: &$selft:ident $(<$selfg:ident>)?, $rhs:ident: &$rhst:ident $(<$rhsg:ident>)?| => $e:expr) => {
        impl<'a, T: NumericalFloatLike> $trait<&'a $rhst $(<$rhsg>)?> for &$selft $(<$selfg>)? {
            type Output = Complex<T>;

            #[inline]
            fn $f($self, $rhs: &'a $rhst $(<$rhsg>)?) -> Self::Output {
                $e
            }
        }
    };
    ($trait:ident, $f:ident, |$self:ident: &$selft:ident $(<$selfg:ident>)?, $rhs:ident: $rhst:ident $(<$rhsg:ident>)?| => $e:expr) => {
        impl<T: NumericalFloatLike> $trait<$rhst $(<$rhsg>)?> for &$selft $(<$selfg>)? {
            type Output = Complex<T>;

            #[inline]
            fn $f($self, $rhs: $rhst $(<$rhsg>)?) -> Self::Output {
                $e
            }
        }
    };
}

impl_op!(Add, add, |self: Complex<T>, rhs: Complex<T>| => Complex::new(self.re + rhs.re, self.im + rhs.im));
impl_op!(Add, add, |self: Complex<T>, rhs: T| => Complex::new(self.re + rhs, self.im));
impl_op!(Add, add, |self: Complex<T>, rhs: &Complex<T>| => Complex::new(self.re + &rhs.re, self.im + &rhs.im));
impl_op!(Add, add, |self: Complex<T>, rhs: &T| => Complex::new(self.re + rhs, self.im));
impl_op!(Add, add, |self: &Complex<T>, rhs: &Complex<T>| => self.clone() + rhs);
impl_op!(Add, add, |self: &Complex<T>, rhs: &T| => self.clone() + rhs);
impl_op!(Add, add, |self: &Complex<T>, rhs: Complex<T>| => self.clone() + rhs);
impl_op!(Add, add, |self: &Complex<T>, rhs: T| => self.clone() + rhs);
impl_op!(Sub, sub, |self: Complex<T>, rhs: Complex<T>| => Complex::new(self.re - rhs.re, self.im - rhs.im));
impl_op!(Sub, sub, |self: Complex<T>, rhs: T| => Complex::new(self.re - rhs, self.im));
impl_op!(Sub, sub, |self: Complex<T>, rhs: &Complex<T>| => Complex::new(self.re - &rhs.re, self.im - &rhs.im));
impl_op!(Sub, sub, |self: Complex<T>, rhs: &T| => Complex::new(self.re - rhs, self.im));
impl_op!(Sub, sub, |self: &Complex<T>, rhs: &Complex<T>| => self.clone() - rhs);
impl_op!(Sub, sub, |self: &Complex<T>, rhs: &T| => self.clone() - rhs);
impl_op!(Sub, sub, |self: &Complex<T>, rhs: Complex<T>| => self.clone() - rhs);
impl_op!(Sub, sub, |self: &Complex<T>, rhs: T| => self.clone() - rhs);
impl_op!(Mul, mul, |self: Complex<T>, rhs: Complex<T>| => self.mul(&rhs));
impl_op!(Mul, mul, |self: Complex<T>, rhs: T| => Complex::new(self.re * &rhs, self.im * &rhs));
impl_op!(Mul, mul, |self: Complex<T>, rhs: &Complex<T>| => {
    Complex::new(
        self.re.clone() * &rhs.re - self.im.clone() * &rhs.im,
        self.re.clone() * &rhs.im + self.im.clone() * &rhs.re,
    )
});
impl_op!(Mul, mul, |self: Complex<T>, rhs: &T| => Complex::new(self.re * rhs, self.im * rhs));
impl_op!(Mul, mul, |self: &Complex<T>, rhs: &Complex<T>| => self.clone() * rhs);
impl_op!(Mul, mul, |self: &Complex<T>, rhs: &T| => self.clone() * rhs);
impl_op!(Mul, mul, |self: &Complex<T>, rhs: Complex<T>| => self.clone() * rhs);
impl_op!(Mul, mul, |self: &Complex<T>, rhs: T| => self.clone() * rhs);
impl_op!(Div, div, |self: Complex<T>, rhs: Complex<T>| => self.div(&rhs));
impl_op!(Div, div, |self: Complex<T>, rhs: T| => Complex::new(self.re / &rhs, self.im / &rhs));
impl_op!(Div, div, |self: Complex<T>, rhs: &Complex<T>| => {
    let n = rhs.length_squared();
    let re = self.re.clone() * &rhs.re + self.im.clone() * &rhs.im;
    let im = self.im.clone() * &rhs.re - self.re.clone() * &rhs.im;
    Complex::new(re / &n, im / &n)
});
impl_op!(Div, div, |self: Complex<T>, rhs: &T| => Complex::new(self.re / rhs, self.im / rhs));
impl_op!(Div, div, |self: &Complex<T>, rhs: &Complex<T>| => self.clone() / rhs);
impl_op!(Div, div, |self: &Complex<T>, rhs: &T| => self.clone() / rhs);
impl_op!(Div, div, |self: &Complex<T>, rhs: Complex<T>| => self.clone() / rhs);
impl_op!(Div, div, |self: &Complex<T>, rhs: T| => self.clone() / rhs);

macro_rules! impl_op_assign {
    ($trait:ident, $f:ident, |$self:ident: $selft:ident $(<$selfg:ident>)?, $rhs:ident: $rhst:ident $(<$rhsg:ident>)?| => $e:block) => {
        impl<T: NumericalFloatLike> $trait<$rhst $(<$rhsg>)?> for $selft $(<$selfg>)? {
            #[inline]
            fn $f(&mut $self, $rhs: $rhst $(<$rhsg>)?) $e
        }
    };
    ($trait:ident, $f:ident, |$self:ident: $selft:ident $(<$selfg:ident>)?, $rhs:ident: &$rhst:ident $(<$rhsg:ident>)?| => $e:block) => {
        impl<T: NumericalFloatLike> $trait<&$rhst $(<$rhsg>)?> for $selft $(<$selfg>)? {
            #[inline]
            fn $f(&mut $self, $rhs: &$rhst $(<$rhsg>)?) $e
        }
    };
}

impl_op_assign!(AddAssign, add_assign, |self: Complex<T>, rhs: Complex<T>| => {
    self.add_assign(&rhs)
});
impl_op_assign!(AddAssign, add_assign, |self: Complex<T>, rhs: T| => {
    self.re += rhs;
});
impl_op_assign!(AddAssign, add_assign, |self: Complex<T>, rhs: &Complex<T>| => {
    self.re += &rhs.re;
    self.im += &rhs.im;
});
impl_op_assign!(AddAssign, add_assign, |self: Complex<T>, rhs: &T| => {
    self.re += rhs;
});
impl_op_assign!(SubAssign, sub_assign, |self: Complex<T>, rhs: Complex<T>| => {
    self.sub_assign(&rhs)
});
impl_op_assign!(SubAssign, sub_assign, |self: Complex<T>, rhs: T| => {
    self.re -= rhs;
});
impl_op_assign!(SubAssign, sub_assign, |self: Complex<T>, rhs: &Complex<T>| => {
    self.re -= &rhs.re;
    self.im -= &rhs.im;
});
impl_op_assign!(SubAssign, sub_assign, |self: Complex<T>, rhs: &T| => {
    self.re -= rhs;
});
impl_op_assign!(MulAssign, mul_assign, |self: Complex<T>, rhs: Complex<T>| => {
    *self = self.clone().mul(rhs);
});
impl_op_assign!(MulAssign, mul_assign, |self: Complex<T>, rhs: T| => {
    *self = self.clone().mul(rhs);
});
impl_op_assign!(MulAssign, mul_assign, |self: Complex<T>, rhs: &Complex<T>| => {
    *self = self.clone().mul(rhs);
});
impl_op_assign!(MulAssign, mul_assign, |self: Complex<T>, rhs: &T| => {
    *self = self.clone().mul(rhs);
});
impl_op_assign!(DivAssign, div_assign, |self: Complex<T>, rhs: Complex<T>| => {
    *self = self.clone().div(rhs);
});
impl_op_assign!(DivAssign, div_assign, |self: Complex<T>, rhs: T| => {
    *self = self.clone().div(rhs);
});
impl_op_assign!(DivAssign, div_assign, |self: Complex<T>, rhs: &Complex<T>| => {
    *self = self.clone().div(rhs);
});
impl_op_assign!(DivAssign, div_assign, |self: Complex<T>, rhs: &T| => {
    *self = self.clone().div(rhs);
});

impl<T: NumericalFloatLike> Neg for Complex<T> {
    type Output = Complex<T>;

    #[inline]
    fn neg(self) -> Self::Output {
        Complex::new(-self.re, -self.im)
    }
}

impl<T: Debug> Debug for Complex<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        Debug::fmt(&self.re, f)?;
        f.write_char('+')?;
        Debug::fmt(&self.im, f)?;
        f.write_str("i)")
    }
}

impl<T: SingleFloat> SingleFloat for Complex<T> {
    #[inline(always)]
    fn is_zero(&self) -> bool {
        self.re.is_zero() && self.im.is_zero()
    }

    #[inline(always)]
    fn is_one(&self) -> bool {
        self.re.is_one() && self.im.is_zero()
    }

    #[inline(always)]
    fn is_negative_one(&self) -> bool {
        self.re.is_negative_one() && self.im.is_zero()
    }

    #[inline(always)]
    fn is_finite(&self) -> bool {
        self.re.is_finite() && self.im.is_finite()
    }

    #[inline(always)]
    fn from_rational(rat: &Rational) -> Self {
        Complex {
            re: T::from_rational(rat),
            im: T::zero(),
        }
    }
}

impl<T: NumericalFloatLike> NumericalFloatLike for Complex<T> {
    #[inline]
    fn mul_add(&self, a: &Self, b: &Self) -> Self {
        self.clone() * a + b
    }

    #[inline]
    fn neg(&self) -> Self {
        -self.clone()
    }

    #[inline]
    fn zero() -> Self {
        Complex::zero()
    }

    #[inline]
    fn one() -> Self {
        Complex::one()
    }

    #[inline]
    fn pow(&self, e: u64) -> Self {
        let mut r = Complex::one();
        for _ in 0..e {
            r *= self;
        }
        r
    }

    #[inline]
    fn inv(&self) -> Self {
        let n = self.length_squared();
        Complex::new(self.re.clone() / &n, -self.im.clone() / &n)
    }

    #[inline]
    fn from_usize(a: usize) -> Self {
        Complex {
            re: T::from_usize(a),
            im: T::zero(),
        }
    }

    #[inline]
    fn from_i64(a: i64) -> Self {
        Complex {
            re: T::from_i64(a),
            im: T::zero(),
        }
    }

    #[inline]
    fn epsilon() -> f64 {
        T::epsilon()
    }
}

impl<T: Real> Real for Complex<T> {
    #[inline]
    fn pi() -> Self {
        Complex::new(T::pi(), T::zero())
    }

    #[inline]
    fn e() -> Self {
        Complex::new(T::e(), T::zero())
    }

    #[inline]
    fn euler() -> Self {
        Complex::new(T::euler(), T::zero())
    }

    #[inline]
    fn phi() -> Self {
        Complex::new(T::phi(), T::zero())
    }

    #[inline]
    fn i() -> Option<Self> {
        Some(Complex::i())
    }

    #[inline]
    fn norm(&self) -> Self {
        Complex::new(self.length_squared().sqrt(), T::zero())
    }

    #[inline]
    fn sqrt(&self) -> Self {
        let (r, phi) = self.clone().to_polar();
        Complex::from_polar(r.sqrt(), phi / T::from_usize(2))
    }

    #[inline]
    fn log(&self) -> Self {
        Complex::new(self.norm().re.log(), self.arg())
    }

    #[inline]
    fn exp(&self) -> Self {
        let r = self.re.exp();
        Complex::new(r.clone() * self.im.cos(), r * self.im.sin())
    }

    #[inline]
    fn sin(&self) -> Self {
        Complex::new(self.re.sin() * self.im.cosh(), self.re.cos() * self.im.sinh())
    }

    #[inline]
    fn cos(&self) -> Self {
        Complex::new(self.re.cos() * self.im.cosh(), -self.re.sin() * self.im.sinh())
    }

    #[inline]
    fn tan(&self) -> Self {
        let (r, i) = (self.re.clone() + &self.re, self.im.clone() + &self.im);
        let m = r.cos() + i.cosh();
        Self::new(r.sin() / &m, i.sinh() / m)
    }

    #[inline]
    fn asin(&self) -> Self {
        let i = Self::i();
        -i.clone() * ((Self::one() - self.clone() * self).sqrt() + i * self).log()
    }

    #[inline]
    fn acos(&self) -> Self {
        let i = Self::i();
        -i.clone() * (i * (Self::one() - self.clone() * self).sqrt() + self).log()
    }

    #[inline]
    fn atan2(&self, x: &Self) -> Self {
        // TODO: improve this
        let r = self.clone() / x;
        let i = Self::i();
        let one = Self::one();
        let two = one.clone() + &one;
        // TODO: add edge cases
        ((&one + &i * &r).log() - (&one - &i * r).log()) / (two * i)
    }

    #[inline]
    fn sinh(&self) -> Self {
        Complex::new(self.re.sinh() * self.im.cos(), self.re.cosh() * self.im.sin())
    }

    #[inline]
    fn cosh(&self) -> Self {
        Complex::new(self.re.cosh() * self.im.cos(), self.re.sinh() * self.im.sin())
    }

    #[inline]
    fn tanh(&self) -> Self {
        let (two_re, two_im) = (self.re.clone() + &self.re, self.im.clone() + &self.im);
        let m = two_re.cosh() + two_im.cos();
        Self::new(two_re.sinh() / &m, two_im.sin() / m)
    }

    #[inline]
    fn asinh(&self) -> Self {
        let one = Self::one();
        (self.clone() + (one + self.clone() * self).sqrt()).log()
    }

    #[inline]
    fn acosh(&self) -> Self {
        let one = Self::one();
        let two = one.clone() + &one;
        &two * (((self.clone() + &one) / &two).sqrt() + ((self.clone() - one) / &two).sqrt()).log()
    }

    #[inline]
    fn atanh(&self) -> Self {
        let one = Self::one();
        let two = one.clone() + &one;
        // TODO: add edge cases
        ((&one + self).log() - (one - self).log()) / two
    }

    #[inline]
    fn powf(&self, e: &Self) -> Self {
        if e.re == T::zero() && e.im == T::zero() {
            Self::one()
        } else if e.im == T::zero() {
            let (r, phi) = self.clone().to_polar();
            Self::from_polar(r.powf(&e.re), phi * e.re.clone())
        } else {
            (e * self.log()).exp()
        }
    }
}

impl<T: NumericalFloatLike> From<T> for Complex<T> {
    fn from(value: T) -> Self {
        Complex::new(value, T::zero())
    }
}

impl<'a, T: NumericalFloatLike + From<&'a Rational>> From<&'a Rational> for Complex<T> {
    fn from(value: &'a Rational) -> Self {
        let c: T = value.into();
        Complex::new(c, T::zero())
    }
}

impl Complex<Rational> {
    pub fn gcd(&self, other: &Self) -> Self {
        if self.is_zero() {
            return other.clone();
        }

        if other.is_zero() {
            return self.clone();
        }

        let gcd_re = self.re.gcd(&other.re);
        let gcd_im = self.im.gcd(&other.im);

        Complex::new(gcd_re, gcd_im)
    }
}
