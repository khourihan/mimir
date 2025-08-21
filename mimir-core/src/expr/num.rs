use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg},
    str::FromStr,
};

use crate::{Complex, F64, Integer, NumericalFloatLike, Rational, Real, SingleFloat};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Num {
    Rational(Complex<Rational>),
    Float(Complex<F64>),
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Num {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Num::Rational(r1), Num::Rational(r2)) => r1
                .re
                .partial_cmp(&r2.re)
                .unwrap_or(Ordering::Equal)
                .then_with(|| r1.im.partial_cmp(&r2.im).unwrap_or(Ordering::Equal)),
            (Num::Rational(_), Num::Float(_)) => Ordering::Less,
            (Num::Float(_), Num::Rational(_)) => Ordering::Greater,
            (Num::Float(f1), Num::Float(f2)) => f1
                .re
                .partial_cmp(&f2.re)
                .unwrap_or(Ordering::Equal)
                .then_with(|| f1.im.partial_cmp(&f2.im).unwrap_or(Ordering::Equal)),
        }
    }
}

impl Num {
    pub fn zero() -> Num {
        Num::Rational(Complex::zero())
    }

    pub fn one() -> Num {
        Num::Rational(Complex::one())
    }

    pub fn negative_one() -> Num {
        Num::Rational(Complex::new(Rational::from(-1), Rational::zero()))
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Num::Rational(r) => r.re.is_negative() && r.im.is_zero() || r.im.is_negative() && r.re.is_zero(),
            Num::Float(f) => f.re.is_negative() && f.im.is_zero() || f.im.is_negative() && f.re.is_zero(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Num::Rational(r) => r.is_zero(),
            Num::Float(f) => f.is_zero(),
        }
    }

    pub fn is_one(&self) -> bool {
        match self {
            Num::Rational(r) => r.is_one(),
            Num::Float(f) => f.is_one(),
        }
    }

    pub fn is_negative_one(&self) -> bool {
        match self {
            Num::Rational(r) => r.is_negative_one(),
            Num::Float(f) => f.is_negative_one(),
        }
    }

    pub fn abs_real(&self) -> Self {
        match self {
            Num::Rational(r) => Num::Rational(Complex::new(r.re.abs(), r.im.clone())),
            Num::Float(f) => Num::Float(Complex::new(f.re.abs(), f.im)),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Num::Rational(r) => r.im.is_zero() && r.re.denominator.is_one(),
            Num::Float(_) => false,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            Num::Rational(r) => r.im.is_zero(),
            Num::Float(f) => f.im.is_zero(),
        }
    }

    pub fn gcd(&self, other: &Self) -> Self {
        match (self, other) {
            (Num::Rational(r1), Num::Rational(r2)) => Num::Rational(r1.gcd(r2)),
            _ => Num::Float(Complex::one()),
        }
    }

    pub fn pow(&self, other: &Self) -> (Self, Self, Self) {
        if let Num::Rational(r) = other
            && !r.im.is_zero()
        {
            return (Self::one(), self.clone(), other.clone());
        }

        fn rat_pow(mut base: Rational, mut exp: Rational) -> (Complex<Rational>, Rational, Rational) {
            if base.is_negative() && !exp.is_integer() {
                let pow = &exp.numerator / &exp.denominator;
                let rest = &exp.numerator - &pow * &exp.denominator;

                let base_integer_pow = if pow.is_negative() {
                    base.inv().pow(pow.to_i64().unwrap().unsigned_abs())
                } else {
                    base.pow(pow.to_i64().unwrap().unsigned_abs())
                };

                if exp.denominator == 2 {
                    (
                        if rest.is_negative() {
                            Complex::new(Rational::zero(), -base_integer_pow)
                        } else {
                            Complex::new(Rational::zero(), base_integer_pow)
                        },
                        base.abs(),
                        Rational::from_unchecked(rest, exp.denominator),
                    )
                } else {
                    (
                        base_integer_pow.into(),
                        base,
                        Rational::from_unchecked(rest, exp.denominator),
                    )
                }
            } else {
                if exp < 0.into() {
                    base = base.inv();
                    exp = -exp;
                }

                base = base.pow(exp.numerator.to_i64().unwrap().unsigned_abs());

                (
                    Rational::one().into(),
                    base,
                    Rational::from_unchecked(Integer::one(), exp.denominator),
                )
            }
        }

        match (self, other) {
            (Num::Rational(r1), Num::Rational(r2)) => {
                if r1.im.numerator == 0 {
                    let (coeff, base, exp) = rat_pow(r1.re.clone(), r2.re.clone());
                    (
                        Num::Rational(coeff),
                        Num::Rational(base.into()),
                        Num::Rational(exp.into()),
                    )
                } else if r2.re.denominator == 1 {
                    let r = r1.pow(r2.re.numerator.to_i64().unwrap().unsigned_abs());

                    (
                        Num::one(),
                        if r2.re.numerator < Integer::zero() {
                            Num::Rational(r.inv())
                        } else {
                            Num::Rational(r)
                        },
                        Num::Rational(Rational::one().into()),
                    )
                } else {
                    (Num::one(), Num::Rational(r1.clone()), Num::Rational(r2.clone()))
                }
            },
            (Num::Float(f), Num::Rational(r)) => {
                if r.re.denominator == 1 && r.im.numerator == 0 {
                    let r1 = f.pow(r.re.numerator.to_i64().unwrap().unsigned_abs());
                    (
                        Num::one(),
                        if r.re.numerator < Integer::zero() {
                            Num::Float(r1.inv())
                        } else {
                            Num::Float(r1)
                        },
                        Num::one(),
                    )
                } else {
                    (
                        Num::one(),
                        Num::Float(f.powf(&Complex::new(r.re.clone().into(), r.im.clone().into()))),
                        Num::one(),
                    )
                }
            },
            (Num::Rational(r), Num::Float(f)) => (
                Num::one(),
                Num::Float(Complex::new(r.re.clone().into(), r.im.clone().into()).powf(f)),
                Num::one(),
            ),
            (Num::Float(f1), Num::Float(f2)) => {
                let p = if f1.im.is_zero() && f2.im.is_zero() {
                    f1.re.powf(&f2.re).into()
                } else {
                    f1.powf(f2)
                };

                (Num::one(), Num::Float(p), Num::one())
            },
        }
    }

    pub fn parse_float(s: &str) -> Result<Self, <F64 as FromStr>::Err> {
        Ok(Num::Float(Complex::new(F64::from_str(s)?, F64::zero())))
    }

    pub fn parse_int(s: &str) -> Result<Self, <Integer as FromStr>::Err> {
        Ok(Num::Rational(Complex::new(
            Rational::new(Integer::from_str(s)?, Integer::one()),
            Rational::zero(),
        )))
    }
}

impl Neg for Num {
    type Output = Num;

    fn neg(self) -> Self::Output {
        match self {
            Num::Rational(r) => Num::Rational(-r),
            Num::Float(f) => Num::Float(-f),
        }
    }
}

impl<'a> Add<&'a Num> for &Num {
    type Output = Num;

    fn add(self, rhs: &'a Num) -> Self::Output {
        match (self, rhs) {
            (Num::Rational(r1), Num::Rational(r2)) => Num::Rational(r1 + r2),
            (Num::Rational(r), Num::Float(f)) | (Num::Float(f), Num::Rational(r)) => Num::Float(Complex::new(
                f.re + F64::from(r.re.clone()),
                f.im + F64::from(r.im.clone()),
            )),
            (Num::Float(f1), Num::Float(f2)) => Num::Float(f1 + f2),
        }
    }
}

impl<'a> Mul<&'a Num> for &Num {
    type Output = Num;

    fn mul(self, rhs: &'a Num) -> Self::Output {
        match (self, rhs) {
            (Num::Rational(r1), Num::Rational(r2)) => Num::Rational(r1 * r2),
            (Num::Rational(r), Num::Float(f)) | (Num::Float(f), Num::Rational(r)) => Num::Float(Complex::new(
                f.re * F64::from(&r.re) - f.im * F64::from(&r.im),
                f.re * F64::from(&r.im) + f.im * F64::from(&r.re),
            )),
            (Num::Float(f1), Num::Float(f2)) => Num::Float(f1 * f2),
        }
    }
}
