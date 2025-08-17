use std::cmp::Ordering;
use std::ops;

use ordered_float::OrderedFloat;

use crate::Integer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Num {
    Integer(Integer),
    Float(OrderedFloat<f32>),
    Rational(Integer, Integer),
}

impl ops::Add for &Num {
    type Output = Num;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Num::Integer(i1), Num::Integer(i2)) => Num::Integer(i1 + i2),
            (Num::Float(f1), Num::Float(f2)) => Num::Float(f1 + f2),
            (Num::Rational(r1n, r1d), Num::Rational(r2n, r2d)) => Num::Rational(r1n * r2d + r2n * r1d, r1d * r2d),
            (Num::Integer(int), Num::Float(float)) | (Num::Float(float), Num::Integer(int)) => {
                Num::Float(float + OrderedFloat(int.to_f64() as f32))
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                Num::Rational(int * rd + rn, rd.clone())
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                Num::Float(float + OrderedFloat(rn.to_f64() as f32 / rd.to_f64() as f32))
            },
        }
    }
}

impl ops::Mul for &Num {
    type Output = Num;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Num::Integer(i1), Num::Integer(i2)) => Num::Integer(i1 * i2),
            (Num::Float(f1), Num::Float(f2)) => Num::Float(f1 * f2),
            (Num::Rational(r1n, r1d), Num::Rational(r2n, r2d)) => Num::Rational(r1n * r2n, r1d * r2d),
            (Num::Integer(int), Num::Float(float)) | (Num::Float(float), Num::Integer(int)) => {
                Num::Float(float * OrderedFloat(int.to_f64() as f32))
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                Num::Rational(rn * int, rd.clone())
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                Num::Float(float * OrderedFloat(rn.to_f64() as f32 / rd.to_f64() as f32))
            },
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Num {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Num::Integer(i1), Num::Integer(i2)) => i1.cmp(i2),
            (Num::Float(f1), Num::Float(f2)) => f1.cmp(f2),
            (Num::Rational(r1n, r1d), Num::Rational(r2n, r2d)) => (r1n * r2d).cmp(&(r2n * r1d)),
            (Num::Integer(int), Num::Float(float)) | (Num::Float(float), Num::Integer(int)) => {
                OrderedFloat(int.to_f64() as f32).cmp(float)
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                (int * rd).cmp(rn)
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                (float * OrderedFloat(rd.to_f64() as f32)).cmp(&OrderedFloat(rn.to_f64() as f32))
            },
        }
    }
}

impl Num {
    pub const ZERO: Num = Num::Integer(Integer::zero());
    pub const ONE: Num = Num::Integer(Integer::one());

    pub fn is_one(&self) -> bool {
        match self {
            Num::Integer(int) => *int == 1,
            Num::Float(float) => (float - 1.0).abs() <= 2e-4,
            Num::Rational(n, d) => *n == *d,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Num::Integer(int) => *int == 0,
            Num::Float(float) => float.abs() <= 2e-4,
            Num::Rational(n, d) => *n == 0 && *d != 0,
        }
    }

    pub fn is_negative_one(&self) -> bool {
        match self {
            Num::Integer(int) => *int == -1,
            Num::Float(float) => (float + 1.0).abs() <= 2e-4,
            Num::Rational(n, d) => n.abs() == d.abs() && n.is_negative() ^ d.is_negative(),
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Num::Integer(int) => int.is_negative(),
            Num::Float(float) => float.is_sign_negative(),
            Num::Rational(n, d) => n.is_negative() ^ d.is_negative(),
        }
    }

    pub fn abs(&self) -> Num {
        match self {
            Num::Integer(int) => Num::Integer(int.abs()),
            Num::Float(float) => Num::Float(OrderedFloat(float.abs())),
            Num::Rational(n, d) => Num::Rational(n.abs(), d.abs()),
        }
    }

    /// Raises one number to the power of another, returning the resulting coefficient, base, and
    /// exponent.
    pub fn pow(&self, other: &Num) -> (Num, Num, Num) {
        match (self, other) {
            (Num::Integer(i1), Num::Integer(i2)) => {
                if i2.is_negative() {
                    (
                        Num::ONE,
                        Num::Rational(Integer::one(), i1.pow(i2.abs().to_u32().unwrap())),
                        Num::ONE,
                    )
                } else {
                    (Num::ONE, Num::Integer(i1.pow(i2.to_u32().unwrap())), Num::ONE)
                }
            },
            (Num::Float(f1), Num::Float(f2)) => (Num::ONE, Num::Float(OrderedFloat(f1.powf(**f2))), Num::ONE),
            (Num::Integer(int), Num::Float(float)) => (
                Num::ONE,
                Num::Float(OrderedFloat(OrderedFloat(int.to_f64() as f32).powf(**float))),
                Num::ONE,
            ),
            (Num::Float(float), Num::Integer(int)) => (
                Num::ONE,
                Num::Float(OrderedFloat(
                    float.powi(int.to_i64().and_then(|x| x.try_into().ok()).unwrap()),
                )),
                Num::ONE,
            ),
            (Num::Float(float), Num::Rational(n, d)) => (
                Num::ONE,
                Num::Float(OrderedFloat(float.powf(n.to_f64() as f32 / d.to_f64() as f32))),
                Num::ONE,
            ),
            (Num::Rational(n, d), Num::Float(float)) => (
                Num::ONE,
                Num::Float(OrderedFloat((n.to_f64() as f32 / d.to_f64() as f32).powf(**float))),
                Num::ONE,
            ),
            (Num::Integer(int), Num::Rational(n, d)) => todo!(),
            (Num::Rational(n, d), Num::Integer(int)) => {
                if int.is_negative() {
                    let e = int.abs().to_u32().unwrap();
                    (Num::ONE, Num::Rational(d.pow(e), n.pow(e)), Num::ONE)
                } else {
                    let e = int.to_u32().unwrap();
                    (Num::ONE, Num::Rational(n.pow(e), d.pow(e)), Num::ONE)
                }
            },
            (Num::Rational(n1, d1), Num::Rational(n2, d2)) => todo!(),
        }
    }
}
