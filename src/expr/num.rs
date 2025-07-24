use std::cmp::Ordering;
use std::ops;

use ordered_float::OrderedFloat;

#[derive(Clone, PartialEq, Eq)]
pub enum Num {
    Integer(i32),
    Float(OrderedFloat<f32>),
    Rational(i32, i32),
}

impl ops::Add for &Num {
    type Output = Num;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Num::Integer(i1), Num::Integer(i2)) => Num::Integer(i1 + i2),
            (Num::Float(f1), Num::Float(f2)) => Num::Float(f1 + f2),
            (Num::Rational(r1n, r1d), Num::Rational(r2n, r2d)) => Num::Rational(r1n * r2d + r2n * r1d, r1d * r2d),
            (Num::Integer(int), Num::Float(float)) | (Num::Float(float), Num::Integer(int)) => {
                Num::Float(float + OrderedFloat(*int as f32))
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                Num::Rational(int * rd + rn, *rd)
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                Num::Float(float + OrderedFloat(*rn as f32 / *rd as f32))
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
                Num::Float(float * OrderedFloat(*int as f32))
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                Num::Rational(rn * int, *rd)
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                Num::Float(float * OrderedFloat(*rn as f32 / *rd as f32))
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
                OrderedFloat(*int as f32).cmp(float)
            },
            (Num::Integer(int), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Integer(int)) => {
                (int * rd).cmp(rn)
            },
            (Num::Float(float), Num::Rational(rn, rd)) | (Num::Rational(rn, rd), Num::Float(float)) => {
                (float * OrderedFloat(*rd as f32)).cmp(&OrderedFloat(*rn as f32))
            },
        }
    }
}

impl Num {
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
}
