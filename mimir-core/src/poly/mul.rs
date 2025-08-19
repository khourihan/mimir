use std::{fmt::Debug, ops::Mul};

use crate::{Exponent, MonomialOrder, MultivariatePolynomial, Ring};

impl<R: Ring, E: Exponent, O: MonomialOrder> Mul<R::Element> for MultivariatePolynomial<R, E, O> {
    type Output = MultivariatePolynomial<R, E, O>;

    fn mul(mut self, other: R::Element) -> Self::Output {
        if self.ring.is_one(&other) {
            return self;
        }

        for c in &mut self.coefficients {
            self.ring.mul_assign(c, &other);
        }

        for i in (0..self.nterms()).rev() {
            if self.ring.is_zero(&self.coefficients[i]) {
                self.coefficients.remove(i);
                self.exponents.drain(i * self.nvars()..(i + 1) * self.nvars());
            }
        }

        self
    }
}

impl<'a, R: Ring + Debug, E: Exponent, O: MonomialOrder> Mul<&'a MultivariatePolynomial<R, E, O>>
    for &MultivariatePolynomial<R, E, O>
{
    type Output = MultivariatePolynomial<R, E, O>;

    fn mul(self, other: &'a MultivariatePolynomial<R, E, O>) -> Self::Output {
        assert_eq!(self.ring, other.ring);

        if self.is_zero() {
            return other.zeroed();
        }

        if other.is_zero() {
            return self.zeroed();
        }

        if self.is_one() {
            return other.clone();
        }

        if other.is_one() {
            return self.clone();
        }

        if self.variables != other.variables {
            let mut c1 = self.clone();
            let mut c2 = other.clone();
            c1.combine_variables(&mut c2);
            return &c1 + &c2;
        }

        let mut new_coeffs = vec![self.ring.zero(); self.nterms() + other.nterms()];
        let mut new_exps = vec![E::zero(); self.nvars() * (self.nterms() + other.nterms())];
        let mut s = self.clone();

        for i in 0..other.nterms() {
            s = s.mul_monomial(&other.coefficients[i], other.exponents(i));
        }

        s
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> MultivariatePolynomial<R, E, O> {
    /// Add `exponents` to every exponent.
    pub fn mul_exp(mut self, exponents: &[E]) -> Self {
        debug_assert_eq!(self.nvars(), exponents.len());

        if self.nvars() == 0 {
            return self;
        }

        for e in self.iter_exponents_mut() {
            for (e1, e2) in e.iter_mut().zip(exponents) {
                *e1 = e1.checked_add(e2).expect("overflow in adding exponents");
            }
        }

        self
    }

    #[inline]
    fn mul_monomial(self, coefficient: &R::Element, exponents: &[E]) -> Self {
        self.mul(coefficient.clone()).mul_exp(exponents)
    }
}
