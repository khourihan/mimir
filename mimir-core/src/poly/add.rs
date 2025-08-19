use std::{
    cmp::Ordering,
    fmt::Debug,
    marker::PhantomData,
    ops::{Add, Neg, Sub},
};

use crate::{Exponent, MonomialOrder, MultivariatePolynomial, Ring};

impl<'a, R: Ring + Debug, E: Exponent, O: MonomialOrder> Add<&'a MultivariatePolynomial<R, E, O>>
    for &MultivariatePolynomial<R, E, O>
{
    type Output = MultivariatePolynomial<R, E, O>;

    fn add(self, other: &'a MultivariatePolynomial<R, E, O>) -> Self::Output {
        assert_eq!(self.ring, other.ring);

        if self.is_zero() {
            return other.clone();
        }

        if other.is_zero() {
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

        let mut new_nterms = 0;
        let mut i = 0;
        let mut j = 0;

        macro_rules! insert_monomial {
            ($source:expr, $index:expr) => {
                new_coeffs[new_nterms] = $source.coefficients[$index].clone();
                new_exps[new_nterms * $source.nvars()..(new_nterms + 1) * $source.nvars()]
                    .clone_from_slice($source.exponents($index));
                new_nterms += 1;
            };
        }

        while i < self.nterms() && j < other.nterms() {
            let c = O::cmp(self.exponents(i), other.exponents(j));

            match c {
                Ordering::Less => {
                    insert_monomial!(self, i);
                    i += 1;
                },
                Ordering::Greater => {
                    insert_monomial!(other, j);
                    j += 1;
                },
                Ordering::Equal => {
                    let coeff = self.ring.add(&self.coefficients[i], &other.coefficients[j]);
                    if !self.ring.is_zero(&coeff) {
                        new_coeffs[new_nterms] = coeff;
                        new_exps[new_nterms * self.nvars()..(new_nterms + 1) * self.nvars()]
                            .clone_from_slice(self.exponents(i));
                        new_nterms += 1;
                    }

                    i += 1;
                    j += 1;
                },
            }
        }

        while i < self.nterms() {
            insert_monomial!(self, i);
            i += 1;
        }

        while j < other.nterms() {
            insert_monomial!(other, j);
            j += 1;
        }

        new_coeffs.truncate(new_nterms);
        new_exps.truncate(self.nvars() * new_nterms);

        MultivariatePolynomial {
            coefficients: new_coeffs,
            exponents: new_exps,
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<'a, R: Ring + Debug, E: Exponent, O: MonomialOrder> Sub<&'a MultivariatePolynomial<R, E, O>>
    for &MultivariatePolynomial<R, E, O>
{
    type Output = MultivariatePolynomial<R, E, O>;

    fn sub(self, other: &'a MultivariatePolynomial<R, E, O>) -> Self::Output {
        self.add(&other.clone().neg())
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> Neg for MultivariatePolynomial<R, E, O> {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        for c in &mut self.coefficients {
            *c = self.ring.neg(c);
        }

        self
    }
}
