use thiserror::Error;

use crate::{Exponent, MonomialOrder, MultivariatePolynomial, Ring};

#[derive(Debug, Error)]
pub enum InconsistentPolynomial<R: Ring, E: Exponent, O: MonomialOrder> {
    #[error("Zero coefficient: {0}")]
    ZeroCoefficient(MultivariatePolynomial<R, E, O>),
    #[error("Equal monomials: {0}")]
    EqualMonomials(MultivariatePolynomial<R, E, O>),
    #[error("Wrong monomial ordering: {0}")]
    WrongMonomialOrdering(MultivariatePolynomial<R, E, O>),
}
