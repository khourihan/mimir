use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::Debug,
    hash::Hash,
    hint::assert_unchecked,
    marker::PhantomData,
    ops::{Add, Div, Mul, MulAssign, Neg, Sub},
    sync::Arc,
};

use crate::{
    EuclideanDomain, Exponent, InconsistentPolynomial, InternalOrdering, LexOrder, MonomialOrder, PositiveExponent,
    Ring, Symbol,
};

#[derive(Clone)]
pub struct MultivariatePolynomial<R: Ring, E: Exponent = u16, O: MonomialOrder = LexOrder> {
    pub coefficients: Vec<R::Element>,
    pub exponents: Vec<E>,
    pub ring: R,
    pub variables: Arc<Vec<Symbol>>,
    pub(super) _phantom: PhantomData<O>,
}

impl<R: Ring, E: Exponent, O: MonomialOrder> MultivariatePolynomial<R, E, O> {
    /// Creates a new zeroed polynomial from a given capacity without inheriting a field or variable map.
    #[inline]
    pub fn new(ring: &R, cap: Option<usize>, variables: Arc<Vec<Symbol>>) -> Self {
        Self {
            coefficients: Vec::with_capacity(cap.unwrap_or(0)),
            exponents: Vec::with_capacity(cap.unwrap_or(0) * variables.len()),
            ring: ring.clone(),
            variables,
            _phantom: PhantomData,
        }
    }

    /// Creates a new zeroed polynomial without inheriting a field or variable map.
    #[inline]
    pub fn zero(ring: &R) -> Self {
        Self {
            coefficients: vec![],
            exponents: vec![],
            ring: ring.clone(),
            variables: Arc::new(vec![]),
            _phantom: PhantomData,
        }
    }

    /// Creates a new one polynomial without inheriting a field or variable map.
    #[inline]
    pub fn one(ring: &R) -> Self {
        Self {
            coefficients: vec![ring.one()],
            exponents: vec![],
            ring: ring.clone(),
            variables: Arc::new(vec![]),
            _phantom: PhantomData,
        }
    }

    /// Creates a new zeroed polynomial, inheriting the field and variable map from `self`.
    #[inline]
    pub fn zeroed(&self) -> Self {
        Self {
            coefficients: vec![],
            exponents: vec![],
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }

    /// Creates a new one polynomial, inheriting the field and variable map from `self`.
    #[inline]
    pub fn oned(&self) -> Self {
        Self {
            coefficients: vec![self.ring.one()],
            exponents: vec![E::zero(); self.nvars()],
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }

    /// Creates a new zeroed polynomial with the given capacity, inheriting the field and variable map from `self`.
    #[inline]
    pub fn zeroed_with_capacity(&self, cap: usize) -> Self {
        Self {
            coefficients: Vec::with_capacity(cap),
            exponents: Vec::with_capacity(cap * self.nvars()),
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }

    /// Creates a new constant polynomial, inheriting the field and variable map from `self`.
    #[inline]
    pub fn constant(&self, coeff: R::Element) -> Self {
        if self.ring.is_zero(&coeff) {
            return self.zeroed();
        }

        Self {
            coefficients: vec![coeff],
            exponents: vec![E::zero(); self.nvars()],
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }

    /// Creates a new polynomial with a single term, inheriting the field and variable map from `self`.
    #[inline]
    pub fn monomial(&self, coeff: R::Element, exponents: Vec<E>) -> Self {
        debug_assert!(self.nvars() == exponents.len());

        if self.ring.is_zero(&coeff) {
            return self.zeroed();
        }

        Self {
            coefficients: vec![coeff],
            exponents,
            ring: self.ring.clone(),
            variables: self.variables.clone(),
            _phantom: PhantomData,
        }
    }

    /// Creates a new polynomial with a single term that is a variable, inheriting the field and
    /// variable map from `self`.
    #[inline]
    pub fn variable(&self, var: &Symbol) -> Option<Self> {
        if let Some(pos) = self.variables.iter().position(|v| v == var) {
            let mut exp = vec![E::zero(); self.nvars()];
            exp[pos] = E::one();
            Some(self.monomial(self.ring.one(), exp))
        } else {
            None
        }
    }

    #[inline]
    pub fn reserve(&mut self, cap: usize) -> &mut Self {
        self.coefficients.reserve(cap);
        self.exponents.reserve(cap * self.nvars());
        self
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        self.nterms() == 0
    }

    #[inline]
    pub fn is_one(&self) -> bool {
        self.nterms() == 1 && self.ring.is_one(&self.coefficients[0]) && self.exponents.iter().all(|x| x.is_zero())
    }

    /// The number of terms in this polynomial.
    #[inline]
    pub fn nterms(&self) -> usize {
        self.coefficients.len()
    }

    /// The number of variables in this polynomial.
    #[inline]
    pub fn nvars(&self) -> usize {
        self.variables.len()
    }

    /// Whether or not this polynomial is constant.
    #[inline]
    pub fn is_constant(&self) -> bool {
        if self.is_zero() {
            return true;
        }

        if self.nterms() >= 2 {
            return false;
        }

        debug_assert!(!self.ring.is_zero(self.coefficients.first().unwrap()));
        self.exponents.iter().all(|e| e.is_zero())
    }

    /// Get the constant term of the polynomial.
    #[inline]
    pub fn get_constant(&self) -> R::Element {
        if self.is_zero() || !self.exponents(0).iter().all(|e| e.is_zero()) {
            return self.ring.zero();
        }

        self.coefficients[0].clone()
    }

    /// Get the `i`th monomial, starting from the back.
    #[inline]
    pub fn coefficient_back(&self, i: usize) -> &R::Element {
        &self.coefficients[self.nterms() - i - 1]
    }

    /// Get a slice of the exponents of the `i`th monomial.
    #[inline]
    pub fn exponents(&self, i: usize) -> &[E] {
        &self.exponents[i * self.nvars()..(i + 1) * self.nvars()]
    }

    /// Get a slice of the exponents of the `i`th monomial, starting from the back.
    #[inline]
    pub fn exponents_back(&self, i: usize) -> &[E] {
        let i = self.nterms() - i - 1;
        &self.exponents[i * self.nvars()..(i + 1) * self.nvars()]
    }

    /// Get a mutable slice of the exponents of the `i`th monomial, starting from the back.
    #[inline]
    pub fn exponents_mut(&mut self, i: usize) -> &mut [E] {
        let nvars = self.nvars();
        &mut self.exponents[i * nvars..(i + 1) * nvars]
    }

    /// Returns an iterator over the exponents of every monomial.
    #[inline]
    pub fn iter_exponents(&self) -> impl Iterator<Item = &[E]> {
        self.exponents.chunks(self.nvars())
    }

    /// Returns an iterator over the mutable exponents of every monomial.
    #[inline]
    pub fn iter_exponents_mut(&mut self) -> impl Iterator<Item = &mut [E]> {
        let nvars = self.nvars();
        self.exponents.chunks_mut(nvars)
    }

    /// Reset the polynomial.
    #[inline]
    pub fn clear(&mut self) {
        self.coefficients.clear();
        self.exponents.clear();
    }

    /// Get a reference to the variables in this polynomial.
    #[inline]
    pub fn get_vars(&self) -> &[Symbol] {
        self.variables.as_ref()
    }

    /// Combine the variable maps of two polynomials.
    #[inline]
    pub fn combine_variables(&mut self, other: &mut Self) {
        if self.variables == other.variables {
            return;
        }

        let mut new_var_map = self.variables.as_ref().clone();
        let mut new_var_pos_other = vec![0; other.nvars()];
        for (pos, v) in new_var_pos_other.iter_mut().zip(other.variables.as_ref()) {
            if let Some(p) = new_var_map.iter().position(|x| x == v) {
                *pos = p;
            } else {
                *pos = new_var_map.len();
                new_var_map.push(*v);
            }
        }

        let mut new_exp = vec![E::zero(); new_var_map.len() * self.nterms()];

        for t in 0..self.nterms() {
            new_exp[t * new_var_map.len()..t * new_var_map.len() + self.nvars()].copy_from_slice(self.exponents(t));
        }

        self.variables = Arc::new(new_var_map);
        self.exponents = new_exp;

        if new_var_pos_other.windows(2).all(|w| w[0] <= w[1]) {
            let mut new_exp = vec![E::zero(); self.nvars() * other.nterms()];

            if other.nvars() > 0 {
                for (d, t) in new_exp
                    .chunks_mut(self.nvars())
                    .zip(other.exponents.chunks(other.nvars()))
                {
                    for (var, e) in t.iter().enumerate() {
                        d[new_var_pos_other[var]] = *e;
                    }
                }
            }

            other.variables = self.variables.clone();
            other.exponents = new_exp;
            return;
        }

        let mut new_other = Self::new(&other.ring, other.nterms().into(), self.variables.clone());
        let mut new_exp = vec![E::zero(); self.nvars()];

        for t in other.into_iter() {
            for c in &mut new_exp {
                *c = E::zero();
            }

            for (var, e) in t.exponents.iter().enumerate() {
                new_exp[new_var_pos_other[var]] = *e;
            }

            new_other.push_monomial(t.coefficient.clone(), &new_exp);
        }

        *other = new_other;
    }

    /// Combines the variable maps of all the given polynomials.
    pub fn combine_all_variables(polys: &mut [Self]) {
        if polys.len() < 2 {
            return;
        }

        let (first, rest) = polys.split_first_mut().unwrap();

        for _ in 0..2 {
            for p in &mut *rest {
                first.combine_variables(p);
            }
        }
    }

    /// Reverse the monomial ordering in-place.
    fn reverse(&mut self) {
        let nterms = self.nterms();
        let nvars = self.nvars();

        if nterms < 2 {
            return;
        }

        self.coefficients.reverse();

        let midu = if nterms % 2 == 0 {
            self.nvars() * (nterms / 2)
        } else {
            self.nvars() * (nterms / 2 + 1)
        };

        let (l, r) = self.exponents.split_at_mut(midu);

        let rend = r.len();
        for i in 0..nterms / 2 {
            l[i * nvars..(i + 1) * nvars].swap_with_slice(&mut r[rend - (i + 1) * nvars..rend - i * nvars]);
        }
    }

    /// Add a variable to the polynomial if it is not already present.
    pub fn add_variable(&mut self, var: &Symbol) {
        if self.variables.iter().any(|v| v == var) {
            return;
        }

        let l = self.variables.len();

        let mut new_exp = vec![E::zero(); (l + 1) * self.nterms()];

        if l > 0 {
            for (en, e) in new_exp.chunks_mut(l + 1).zip(self.exponents.chunks(l)) {
                en[..l].copy_from_slice(e);
            }
        }

        let mut new_vars = self.variables.as_ref().clone();
        new_vars.push(*var);
        self.variables = Arc::new(new_vars);
        self.exponents = new_exp;
    }

    /// Check if the polynomial is sorted and has only non-zero coefficients.
    pub fn check_consistency(&self) -> Result<(), InconsistentPolynomial<R, E, O>> {
        assert_eq!(self.coefficients.len(), self.nterms());
        assert_eq!(self.exponents.len(), self.nterms() * self.nvars());

        for c in &self.coefficients {
            if self.ring.is_zero(c) {
                return Err(InconsistentPolynomial::ZeroCoefficient(self.clone()));
            }
        }

        for t in 1..self.nterms() {
            match O::cmp(self.exponents(t), self.exponents(t - 1)) {
                Ordering::Equal => return Err(InconsistentPolynomial::EqualMonomials(self.clone())),
                Ordering::Less => return Err(InconsistentPolynomial::WrongMonomialOrdering(self.clone())),
                Ordering::Greater => (),
            }
        }

        Ok(())
    }

    /// Push a monomial to the back of this polynomial.
    pub fn push_monomial_back(&mut self, coefficient: R::Element, exponents: &[E]) {
        if self.ring.is_zero(&coefficient) {
            return;
        }

        let nterms = self.nterms();
        if nterms > 0 && exponents == self.exponents_back(0) {
            self.ring.add_assign(&mut self.coefficients[nterms - 1], &coefficient);

            if self.ring.is_zero(&self.coefficients[nterms - 1]) {
                self.coefficients.pop();
                self.exponents.truncate((nterms - 1) * self.nvars());
            }
        } else {
            self.coefficients.push(coefficient);
            self.exponents.extend_from_slice(exponents);
        }
    }

    /// Push a monomial to the polynomial.
    pub fn push_monomial(&mut self, coefficient: R::Element, exponents: &[E]) {
        if self.ring.is_zero(&coefficient) {
            return;
        }

        if self.nvars() != exponents.len() {
            panic!("nvars mismatched: got {}, expected {}", exponents.len(), self.nvars());
        }

        if self.nterms() == 0 || O::cmp(self.exponents_back(0), exponents).is_lt() {
            self.coefficients.push(coefficient);
            self.exponents.extend_from_slice(exponents);
            return;
        }

        if O::cmp(self.exponents(0), exponents).is_gt() {
            self.coefficients.insert(0, coefficient);
            self.exponents.splice(0..0, exponents.iter().cloned());
            return;
        }

        let mut l = 0;
        let mut r = self.nterms();

        while l <= r {
            let m = (l + r) / 2;
            let c = O::cmp(exponents, self.exponents(m));

            match c {
                Ordering::Equal => {
                    self.ring.add_assign(&mut self.coefficients[m], &coefficient);

                    if self.ring.is_zero(&self.coefficients[m]) {
                        self.coefficients.remove(m);
                        let i = m * self.nvars();
                        self.exponents.splice(i..i + self.nvars(), Vec::new());
                    }

                    return;
                },
                Ordering::Less => {
                    if m == 0 {
                        self.coefficients.insert(0, coefficient);
                        self.exponents.splice(0..0, exponents.iter().cloned());
                        return;
                    }

                    r = m - 1
                },
                Ordering::Greater => {
                    l = m + 1;

                    if l == self.nterms() {
                        self.coefficients.push(coefficient);
                        self.exponents.extend_from_slice(exponents);
                        return;
                    }
                },
            }
        }

        self.coefficients.insert(l, coefficient);
        let i = l * self.nvars();
        self.exponents.splice(i..i, exponents.iter().cloned());
    }
}

impl<R: Ring + Debug, E: Exponent + Debug, O: MonomialOrder> Debug for MultivariatePolynomial<R, E, O>
where
    R::Element: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_zero() {
            return write!(f, "[]");
        }

        let mut first = true;
        write!(f, "[ ")?;

        for monomial in self {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{{ {:?}, {:?} }}", monomial.coefficient, monomial.exponents)?;
        }

        write!(f, " ]")
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> PartialEq for MultivariatePolynomial<R, E, O> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if self.variables != other.variables {
            if self.is_constant() != other.is_constant() {
                return false;
            }

            if self.is_zero() != other.is_zero() {
                return false;
            }

            if self.is_zero() {
                return true;
            }

            if self.is_constant() {
                return self.coefficients[0] == other.coefficients[0];
            }

            return false;
        }

        if self.nterms() != other.nterms() {
            return false;
        }

        self.exponents.eq(&other.exponents) && self.coefficients.eq(&other.coefficients)
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> Hash for MultivariatePolynomial<R, E, O> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.coefficients.hash(state);
        self.exponents.hash(state);

        if !self.is_constant() {
            self.variables.hash(state);
        }
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> Eq for MultivariatePolynomial<R, E, O> {}

impl<R: Ring, E: Exponent, O: MonomialOrder> InternalOrdering for MultivariatePolynomial<R, E, O> {
    fn internal_cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&self.exponents, &other.exponents).then_with(|| self.coefficients.internal_cmp(&other.coefficients))
    }
}

impl<R: Ring, E: Exponent, O: MonomialOrder> MultivariatePolynomial<R, E, O> {
    /// Get the degree of the variable `x`.
    /// This operation is O(n).
    pub fn degree(&self, x: usize) -> E {
        if self.nvars() == 0 {
            return E::zero();
        }

        let mut max = None;
        for e in self.exponents.iter().skip(x).step_by(self.nvars()) {
            if max.map(|max| max < *e).unwrap_or(true) {
                max = Some(*e);
            }
        }
        max.unwrap_or(E::zero())
    }

    /// Get the lowest and highest exponent of the variable `x`.
    /// This operation is O(n).
    pub fn degree_bounds(&self, x: usize) -> (E, E) {
        if self.nvars() == 0 {
            return (E::zero(), E::zero());
        }

        let mut min = None;
        let mut max = None;
        for e in self.exponents.iter().skip(x).step_by(self.nvars()) {
            if max.map(|max| max < *e).unwrap_or(true) {
                max = Some(*e);
            }
            if min.map(|min| min > *e).unwrap_or(true) {
                min = Some(*e);
            }
        }
        (min.unwrap_or(E::zero()), max.unwrap_or(E::zero()))
    }

    // Get the highest degree of a variable in the leading monomial.
    pub fn ldegree(&self, v: usize) -> E {
        if self.is_zero() {
            return E::zero();
        }
        self.exponents_back(0)[v]
    }

    /// Get the highest degree of the leading monomial.
    pub fn ldegree_max(&self) -> E {
        if self.is_zero() {
            return E::zero();
        }
        *self.exponents_back(0).iter().max().unwrap_or(&E::zero())
    }

    /// Get the leading coefficient.
    pub fn lcoeff(&self) -> R::Element {
        if self.is_zero() {
            return self.ring.zero();
        }
        self.coefficients.last().unwrap().clone()
    }
}

impl<R: Ring, E: Exponent> MultivariatePolynomial<R, E, LexOrder> {
    /// Check if all exponents are positive.
    pub fn is_polynomial(&self) -> bool {
        self.is_zero() || self.exponents.iter().all(|e| *e >= E::zero())
    }
}

/// View object for a term in the multivariate polynomial.
#[derive(Copy, Clone, Debug)]
pub struct MonomialView<'a, R: 'a + Ring, E: 'a + Exponent> {
    pub coefficient: &'a R::Element,
    pub exponents: &'a [E],
}

/// Iterator over terms in a multivariate polynomial.
pub struct MonomialViewIterator<'a, F: Ring, E: Exponent, O: MonomialOrder> {
    poly: &'a MultivariatePolynomial<F, E, O>,
    index: usize,
}

impl<'a, R: Ring, E: Exponent, O: MonomialOrder> Iterator for MonomialViewIterator<'a, R, E, O> {
    type Item = MonomialView<'a, R, E>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.poly.nterms() {
            None
        } else {
            let view = MonomialView {
                coefficient: &self.poly.coefficients[self.index],
                exponents: self.poly.exponents(self.index),
            };

            self.index += 1;
            Some(view)
        }
    }
}

impl<'a, R: Ring, E: Exponent, O: MonomialOrder> IntoIterator for &'a MultivariatePolynomial<R, E, O> {
    type Item = MonomialView<'a, R, E>;
    type IntoIter = MonomialViewIterator<'a, R, E, O>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter { poly: self, index: 0 }
    }
}
