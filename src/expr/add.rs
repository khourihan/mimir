use super::Expr;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Add {
    pub terms: Vec<Expr>,
}

impl Add {
    pub fn new(terms: Vec<Expr>) -> Add {
        Add { terms }
    }

    pub fn new_boxed(factors: Vec<Box<Expr>>) -> Add {
        Add::new(factors.into_iter().map(|x| *x).collect())
    }

    #[inline]
    pub fn push(&mut self, expr: Expr) {
        self.terms.push(expr);
    }

    #[inline]
    pub fn iter(&'_ self) -> std::slice::Iter<'_, Expr> {
        self.terms.iter()
    }

    #[inline]
    pub fn iter_mut(&'_ mut self) -> std::slice::IterMut<'_, Expr> {
        self.terms.iter_mut()
    }
}

impl IntoIterator for Add {
    type Item = Expr;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}
