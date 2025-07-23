use super::Expr;

#[derive(Clone, PartialEq, Eq)]
pub struct Mul {
    pub factors: Vec<Expr>,
}

impl Mul {
    pub fn new(factors: Vec<Expr>) -> Mul {
        Mul { factors }
    }

    pub fn new_boxed(factors: Vec<Box<Expr>>) -> Mul {
        Mul::new(factors.into_iter().map(|x| *x).collect())
    }

    pub fn has_coefficient(&self) -> bool {
        self.factors.iter().any(|x| matches!(x, Expr::Num(_)))
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{Expr, Num, Var};

    use super::Mul;

    #[test]
    fn test_has_coefficient() {
        let m1 = Mul::new(vec![Expr::Num(Num::Integer(5))]);
        let m2 = Mul::new(vec![Expr::Var(Var::new(String::from("x")))]);

        assert!(m1.has_coefficient());
        assert!(!m2.has_coefficient());
    }
}
