use super::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pow {
    pub base: Box<Expr>,
    pub exp: Box<Expr>,
}

impl Pow {
    pub fn new(base: Expr, exp: Expr) -> Pow {
        Pow::new_boxed(Box::new(base), Box::new(exp))
    }

    pub fn new_boxed(base: Box<Expr>, exp: Box<Expr>) -> Pow {
        Pow { base, exp }
    }
}
