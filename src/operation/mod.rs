use crate::expr::Expr;

pub mod reduce;

/// A symbolic operation.
pub trait Operation {
    type Error;

    #[allow(clippy::vec_box)]
    fn apply(&mut self, expr: Box<Expr>) -> Result<Vec<Box<Expr>>, Self::Error>;
}
