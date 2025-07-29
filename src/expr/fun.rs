use crate::context::{Context, Symbol};

use super::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fun {
    pub id: Symbol,
    pub args: Vec<Expr>,
}

impl Fun {
    #[inline]
    pub fn new(id: Symbol, args: Vec<Expr>) -> Fun {
        Fun { id, args }
    }

    #[inline]
    pub fn from_name(name: String, args: Vec<Box<Expr>>) -> Fun {
        let s = Context::get_context_mut().get_symbol(name);
        Fun::new(s, args.into_iter().map(|x| *x).collect())
    }

    #[inline]
    pub fn push(&mut self, arg: Expr) {
        self.args.push(arg);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.args.len()
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<Expr> {
        self.args.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> std::slice::IterMut<Expr> {
        self.args.iter_mut()
    }
}

impl IntoIterator for Fun {
    type Item = Expr;
    type IntoIter = std::vec::IntoIter<Expr>;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}
