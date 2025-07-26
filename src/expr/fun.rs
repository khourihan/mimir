use super::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fun {
    pub name: String,
    pub args: Vec<Expr>,
}

impl Fun {
    #[inline]
    pub fn new(name: String, args: Vec<Expr>) -> Fun {
        Fun { name, args }
    }

    #[inline]
    pub fn new_boxed(name: String, args: Vec<Box<Expr>>) -> Fun {
        Fun::new(name, args.into_iter().map(|x| *x).collect())
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
