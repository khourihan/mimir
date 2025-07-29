#![allow(clippy::vec_box)]

use std::cmp::Ordering;

pub use add::Add;
pub use fun::Fun;
pub use mul::Mul;
pub use num::Num;
pub use pow::Pow;
pub use var::Var;

use crate::context::Symbol;

mod add;
mod fmt;
mod fun;
mod mul;
mod num;
mod pow;
mod var;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(Num),
    Var(Var),
    Fun(Fun),
    Pow(Pow),
    Mul(Mul),
    Add(Add),
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord)]
pub enum ExprType {
    Num = 0,
    Var = 1,
    Fun = 2,
    Pow = 3,
    Mul = 4,
    Add = 5,
}

impl Expr {
    pub fn expr_type(&self) -> ExprType {
        match self {
            Expr::Num(_) => ExprType::Num,
            Expr::Var(_) => ExprType::Var,
            Expr::Fun(_) => ExprType::Fun,
            Expr::Pow(_) => ExprType::Pow,
            Expr::Mul(_) => ExprType::Mul,
            Expr::Add(_) => ExprType::Add,
        }
    }

    pub fn add(self, rhs: Expr) -> Expr {
        let mut terms = Vec::new();

        if let Expr::Add(a) = self {
            terms.extend(a.terms);
        } else {
            terms.push(self);
        }

        if let Expr::Add(a) = rhs {
            terms.extend(a.terms);
        } else {
            terms.push(rhs);
        }

        Expr::Add(Add::new(terms))
    }

    pub fn mul(self, rhs: Expr) -> Expr {
        let mut factors = Vec::new();

        if let Expr::Mul(m) = self {
            factors.extend(m.factors);
        } else {
            factors.push(self);
        }

        if let Expr::Mul(m) = rhs {
            factors.extend(m.factors);
        } else {
            factors.push(rhs);
        }

        Expr::Mul(Mul::new(factors))
    }

    pub fn pow(self, rhs: Expr) -> Expr {
        Expr::Pow(Pow::new(self, rhs))
    }

    pub fn contains_symbol(&self, s: Symbol) -> bool {
        let mut stack = Vec::with_capacity(16);
        stack.push(self.clone());
        while let Some(c) = stack.pop() {
            match c {
                Expr::Num(_) => (),
                Expr::Var(var) => {
                    if var.id == s {
                        return true;
                    }
                },
                Expr::Fun(fun) => {
                    if fun.id == s {
                        return true;
                    }
                    for arg in fun {
                        stack.push(arg);
                    }
                },
                Expr::Pow(pow) => {
                    stack.push(*pow.base);
                    stack.push(*pow.exp);
                },
                Expr::Mul(mul) => {
                    for factor in mul {
                        stack.push(factor);
                    }
                },
                Expr::Add(add) => {
                    for term in add {
                        stack.push(term);
                    }
                },
            }
        }

        false
    }
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            return Ordering::Equal;
        }

        match (self, other) {
            (Expr::Num(n1), Expr::Num(n2)) => n1.cmp(n2),
            (Expr::Num(_), _) => Ordering::Greater,
            (_, Expr::Num(_)) => Ordering::Less,
            (Expr::Var(v1), Expr::Var(v2)) => v1.id.cmp(&v2.id),
            (Expr::Var(_), _) => Ordering::Less,
            (_, Expr::Var(_)) => Ordering::Greater,
            (Expr::Pow(p1), Expr::Pow(p2)) => p1.base.cmp(&p2.base).then_with(|| p1.exp.cmp(&p2.exp)),
            (_, Expr::Pow(_)) => Ordering::Greater,
            (Expr::Pow(_), _) => Ordering::Less,
            (Expr::Mul(m1), Expr::Mul(m2)) => {
                let len_cmp = m1.factors.len().cmp(&m2.factors.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in m1.factors.iter().zip(m2.factors.iter()) {
                    let factor_cmp = x1.cmp(x2);
                    if factor_cmp != Ordering::Equal {
                        return factor_cmp;
                    }
                }

                Ordering::Equal
            },
            (Expr::Mul(_), _) => Ordering::Less,
            (_, Expr::Mul(_)) => Ordering::Greater,
            (Expr::Add(a1), Expr::Add(a2)) => {
                let len_cmp = a1.terms.len().cmp(&a2.terms.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in a1.terms.iter().zip(a2.terms.iter()) {
                    let term_cmp = x1.cmp(x2);
                    if term_cmp != Ordering::Equal {
                        return term_cmp;
                    }
                }

                Ordering::Equal
            },
            (Expr::Add(_), _) => Ordering::Less,
            (_, Expr::Add(_)) => Ordering::Greater,
            (Expr::Fun(f1), Expr::Fun(f2)) => {
                let name_cmp = f1.id.cmp(&f2.id);
                if name_cmp != Ordering::Equal {
                    return name_cmp;
                }

                let len_cmp = f1.args.len().cmp(&f2.args.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in f1.args.iter().zip(f2.args.iter()) {
                    let arg_cmp = x1.cmp(x2);
                    if arg_cmp != Ordering::Equal {
                        return arg_cmp;
                    }
                }

                Ordering::Equal
            },
        }
    }
}
