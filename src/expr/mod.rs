#![allow(clippy::vec_box)]

use std::{cmp::Ordering, collections::VecDeque, fmt};

pub use add::Add;
use dyn_clone::DynClone;
pub use mul::Mul;
pub use num::Num;
use termion::color;

mod add;
mod mul;
mod num;

#[derive(Clone, PartialEq, Eq)]
pub enum Expr {
    Num(Num),
    Var(Var),
    Fun(Fun),
    Pow(Pow),
    Mul(Mul),
    Add(Add),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Var {
    pub name: String,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Fun {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Pow {
    pub base: Box<Expr>,
    pub exp: Box<Expr>,
}

impl Var {
    pub fn new(name: String) -> Var {
        Var { name }
    }
}

impl Fun {
    pub fn new(name: String, args: Vec<Expr>) -> Fun {
        Fun { name, args }
    }

    pub fn new_boxed(name: String, args: Vec<Box<Expr>>) -> Fun {
        Fun::new(name, args.into_iter().map(|x| *x).collect())
    }
}

impl Pow {
    pub fn new(base: Expr, exp: Expr) -> Pow {
        Pow::new_boxed(Box::new(base), Box::new(exp))
    }

    pub fn new_boxed(base: Box<Expr>, exp: Box<Expr>) -> Pow {
        Pow { base, exp }
    }
}

impl Expr {
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
            (Expr::Var(v1), Expr::Var(v2)) => v1.name.cmp(&v2.name),
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
                let name_cmp = f1.name.cmp(&f2.name);
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

impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Num::Integer(int) => write!(f, "{}", int),
            Num::Float(float) => write!(f, "{}", float),
            Num::Rational(num, denom) => write!(f, "{}/{}", num, denom),
        }
    }
}

trait DisplayLeaves: fmt::Debug + DynClone {
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>>;
}

dyn_clone::clone_trait_object!(DisplayLeaves);

impl DisplayLeaves for Expr {
    #[allow(clippy::map_clone)]
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>> {
        match self {
            Expr::Fun(fun) => fun
                .args
                .iter()
                .map(|e| -> Box<dyn DisplayLeaves> { Box::new(e.clone()) })
                .collect(),
            Expr::Pow(pow) => vec![pow.base.clone(), pow.exp.clone()],
            Expr::Mul(mul) => mul
                .factors
                .iter()
                .map(|e| -> Box<dyn DisplayLeaves> { Box::new(e.clone()) })
                .collect(),
            Expr::Add(add) => add
                .terms
                .iter()
                .map(|e| -> Box<dyn DisplayLeaves> { Box::new(e.clone()) })
                .collect(),
            _ => vec![],
        }
    }
}

type DisplayQueue = VecDeque<(bool, Box<dyn DisplayLeaves>, Vec<bool>)>;

const GLYPH_MIDDLE_ITEM: &str = "├";
const GLYPH_LAST_ITEM: &str = "└";
const GLYPH_ITEM_INDENT: &str = "── ";
const GLYPH_MIDDLE_SKIP: &str = "│";
const GLYPH_LAST_SKIP: &str = " ";
const GLYPH_SKIP_INDENT: &str = "  ";

fn enqueue_leaves<T: DisplayLeaves>(
    queue: &mut VecDeque<(bool, Box<(dyn DisplayLeaves + '_)>, Vec<bool>)>,
    parent: &T,
    spaces: Vec<bool>,
) {
    for (i, leaf) in parent.leaves().iter().rev().enumerate() {
        let last = i == 0;
        queue.push_front((last, leaf.clone(), spaces.clone()));
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Num(num) => write!(f, "{}{}", color::Fg(color::Yellow), num)?,
            Expr::Var(v) => write!(f, "{}{}", color::Fg(color::White), v.name)?,
            Expr::Fun(fun) => write!(f, "{}{}", color::Fg(color::Blue), fun.name)?,
            Expr::Pow(_) => write!(f, "{}Pow", color::Fg(color::Cyan))?,
            Expr::Mul(_) => write!(f, "{}Mul", color::Fg(color::Cyan))?,
            Expr::Add(_) => write!(f, "{}Add", color::Fg(color::Cyan))?,
        };

        write!(f, "{}", color::Fg(color::Reset))?;
        writeln!(f)?;

        let mut queue = DisplayQueue::new();
        enqueue_leaves(&mut queue, self, Vec::new());

        while let Some((last, leaf, spaces)) = queue.pop_front() {
            let mut prefix = (
                if last { GLYPH_LAST_ITEM } else { GLYPH_MIDDLE_ITEM },
                GLYPH_ITEM_INDENT,
            );

            let rest_prefix = (
                if last { GLYPH_LAST_SKIP } else { GLYPH_MIDDLE_SKIP },
                GLYPH_SKIP_INDENT,
            );

            let root = format!("{:?}", leaf);
            for line in root.lines() {
                write!(f, "{}", color::Fg(color::LightBlack))?;
                for s in spaces.as_slice() {
                    if *s {
                        fmt::Display::fmt(GLYPH_LAST_SKIP, f)?;
                    } else {
                        fmt::Display::fmt(GLYPH_MIDDLE_SKIP, f)?;
                    }
                    fmt::Display::fmt(GLYPH_SKIP_INDENT, f)?;
                }

                fmt::Display::fmt(prefix.0, f)?;
                fmt::Display::fmt(prefix.1, f)?;
                write!(f, "{}", color::Fg(color::Reset))?;
                fmt::Display::fmt(line, f)?;
                writeln!(f)?;
                prefix = rest_prefix;
            }
        }

        Ok(())
    }
}
