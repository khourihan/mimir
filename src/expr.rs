use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use dyn_clone::DynClone;
use ordered_float::OrderedFloat;
use termion::color;

#[derive(Clone, Debug)]
#[allow(clippy::vec_box)]
pub enum Expr {
    /// An integer.
    Integer(i32),
    /// A real number.
    Real(OrderedFloat<f32>),
    /// A variable.
    Var(String),
    /// A call with a literal function name.
    Call {
        name: String,
        args: Vec<Box<Expr>>,
    },
    Error,
}

#[derive(Clone, Debug)]
#[allow(clippy::vec_box)]
pub enum Pat {
    /// An integer.
    Integer(i32),
    /// A real number.
    Real(OrderedFloat<f32>),
    /// A variable
    Var(String),
    /// A call with a literal function name.
    Call {
        name: String,
        args: Vec<Box<Pat>>,
    },
    Error,
}

impl From<Expr> for Pat {
    fn from(value: Expr) -> Self {
        match value {
            Expr::Integer(int) => Pat::Integer(int),
            Expr::Real(float) => Pat::Real(float),
            Expr::Var(var) => Pat::Var(var),
            Expr::Call { name, args } => Pat::Call {
                name,
                args: args.into_iter().map(|a| a.into()).collect(),
            },
            Expr::Error => Pat::Error,
        }
    }
}

impl From<Box<Expr>> for Box<Pat> {
    fn from(value: Box<Expr>) -> Self {
        Box::new(Pat::from(*value))
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Reduction { pattern: Box<Pat>, result: Box<Expr> },
    Error,
}

impl Expr {
    /// Matches an expr against a pattern, returning [`None`] if there is no match or a [`HashMap`]
    /// from variable identifiers to expressions they matched if there is a match.
    pub fn matches(&self, pattern: &Pat) -> Option<HashMap<String, Box<Expr>>> {
        let mut vars = HashMap::new();

        if self.matches_recurse(pattern, &mut vars) {
            Some(vars)
        } else {
            None
        }
    }

    fn matches_recurse(&self, pattern: &Pat, vars: &mut HashMap<String, Box<Expr>>) -> bool {
        match (self, pattern) {
            (any, Pat::Var(var)) => {
                if let Some(value) = vars.get(var) {
                    *any == **value
                } else {
                    vars.insert(var.clone(), Box::new(any.clone()));
                    true
                }
            },
            (Expr::Integer(l0), Pat::Integer(r0)) => *l0 == *r0,
            (Expr::Real(l0), Pat::Real(r0)) => *l0 == *r0,
            (
                Expr::Call {
                    name: name1,
                    args: args1,
                },
                Pat::Call {
                    name: name2,
                    args: args2,
                },
            ) => {
                // TODO: wildcard function names (may need to have separate Pattern type)
                name1 == name2
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(arg1, arg2)| arg1.matches_recurse(arg2, vars))
            },
            _ => false,
        }
    }

    pub fn expand(&self, vars: &HashMap<String, Box<Expr>>) -> Expr {
        match self {
            Expr::Var(var) => {
                if let Some(e) = vars.get(var) {
                    *e.clone()
                } else {
                    panic!()
                }
            },
            Expr::Call { name, args } => Expr::Call {
                name: name.clone(),
                args: args.iter().map(|arg| Box::new(arg.expand(vars))).collect(),
            },
            _ => self.clone(),
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Real(l0), Self::Real(r0)) => l0 == r0,
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (
                Self::Call {
                    name: l_name,
                    args: l_args,
                },
                Self::Call {
                    name: r_name,
                    args: r_args,
                },
            ) => l_name == r_name && l_args == r_args,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialEq for Pat {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Real(l0), Self::Real(r0)) => l0 == r0,
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (
                Self::Call {
                    name: l_name,
                    args: l_args,
                },
                Self::Call {
                    name: r_name,
                    args: r_args,
                },
            ) => l_name == r_name && l_args == r_args,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Reduction {
                    pattern: l_pattern,
                    result: l_result,
                },
                Self::Reduction {
                    pattern: r_pattern,
                    result: r_result,
                },
            ) => l_pattern == r_pattern && l_result == r_result,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for Expr {}
impl Eq for Statement {}

impl std::hash::Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expr::Integer(int) => int.hash(state),
            Expr::Real(float) => float.hash(state),
            Expr::Var(var) => var.hash(state),
            Expr::Call { name, args } => {
                name.hash(state);
                args.hash(state);
            },
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl std::hash::Hash for Pat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Pat::Integer(int) => int.hash(state),
            Pat::Real(float) => float.hash(state),
            Pat::Var(var) => var.hash(state),
            Pat::Call { name, args } => {
                name.hash(state);
                args.hash(state);
            },
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl std::hash::Hash for Statement {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Statement::Reduction { pattern, result } => {
                pattern.hash(state);
                result.hash(state);
            },
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

trait DisplayLeaves: fmt::Display + DynClone {
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>>;
}

dyn_clone::clone_trait_object!(DisplayLeaves);

impl DisplayLeaves for Expr {
    #[allow(clippy::map_clone)]
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>> {
        match self {
            Expr::Call { name: _, args } => args.iter().map(|e| -> Box<dyn DisplayLeaves> { e.clone() }).collect(),
            _ => vec![],
        }
    }
}

impl DisplayLeaves for Pat {
    #[allow(clippy::map_clone)]
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>> {
        match self {
            Pat::Call { name: _, args } => args.iter().map(|e| -> Box<dyn DisplayLeaves> { e.clone() }).collect(),
            _ => vec![],
        }
    }
}

impl DisplayLeaves for Statement {
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>> {
        match self {
            Statement::Reduction { pattern, result } => vec![pattern.clone(), result.clone()],
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(v) => write!(f, "{}{:?}", color::Fg(color::Yellow), v)?,
            Expr::Real(v) => write!(f, "{}{:?}", color::Fg(color::Yellow), v)?,
            Expr::Var(v) => write!(f, "{}{:?}", color::Fg(color::White), v)?,
            Expr::Error => write!(f, "{}error", color::Fg(color::Red))?,
            Expr::Call { name, .. } => write!(f, "{}{}", color::Fg(color::Blue), name)?,
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

            let root = format!("{}", leaf);
            for line in root.lines() {
                write!(f, "{}", color::Fg(color::LightBlack))?;
                for s in spaces.as_slice() {
                    if *s {
                        GLYPH_LAST_SKIP.fmt(f)?;
                    } else {
                        GLYPH_MIDDLE_SKIP.fmt(f)?;
                    }
                    GLYPH_SKIP_INDENT.fmt(f)?;
                }

                prefix.0.fmt(f)?;
                prefix.1.fmt(f)?;
                write!(f, "{}", color::Fg(color::Reset))?;
                line.fmt(f)?;
                writeln!(f)?;
                prefix = rest_prefix;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Integer(v) => write!(f, "{}{:?}", color::Fg(color::Yellow), v)?,
            Pat::Real(v) => write!(f, "{}{:?}", color::Fg(color::Yellow), v)?,
            Pat::Var(v) => write!(f, "{}{:?}", color::Fg(color::White), v)?,
            Pat::Error => write!(f, "{}error", color::Fg(color::Red))?,
            Pat::Call { name, .. } => write!(f, "{}{}", color::Fg(color::Blue), name)?,
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

            let root = format!("{}", leaf);
            for line in root.lines() {
                write!(f, "{}", color::Fg(color::LightBlack))?;
                for s in spaces.as_slice() {
                    if *s {
                        GLYPH_LAST_SKIP.fmt(f)?;
                    } else {
                        GLYPH_MIDDLE_SKIP.fmt(f)?;
                    }
                    GLYPH_SKIP_INDENT.fmt(f)?;
                }

                prefix.0.fmt(f)?;
                prefix.1.fmt(f)?;
                write!(f, "{}", color::Fg(color::Reset))?;
                line.fmt(f)?;
                writeln!(f)?;
                prefix = rest_prefix;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Reduction { .. } => write!(f, "{}Reduction", color::Fg(color::Magenta))?,
            Statement::Error => write!(f, "{}error", color::Fg(color::Red))?,
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

            let root = format!("{}", leaf);
            for line in root.lines() {
                write!(f, "{}", color::Fg(color::LightBlack))?;
                for s in spaces.as_slice() {
                    if *s {
                        GLYPH_LAST_SKIP.fmt(f)?;
                    } else {
                        GLYPH_MIDDLE_SKIP.fmt(f)?;
                    }
                    GLYPH_SKIP_INDENT.fmt(f)?;
                }

                prefix.0.fmt(f)?;
                prefix.1.fmt(f)?;
                write!(f, "{}", color::Fg(color::Reset))?;
                line.fmt(f)?;
                writeln!(f)?;
                prefix = rest_prefix;
            }
        }

        Ok(())
    }
}
