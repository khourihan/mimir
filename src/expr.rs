use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use dyn_clone::DynClone;
use termion::color;

#[derive(Clone, Debug)]
#[allow(clippy::vec_box)]
pub enum Expr {
    /// An integer.
    Integer(i32),
    /// A real number.
    Real(f32),
    /// An identifier.
    Var(String),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOpCode,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOpCode,
        expr: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Box<Expr>>,
    },
    Block {
        stmts: Vec<Box<Statement>>,
        expr: Option<Box<Expr>>,
    },
    Error,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assertion {
        left: Box<Expr>,
        op: AssertOpCode,
        right: Box<Expr>,
    },
    Reduction {
        pattern: Box<Expr>,
        result: Box<Expr>,
    },
    Expr(Box<Expr>),
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum AssertOpCode {
    /// Equals
    Eq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less than equals
    Lte,
    /// Greater than equals
    Gte,
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum BinaryOpCode {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Exponentiation
    Pow,
    /// Equals
    Eq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less than equals
    Lte,
    /// Greater than equals
    Gte,
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum UnaryOpCode {
    Neg,
}

impl Expr {
    /// Matches an expr against a pattern, returning [`None`] if there is no match or a [`HashMap`]
    /// from variable identifiers to expressions they matched if there is a match.
    pub fn matches(&self, pattern: &Self) -> Option<HashMap<String, Box<Expr>>> {
        let mut vars = HashMap::new();

        if self.matches_recurse(pattern, &mut vars) {
            Some(vars)
        } else {
            None
        }
    }

    fn matches_recurse(&self, pattern: &Self, vars: &mut HashMap<String, Box<Expr>>) -> bool {
        match (self, pattern) {
            (any, Expr::Var(var)) => {
                if let Some(value) = vars.get(var) {
                    *any == **value
                } else {
                    vars.insert(var.clone(), Box::new(any.clone()));
                    true
                }
            },
            (
                Expr::BinaryOp {
                    left: left1,
                    op: op1,
                    right: right1,
                },
                Expr::BinaryOp {
                    left: left2,
                    op: op2,
                    right: right2,
                },
            ) => op1 == op2 && left1.matches_recurse(left2, vars) && right1.matches_recurse(right2, vars),
            (Expr::UnaryOp { op: op1, expr: expr1 }, Expr::UnaryOp { op: op2, expr: expr2 }) => {
                op1 == op2 && expr1.matches_recurse(expr2, vars)
            },
            (
                Expr::Call {
                    name: name1,
                    args: args1,
                },
                Expr::Call {
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
            (
                Expr::Block {
                    stmts: stmts1,
                    expr: expr1,
                },
                Expr::Block {
                    stmts: stmts2,
                    expr: expr2,
                },
            ) => {
                expr1
                    .as_ref()
                    .zip(expr2.as_ref())
                    .is_none_or(|(e1, e2)| e1.matches_recurse(e2, vars))
                    && stmts1
                        .iter()
                        .zip(stmts2.iter())
                        .all(|(s1, s2)| match (s1.as_ref(), s2.as_ref()) {
                            (
                                Statement::Assertion {
                                    left: left1,
                                    op: op1,
                                    right: right1,
                                },
                                Statement::Assertion {
                                    left: left2,
                                    op: op2,
                                    right: right2,
                                },
                            ) => {
                                op1 == op2 && left1.matches_recurse(left2, vars) && right1.matches_recurse(right2, vars)
                            },
                            (Statement::Expr(expr1), Statement::Expr(expr2)) => expr1.matches_recurse(expr2, vars),
                            _ => false,
                        })
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
            Expr::BinaryOp { left, op, right } => Expr::BinaryOp {
                left: Box::new(left.expand(vars)),
                op: *op,
                right: Box::new(right.expand(vars)),
            },
            Expr::UnaryOp { op, expr } => Expr::UnaryOp {
                op: *op,
                expr: Box::new(expr.expand(vars)),
            },
            Expr::Call { name, args } => Expr::Call {
                name: name.clone(),
                args: args.iter().map(|arg| Box::new(arg.expand(vars))).collect(),
            },
            Expr::Block { stmts, expr } => Expr::Block {
                stmts: stmts
                    .iter()
                    .map(|stmt| match *stmt.clone() {
                        Statement::Assertion { left, op, right } => Box::new(Statement::Assertion {
                            left: Box::new(left.expand(vars)),
                            op,
                            right: Box::new(right.expand(vars)),
                        }),
                        Statement::Reduction { pattern, result } => Box::new(Statement::Reduction {
                            pattern: Box::new(pattern.expand(vars)),
                            result: Box::new(result.expand(vars)),
                        }),
                        Statement::Expr(expr) => Box::new(Statement::Expr(Box::new(expr.expand(vars)))),
                        _ => stmt.clone(),
                    })
                    .collect(),
                expr: expr.as_ref().map(|e| Box::new(e.expand(vars))),
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
                Self::BinaryOp {
                    left: l_left,
                    op: l_op,
                    right: l_right,
                },
                Self::BinaryOp {
                    left: r_left,
                    op: r_op,
                    right: r_right,
                },
            ) => l_left == r_left && l_op == r_op && l_right == r_right,
            (Self::UnaryOp { op: l_op, expr: l_expr }, Self::UnaryOp { op: r_op, expr: r_expr }) => {
                l_op == r_op && l_expr == r_expr
            },
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
            (
                Self::Block {
                    stmts: l_stmts,
                    expr: l_expr,
                },
                Self::Block {
                    stmts: r_stmts,
                    expr: r_expr,
                },
            ) => l_stmts.iter().zip(r_stmts.iter()).all(|(l, r)| l == r) && l_expr == r_expr,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Assertion {
                    left: l_left,
                    op: l_op,
                    right: l_right,
                },
                Self::Assertion {
                    left: r_left,
                    op: r_op,
                    right: r_right,
                },
            ) => l_left == r_left && l_op == r_op && l_right == r_right,
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
            (Self::Expr(l0), Self::Expr(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
            Expr::BinaryOp { left, op: _, right } => vec![left.clone(), right.clone()],
            Expr::UnaryOp { op: _, expr } => vec![expr.clone()],
            Expr::Call { name: _, args } => args.iter().map(|e| -> Box<dyn DisplayLeaves> { e.clone() }).collect(),
            Expr::Block { stmts, expr } => stmts
                .iter()
                .map(|e| -> Box<dyn DisplayLeaves> { e.clone() })
                .chain(
                    std::iter::once(expr)
                        .flatten()
                        .map(|e| -> Box<dyn DisplayLeaves> { e.clone() }),
                )
                .collect::<Vec<Box<dyn DisplayLeaves>>>(),
            _ => vec![],
        }
    }
}

impl DisplayLeaves for Statement {
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>> {
        match self {
            Statement::Assertion { left, op: _, right } => vec![left.clone(), right.clone()],
            Statement::Reduction { pattern, result } => vec![pattern.clone(), result.clone()],
            Statement::Expr(expr) => vec![expr.clone()],
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
            Expr::BinaryOp { op, .. } => match op {
                BinaryOpCode::Add => write!(f, "{}Add", color::Fg(color::Cyan))?,
                BinaryOpCode::Sub => write!(f, "{}Sub", color::Fg(color::Cyan))?,
                BinaryOpCode::Mul => write!(f, "{}Mul", color::Fg(color::Cyan))?,
                BinaryOpCode::Div => write!(f, "{}Div", color::Fg(color::Cyan))?,
                BinaryOpCode::Pow => write!(f, "{}Pow", color::Fg(color::Cyan))?,
                BinaryOpCode::Eq => write!(f, "{}Eq", color::Fg(color::Cyan))?,
                BinaryOpCode::Lt => write!(f, "{}Lt", color::Fg(color::Cyan))?,
                BinaryOpCode::Gt => write!(f, "{}Gt", color::Fg(color::Cyan))?,
                BinaryOpCode::Lte => write!(f, "{}Lte", color::Fg(color::Cyan))?,
                BinaryOpCode::Gte => write!(f, "{}Gte", color::Fg(color::Cyan))?,
            },
            Expr::UnaryOp { op, .. } => match op {
                UnaryOpCode::Neg => write!(f, "{}Neg", color::Fg(color::Cyan))?,
            },
            Expr::Call { name, .. } => write!(f, "{}{}", color::Fg(color::Blue), name)?,
            Expr::Block { .. } => write!(f, "{}Block", color::Fg(color::Magenta))?,
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
            Statement::Assertion { op, .. } => match op {
                AssertOpCode::Eq => write!(f, "{}Assert Eq", color::Fg(color::Magenta))?,
                AssertOpCode::Lt => write!(f, "{}Assert Lt", color::Fg(color::Magenta))?,
                AssertOpCode::Gt => write!(f, "{}Assert Gt", color::Fg(color::Magenta))?,
                AssertOpCode::Lte => write!(f, "{}Assert Lte", color::Fg(color::Magenta))?,
                AssertOpCode::Gte => write!(f, "{}Assert Gte", color::Fg(color::Magenta))?,
            },
            Statement::Expr(_) => write!(f, "{}Expr", color::Fg(color::Magenta))?,
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
