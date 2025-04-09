use std::{collections::VecDeque, fmt};

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
        expr: Box<Expr>,
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UnaryOpCode {
    Neg,
}

trait DisplayLeaves: fmt::Display + DynClone {
    fn leaves(&self) -> Vec<Box<dyn DisplayLeaves>>;
}

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
                .chain(std::iter::once(expr).map(|e| -> Box<dyn DisplayLeaves> { e.clone() }))
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

dyn_clone::clone_trait_object!(DisplayLeaves);

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
