use std::{collections::VecDeque, fmt, str::FromStr};

use termion::{color, style};

#[derive(Debug)]
pub enum ParseError {
    InvalidToken {
        location: usize,
    },
    UnexpectedEof {
        location: usize,
        expected: Vec<String>,
    },
    UnexpectedToken {
        start: usize,
        token: String,
        end: usize,
        expected: Vec<String>,
    },
    ExtraToken {
        start: usize,
        token: String,
        end: usize,
    },
    User {
        error: String,
    },
    Multiple(Vec<ParseError>),
}

impl From<lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &str>> for ParseError {
    fn from(value: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &str>) -> Self {
        match value {
            lalrpop_util::ParseError::InvalidToken { location } => Self::InvalidToken { location },
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                Self::UnexpectedEof { location, expected }
            },
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Self::UnexpectedToken {
                start,
                token: token.to_string(),
                end,
                expected,
            },
            lalrpop_util::ParseError::ExtraToken {
                token: (start, token, end),
            } => Self::ExtraToken {
                start,
                token: token.to_string(),
                end,
            },
            lalrpop_util::ParseError::User { error } => Self::User {
                error: error.to_string(),
            },
        }
    }
}

impl ParseError {
    pub fn fmt(&self, src: &str) {
        match self {
            ParseError::Multiple(_) => (),
            _ => {
                print!(
                    "\n{}{}error{}: ",
                    color::Fg(color::Red),
                    style::Bold,
                    color::Fg(color::Reset)
                );
            },
        }

        match self {
            ParseError::InvalidToken { location } => {
                print!("invalid token");
                self.fmt_details("", *location, *location + 1, src);
            },
            ParseError::UnexpectedEof { location, expected } => {
                print!("unexpected eof");

                let details = if expected.is_empty() {
                    String::new()
                } else {
                    let mut details = String::from("expected ");
                    for s in expected.iter().take(expected.len() - 1) {
                        details.push_str(&format!("{}, ", s));
                    }
                    details.push_str(&format!("or {}", expected.last().unwrap()));
                    details
                };

                self.fmt_details(&details, *location, *location + 1, src);
            },
            ParseError::UnexpectedToken {
                start,
                token,
                end,
                expected,
            } => {
                print!("unexpected token {}", token);

                let details = if expected.is_empty() {
                    String::new()
                } else {
                    let mut details = String::from("expected ");
                    for s in expected.iter().take(expected.len() - 1) {
                        details.push_str(&format!("{}, ", s));
                    }
                    details.push_str(&format!("or {}", expected.last().unwrap()));
                    details
                };

                self.fmt_details(&details, *start, *end, src);
            },
            ParseError::ExtraToken { start, token, end } => {
                print!("extra token {}", token);
                self.fmt_details("", *start, *end, src);
            },
            ParseError::User { error } => {
                print!("{}", error);
            },
            ParseError::Multiple(errors) => {
                for err in errors {
                    err.fmt(src);
                }
            },
        }
    }

    pub fn fmt_details(&self, details: &str, start: usize, end: usize, src: &str) {
        let start_line = src[0..start].chars().filter(|c| *c == '\n').count();
        // let end_line = src[0..end].chars().filter(|c| *c == '\n').count();

        let start_col = 1 + start
            - src[0..start]
                .chars()
                .enumerate()
                .filter(|(_, c)| *c == '\n')
                .last()
                .unwrap()
                .0;
        let end_col = 1 + end
            - src[0..end]
                .chars()
                .enumerate()
                .filter(|(_, c)| *c == '\n')
                .last()
                .unwrap()
                .0;

        print!(
            "\n {}{}-->{} {}:{}\n{} {}{}|\n",
            color::Fg(color::Blue),
            style::Bold,
            style::Reset,
            start_line,
            start_col,
            String::from(" ").repeat(start_line.ilog10() as usize + 1),
            color::Fg(color::Blue),
            style::Bold
        );

        println!(
            "{}{}{} | {}{}{}",
            color::Fg(color::Blue),
            style::Bold,
            start_line,
            color::Fg(color::Reset),
            style::Reset,
            src.split('\n').nth(start_line).unwrap(),
        );

        print!(
            "{}{}{} | {}{}{} {}{}{}\n\n",
            String::from(" ").repeat(start_line.ilog10() as usize + 1),
            color::Fg(color::Blue),
            style::Bold,
            String::from(" ").repeat(start_col - 1),
            color::Fg(color::Yellow),
            String::from("^").repeat((end_col - start_col).max(1)),
            details,
            color::Fg(color::Reset),
            style::Reset
        );
    }
}

pub struct Ast(pub Expr);

#[derive(Clone, Debug)]
#[allow(clippy::vec_box)]
pub enum Expr {
    /// An integer.
    Integer(i32),
    /// A real number.
    Real(f32),
    /// An identifier.
    Ident(String),
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
    Error,
}

#[derive(Clone, Debug)]
pub enum BinaryOpCode {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Clone, Debug)]
pub enum UnaryOpCode {
    Neg,
}

impl FromStr for Ast {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut errors = Vec::new();

        let expr = match crate::grammar::ExprParser::new().parse(&mut errors, s) {
            Ok(expr) => expr,
            Err(err) => return Err(err.into()),
        };

        if !errors.is_empty() {
            return Err(ParseError::Multiple(
                errors.into_iter().map(|e| e.error.into()).collect(),
            ));
        }

        Ok(Ast(*expr))
    }
}

impl Expr {
    pub fn leaves(&self) -> Vec<Expr> {
        match self {
            Expr::BinaryOp { left, op: _, right } => vec![*left.clone(), *right.clone()],
            Expr::UnaryOp { op: _, expr } => vec![*expr.clone()],
            Expr::Call { name: _, args } => args.iter().map(|e| *e.clone()).collect(),
            _ => vec![],
        }
    }
}

type DisplayQueue = VecDeque<(bool, Expr, Vec<bool>)>;

const GLYPH_MIDDLE_ITEM: &str = "├";
const GLYPH_LAST_ITEM: &str = "└";
const GLYPH_ITEM_INDENT: &str = "── ";
const GLYPH_MIDDLE_SKIP: &str = "│";
const GLYPH_LAST_SKIP: &str = " ";
const GLYPH_SKIP_INDENT: &str = "  ";

fn enqueue_leaves(queue: &mut DisplayQueue, parent: &Expr, spaces: Vec<bool>) {
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
            Expr::Ident(v) => write!(f, "{}{:?}", color::Fg(color::White), v)?,
            Expr::Error => write!(f, "{}error", color::Fg(color::Yellow))?,
            Expr::BinaryOp { op, .. } => match op {
                BinaryOpCode::Add => write!(f, "{}Add", color::Fg(color::Cyan))?,
                BinaryOpCode::Sub => write!(f, "{}Sub", color::Fg(color::Cyan))?,
                BinaryOpCode::Mul => write!(f, "{}Mul", color::Fg(color::Cyan))?,
                BinaryOpCode::Div => write!(f, "{}Div", color::Fg(color::Cyan))?,
                BinaryOpCode::Pow => write!(f, "{}Pow", color::Fg(color::Cyan))?,
            },
            Expr::UnaryOp { op, .. } => match op {
                UnaryOpCode::Neg => write!(f, "{}Neg", color::Fg(color::Cyan))?,
            },
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
