use std::str::FromStr;

use crate::expr::{Expr, Statement};

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
    pub fn fmt(&self, src: &str, line: usize) {
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
                self.fmt_details("", *location - 1, *location, src, line);
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

                self.fmt_details(&details, *location - 1, *location, src, line);
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

                self.fmt_details(&details, *start, *end, src, line);
            },
            ParseError::ExtraToken { start, token, end } => {
                print!("extra token {}", token);
                self.fmt_details("", *start, *end, src, line);
            },
            ParseError::User { error } => {
                print!("{}", error);
            },
            ParseError::Multiple(errors) => {
                for err in errors {
                    err.fmt(src, line);
                }
            },
        }
    }

    pub fn fmt_details(&self, details: &str, start: usize, end: usize, src: &str, line: usize) {
        let start_line = src[0..=start].chars().filter(|c| *c == '\n').count();

        let start_col = 1 + start
            - src[0..=start]
                .chars()
                .enumerate()
                .filter(|(_, c)| *c == '\n')
                .last()
                .unwrap()
                .0;
        let end_col = 1 + end
            - src[0..=end]
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
            start_line + line,
            start_col,
            String::from(" ").repeat((start_line + line).ilog10() as usize + 1),
            color::Fg(color::Blue),
            style::Bold
        );

        println!(
            "{}{}{} | {}{}{}",
            color::Fg(color::Blue),
            style::Bold,
            start_line + line,
            color::Fg(color::Reset),
            style::Reset,
            src.split('\n').nth(start_line).unwrap(),
        );

        print!(
            "{}{}{} | {}{}{} {}{}{}\n\n",
            String::from(" ").repeat((start_line + line).ilog10() as usize + 1),
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

#[allow(clippy::vec_box)]
pub struct Ast {
    pub expr: Option<Expr>,
    pub stmts: Vec<Box<Statement>>,
}

impl FromStr for Ast {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut errors = Vec::new();
        let mut stmts = Vec::new();

        let expr = match crate::grammar::ExprParser::new().parse(&mut errors, &mut stmts, s) {
            Ok(expr) => expr,
            Err(err) => return Err(err.into()),
        };

        if !errors.is_empty() {
            return Err(ParseError::Multiple(
                errors.into_iter().map(|e| e.error.into()).collect(),
            ));
        }

        Ok(Ast {
            expr: expr.map(|e| *e),
            stmts,
        })
    }
}
