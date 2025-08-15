#![allow(unused)]

mod context;
mod expr;
mod operation;
mod parser;

lalrpop_util::lalrpop_mod!(
    #[cfg_attr(rustfmt, rustfmt_skip)]
    #[allow(unused_parens)]
    grammar
);

pub use context::*;
pub use expr::*;
pub use parser::ParseError;
