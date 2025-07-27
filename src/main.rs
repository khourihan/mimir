use lalrpop_util::lalrpop_mod;
use mimir_macros::{IterEnum, StringifyEnum};
use parser::{Ast, ParseError};
use rustyline::{Config, EditMode, Editor, error::ReadlineError, history::DefaultHistory};
use termion::{color, cursor, style};

mod expr;
mod operation;
mod parser;
lalrpop_mod!(pub grammar, "/grammar.rs");

#[derive(Clone, Copy, IterEnum, StringifyEnum)]
pub enum RunMethod {
    Normalize,
}

pub fn select_method(stdin: &mut Editor<(), DefaultHistory>, depth: u16) -> rustyline::Result<RunMethod> {
    match stdin.readline("= ") {
        Ok(input) => {
            if let Ok(index) = input.parse::<usize>() {
                if let Some(opt) = RunMethod::iter_fields().nth(index) {
                    print!(
                        "{}{}",
                        cursor::Up(RunMethod::iter_fields().count() as u16 + 1 + depth),
                        termion::clear::AfterCursor
                    );
                    Ok(opt)
                } else {
                    select_method(stdin, depth + 1)
                }
            } else {
                select_method(stdin, depth + 1)
            }
        },
        Err(err) => Err(err),
    }
}

fn main() {
    let mut stdin =
        Editor::<(), DefaultHistory>::with_config(Config::builder().edit_mode(EditMode::Vi).build()).unwrap();
    let mut line = 1;

    println!();

    loop {
        match stdin.readline(&format!("[{}] >> ", line)) {
            Ok(mut input) => {
                RunMethod::iter_fields().enumerate().for_each(|(i, x)| {
                    println!(
                        "{}{}[{}] {}{}{}",
                        color::Fg(color::Green),
                        style::Bold,
                        i,
                        color::Fg(color::Reset),
                        style::Reset,
                        x.stringify_field()
                    );
                });

                let Ok(opts) = select_method(&mut stdin, 0) else { break };

                if let Err(err) = input.parse::<Ast>() {
                    if let ParseError::UnexpectedEof { .. } = err {
                        input.insert(0, '\n');
                        err.fmt(&input, line - 1);
                        continue;
                    };
                };

                let ast: Ast = match input.parse() {
                    Ok(ast) => {
                        line += 1;
                        ast
                    },
                    Err(err) => {
                        input.insert(0, '\n');
                        err.fmt(&input, line);
                        continue;
                    },
                };

                match opts {
                    RunMethod::Normalize => {
                        print!("{}", ast.expr.normalize());
                    },
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                break;
            },
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            },
            Err(err) => {
                println!(
                    "{}stdin error{}: {}",
                    color::Fg(color::Red),
                    err,
                    color::Fg(color::Reset)
                );
                break;
            },
        }
    }
}
