use lalrpop_util::lalrpop_mod;
use mimir_macros::{IterEnum, StringifyEnum};
use parser::Ast;
use rustyline::{Config, EditMode, Editor, error::ReadlineError, history::DefaultHistory};
use termion::{color, cursor, style};

mod expr;
mod parser;
mod strutils;
lalrpop_mod!(pub grammar, "/grammar.rs");

#[derive(Clone, Copy, IterEnum, StringifyEnum)]
pub enum RunMethod {
    ShowAST,
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
    let mut src = String::new();
    let mut line = 1;

    println!();

    loop {
        match stdin.readline(&format!("[{}] >> ", line)) {
            Ok(input) => {
                RunMethod::iter_fields().enumerate().for_each(|(i, x)| {
                    println!(
                        "{}{}[{}] {}{}{}",
                        color::Fg(color::Green),
                        style::Bold,
                        i,
                        color::Fg(color::Reset),
                        style::Reset,
                        x.stringify_pretty()
                    );
                });

                let Ok(opts) = select_method(&mut stdin, 0) else { break };

                let mut src_new = src.clone();
                if line != 1 {
                    src_new.push(';');
                    src_new.push('\n');
                }
                src_new.push_str(&input);
                strutils::bracket(&mut src_new, "{\n", "\n}");

                let ast: Ast = match src_new.parse() {
                    Ok(ast) => {
                        strutils::unquote(&mut src_new);
                        strutils::unquote(&mut src_new);
                        src = src_new;
                        line += 1;
                        ast
                    },
                    Err(err) => {
                        err.fmt(&src_new);
                        continue;
                    },
                };

                match opts {
                    RunMethod::ShowAST => {
                        print!("{}", ast.0);
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
