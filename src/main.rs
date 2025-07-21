use expr::Statement;
use lalrpop_util::lalrpop_mod;
use mimir_macros::{IterEnum, StringifyEnum};
use operation::{
    Operation,
    reduce::{ReduceOp, Reduction},
};
use parser::{Ast, ParseError};
use rustyline::{Config, EditMode, Editor, error::ReadlineError, history::DefaultHistory};
use termion::{color, cursor, style};

mod expr;
mod operation;
mod parser;
mod strutils;
lalrpop_mod!(pub grammar, "/grammar.rs");

#[derive(Clone, Copy, IterEnum, StringifyEnum)]
pub enum RunMethod {
    ShowAST,
    Reduce,
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

    let mut stmts = Vec::new();

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

                let mut src_new = src.clone();
                if line != 1 {
                    if let Some((left, _right)) = src_new.rsplit_once(';') {
                        src_new = left.to_string();
                        src_new.push(';');
                        src_new.push('\n');
                    } else {
                        src_new.clear();
                        line = 1;
                    }
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
                        err.fmt(&src_new, 0);
                        continue;
                    },
                };

                match opts {
                    RunMethod::ShowAST => {
                        for stmt in ast.stmts {
                            println!("{}", stmt);
                        }

                        if let Some(e) = ast.expr {
                            print!("{}", e);
                        }
                    },
                    RunMethod::Reduce => {
                        let mut reductions = Vec::new();

                        let Some(expr) = ast.expr else {
                            continue;
                        };

                        stmts.extend(ast.stmts);

                        reductions.extend(stmts.iter().flat_map(|stmt| {
                            if let Statement::Reduction { pattern, result } = *stmt.clone() {
                                Some(Reduction { pattern, result })
                            } else {
                                None
                            }
                        }));

                        let mut reduce = ReduceOp::new(reductions);
                        let mut results = reduce.apply(Box::new(expr)).unwrap();
                        results.dedup();
                        println!();
                        for result in results {
                            println!("{}", *result);
                        }
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
