use std::fmt;

use crate::{
    context::Context,
    expr::{Mul, Pow},
};

use super::{Expr, ExprType, Num};

struct PrintContext {
    line: usize,
    last_expr: ExprType,
    in_exp: bool,
    in_base: bool,
    added_above: usize,
    added_below: usize,
    depths: Vec<usize>,
    depth: usize,
}

impl Default for PrintContext {
    fn default() -> Self {
        Self {
            line: 0,
            last_expr: ExprType::Add,
            in_exp: false,
            in_base: false,
            added_above: 0,
            added_below: 0,
            depths: vec![0],
            depth: 0,
        }
    }
}

impl Expr {
    fn fmt_lines(&self, lines: &mut Vec<String>, ctx: &mut PrintContext) {
        let last_expr = ctx.last_expr;
        ctx.last_expr = self.expr_type();

        match self {
            Expr::Num(num) => match num {
                Num::Integer(int) => {
                    let s = int.to_string();
                    lines[ctx.line].push_str(&s);
                },
                Num::Float(float) => {
                    let s = float.to_string();
                    lines[ctx.line].push_str(&s);
                },
                Num::Rational(n, d) => {
                    let sn = n.to_string();
                    let sd = d.to_string();

                    if !ctx.in_exp {
                        let len = sn.len().max(sd.len());
                        let n_bigger = sn.len() == len;

                        let prev_col = lines[ctx.line].len();
                        ctx.line += 1;
                        ctx.depth += 1;

                        let line_len = if let Some(line) = lines.get(ctx.line) {
                            if ctx.depth == ctx.depths[ctx.line] {
                                line.len()
                            } else {
                                ctx.added_above += 1;
                                lines.insert(ctx.line, String::new());
                                ctx.depths.insert(ctx.line, ctx.depth);
                                0
                            }
                        } else {
                            ctx.added_above += 1;
                            lines.insert(ctx.line, String::new());
                            ctx.depths.insert(ctx.line, ctx.depth);
                            0
                        };

                        let indent = if n_bigger {
                            prev_col - line_len
                        } else {
                            prev_col - line_len + (sd.len() - sn.len()) / 2
                        };

                        lines[ctx.line].push_str(&" ".repeat(indent));
                        lines[ctx.line].push_str(&sn);

                        ctx.line -= 1;

                        if ctx.line == 0 || ctx.depth != ctx.depths[ctx.line] {
                            ctx.added_below += 1;
                            lines.insert(ctx.line, String::new());
                            ctx.depths.insert(ctx.line, ctx.depth);
                        } else {
                            ctx.line -= 1;
                        }

                        let line_len = lines[ctx.line].len();

                        let indent = if !n_bigger {
                            prev_col - line_len
                        } else {
                            prev_col - line_len + (sn.len() - sd.len()) / 2
                        };

                        lines[ctx.line].push_str(&" ".repeat(indent));
                        lines[ctx.line].push_str(&sd);

                        ctx.line += 1;
                        ctx.depth -= 1;
                        lines[ctx.line].push_str(&"-".repeat(len));
                    } else {
                        lines[ctx.line].push_str(&(sn + "/" + &sd));
                    }
                },
            },
            Expr::Var(var) => {
                let s = Context::get_name(var.id);
                lines[ctx.line].push_str(&s);
            },
            Expr::Fun(fun) => {
                let s = Context::get_name(fun.id);
                lines[ctx.line].push_str(&s);
                lines[ctx.line].push('[');

                fun.args[0].fmt_lines(lines, ctx);

                for arg in fun.iter().skip(1) {
                    lines[ctx.line].push_str(", ");

                    arg.fmt_lines(lines, ctx);
                }

                lines[ctx.line].push(']');
            },
            Expr::Pow(pow) => {
                let in_base = ctx.in_base;
                if in_base {
                    lines[ctx.line].push('(');
                }

                ctx.in_base = true;
                pow.base.fmt_lines(lines, ctx);
                ctx.in_base = in_base;

                let prev_col = lines[ctx.line].len();
                ctx.line += 1;
                ctx.depth += 1;

                let line_len = if let Some(line) = lines.get(ctx.line) {
                    if ctx.depth == ctx.depths[ctx.line] {
                        line.len()
                    } else {
                        ctx.added_above += 1;
                        lines.insert(ctx.line, String::new());
                        ctx.depths.insert(ctx.line, ctx.depth);
                        0
                    }
                } else {
                    ctx.added_above += 1;
                    lines.insert(ctx.line, String::new());
                    ctx.depths.insert(ctx.line, ctx.depth);
                    0
                };

                let indent = prev_col - line_len;
                lines[ctx.line].push_str(&" ".repeat(indent));
                ctx.in_exp = true;
                pow.exp.fmt_lines(lines, ctx);
                ctx.in_exp = false;

                let new_col = lines[ctx.line].len();
                ctx.line -= 1;
                ctx.depth -= 1;

                let indent = new_col - prev_col;
                lines[ctx.line].push_str(&" ".repeat(indent));

                if in_base {
                    lines[ctx.line].push(')');
                }
            },
            Expr::Mul(mul) => {
                if last_expr == ExprType::Pow {
                    lines[ctx.line].push('(');
                }

                fn fmt_factors(m: &[Expr], lines: &mut Vec<String>, ctx: &mut PrintContext) {
                    if m.len() == 1 {
                        m[0].fmt_lines(lines, ctx);
                        return;
                    }

                    let factors = if m.iter().any(|e| matches!(e, Expr::Num(_))) {
                        let coeff = m.last().unwrap();
                        let Expr::Num(n) = coeff else { unreachable!() };

                        if n.is_negative_one() {
                            lines[ctx.line].push('-');
                        } else {
                            coeff.fmt_lines(lines, ctx);
                            lines[ctx.line].push(' ');
                        }

                        m[0].fmt_lines(lines, ctx);
                        &m[1..m.len() - 1]
                    } else {
                        m[0].fmt_lines(lines, ctx);
                        &m[1..]
                    };

                    for factor in factors {
                        lines[ctx.line].push(' ');

                        factor.fmt_lines(lines, ctx);
                    }
                }

                let is_denom = |e: &Expr| {
                    if let Expr::Pow(p) = e {
                        if let Expr::Num(n) = &*p.exp {
                            n.is_negative()
                        } else if let Expr::Mul(m) = &*p.exp {
                            if let Some(Expr::Num(n)) = m.factors.last() {
                                n.is_negative()
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                };

                let has_denom = mul.iter().any(is_denom);

                if has_denom {
                    let num: Vec<_> = mul.factors.iter().filter(|&e| !is_denom(e)).cloned().collect();
                    let denom: Vec<_> = mul
                        .factors
                        .iter()
                        .filter_map(|e| {
                            if let Expr::Pow(p) = e {
                                if let Expr::Num(n) = &*p.exp {
                                    if n.is_negative() {
                                        if n.is_negative_one() {
                                            Some(*p.base.clone())
                                        } else {
                                            Some(Expr::Pow(Pow::new(*p.base.clone(), Expr::Num(n.abs()))))
                                        }
                                    } else {
                                        None
                                    }
                                } else if let Expr::Mul(m) = &*p.exp {
                                    if let Some(Expr::Num(n)) = m.factors.last() {
                                        if n.is_negative() {
                                            let mut factors: Vec<_> =
                                                m.factors.iter().take(m.factors.len() - 1).cloned().collect();
                                            factors.push(Expr::Num(n.abs()));
                                            Some(Expr::Pow(Pow::new(*p.base.clone(), Expr::Mul(Mul::new(factors)))))
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect();

                    let old_col = lines[ctx.line].len();
                    ctx.line += 1;
                    ctx.depth += 1;

                    let line_len = if let Some(line) = lines.get(ctx.line) {
                        if ctx.depth == ctx.depths[ctx.line] {
                            line.len()
                        } else {
                            ctx.added_above += 1;
                            lines.insert(ctx.line, String::new());
                            ctx.depths.insert(ctx.line, ctx.depth);
                            0
                        }
                    } else {
                        ctx.added_above += 1;
                        lines.insert(ctx.line, String::new());
                        ctx.depths.insert(ctx.line, ctx.depth);
                        0
                    };

                    let indent = old_col - line_len;
                    lines[ctx.line].push_str(&" ".repeat(indent));

                    let old_below = ctx.added_below;
                    fmt_factors(&num, lines, ctx);
                    let num_len = lines[ctx.line].len() - old_col;
                    let num_height = ctx.added_below - old_below;

                    ctx.line -= num_height + 1;

                    if ctx.line == 0 || ctx.depth != ctx.depths[ctx.line] {
                        ctx.added_below += 1;
                        lines.insert(ctx.line, String::new());
                        ctx.depths.insert(ctx.line, ctx.depth);
                    } else {
                        ctx.line -= 1;
                    }

                    let line_len = lines[ctx.line].len();

                    let indent = old_col - line_len;
                    lines[ctx.line].push_str(&" ".repeat(indent));

                    let old_above = ctx.added_above;
                    fmt_factors(&denom, lines, ctx);
                    let denom_len = lines[ctx.line].len() - old_col;
                    let denom_height = ctx.added_above - old_above + 1;

                    ctx.line += denom_height;
                    ctx.depth -= 1;
                    lines[ctx.line].push_str(&"-".repeat(num_len.max(denom_len)));
                } else {
                    fmt_factors(mul.factors.as_ref(), lines, ctx);
                }

                if last_expr == ExprType::Pow {
                    lines[ctx.line].push(')');
                }
            },
            Expr::Add(add) => {
                if last_expr == ExprType::Mul || last_expr == ExprType::Pow {
                    lines[ctx.line].push('(');
                }

                add.terms[0].fmt_lines(lines, ctx);

                for term in add.iter().skip(1) {
                    if let Expr::Mul(m) = term {
                        if m.has_coefficient() {
                            let coeff = m.factors.last().unwrap();
                            let Expr::Num(n) = coeff else { unreachable!() };

                            if n.is_negative() {
                                lines[ctx.line].push_str(" - ");

                                if !n.is_negative_one() {
                                    Expr::Num(n.abs()).fmt_lines(lines, ctx);
                                }

                                m.factors[0].fmt_lines(lines, ctx);

                                for factor in m.iter().take(m.factors.len() - 1).skip(1) {
                                    lines[ctx.line].push(' ');

                                    factor.fmt_lines(lines, ctx);
                                }

                                continue;
                            }
                        }
                    } else if let Expr::Num(n) = term
                        && n.is_negative()
                    {
                        lines[ctx.line].push_str(" - ");
                        Expr::Num(n.abs()).fmt_lines(lines, ctx);
                        continue;
                    }

                    lines[ctx.line].push_str(" + ");

                    term.fmt_lines(lines, ctx);
                }

                if last_expr == ExprType::Mul || last_expr == ExprType::Pow {
                    lines[ctx.line].push(')');
                }
            },
        }

        ctx.last_expr = last_expr;
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut lines = Vec::new();
        lines.push(String::new());
        self.fmt_lines(&mut lines, &mut PrintContext::default());

        for line in lines.into_iter().filter(|s| !s.is_empty()).rev() {
            writeln!(f, "{}", line)?;
        }

        Ok(())
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
