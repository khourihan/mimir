use std::{fmt, ops::Index};

use crate::{
    Add, F64, Fun, Integer, Rational, SingleFloat, Var,
    context::Context,
    expr::{Mul, Pow},
};

use super::{Expr, ExprType, Num};

pub struct FormatContext {
    pub last_expr: ExprType,
    pub in_exp: bool,
    pub in_base: bool,
    pub exclude_neg: bool,
}

impl Default for FormatContext {
    fn default() -> Self {
        Self {
            last_expr: ExprType::Add,
            in_exp: false,
            in_base: false,
            exclude_neg: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FormatBlock {
    lines: Vec<String>,
    center: usize,
}

impl FormatBlock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_line(s: String) -> Self {
        Self {
            lines: vec![s],
            center: 0,
        }
    }

    /// Stacks two formatting blocks horizontally, placing `other` to the right of `self`,
    /// separated by `sep`.
    pub fn stack_horizontal(&mut self, other: &Self, sep: &str) {
        let len = self.lines.iter().map(|s| s.len()).max().unwrap();

        if self.lines.len() < other.lines.len() {
            if other.center > self.center {
                let d = other.center - self.center;
                self.center += d;

                for _ in 0..d {
                    self.lines.insert(0, String::new());
                }
            }

            for _ in 0..other.lines.len() - self.lines.len() {
                self.lines.push(String::new());
            }
        }

        for (i, line) in self.lines.iter_mut().enumerate() {
            let pad = len - line.len();
            line.push_str(&" ".repeat(pad));

            if i == self.center {
                line.push_str(sep);
            } else {
                line.push_str(&" ".repeat(sep.len()));
            }

            if i + other.center >= self.center
                && let Some(other_line) = other.lines.get(i + other.center - self.center)
            {
                line.push_str(other_line);
            }
        }
    }

    /// Stacks two formatting blocks vertically, placing `self` above `other`, separated by `sep`.
    pub fn stack_vertical_above(&mut self, other: &Self, sep: Option<char>) {
        let top_len = self.lines.iter().map(|s| s.len()).max().unwrap();
        let bottom_len = other.lines.iter().map(|s| s.len()).max().unwrap();

        if let Some(sep) = sep {
            self.lines.insert(0, sep.to_string().repeat(top_len.max(bottom_len)));
        }

        self.center = other.lines.len();

        for line in other.lines.iter().rev() {
            self.lines.insert(0, line.to_string());
        }

        for (i, line) in self.lines.iter_mut().enumerate() {
            if top_len > bottom_len && i < self.center {
                line.insert_str(0, &" ".repeat((top_len - bottom_len) / 2));
            } else if bottom_len > top_len && i > self.center {
                line.insert_str(0, &" ".repeat((bottom_len - top_len) / 2));
            }
        }
    }

    /// Stacks two formatting blocks vertically, placing `self` below `other`, separated by `sep`.
    pub fn stack_vertical_below(&mut self, other: &Self, sep: Option<char>) {
        let top_len = other.lines.iter().map(|s| s.len()).max().unwrap();
        let bottom_len = self.lines.iter().map(|s| s.len()).max().unwrap();

        if let Some(sep) = sep {
            self.lines.push(sep.to_string().repeat(top_len.max(bottom_len)));
        }

        self.center = self.lines.len() - 1;

        for line in other.lines.iter() {
            self.lines.push(line.to_string());
        }

        for (i, line) in self.lines.iter_mut().enumerate() {
            if top_len > bottom_len && i < self.center {
                line.insert_str(0, &" ".repeat((top_len - bottom_len) / 2));
            } else if bottom_len > top_len && i > self.center {
                line.insert_str(0, &" ".repeat((bottom_len - top_len) / 2));
            }
        }
    }

    /// Stacks two formatting blocks vertically, placing `self` below `other`, offsetting `other`
    /// by the length of `self`.
    pub fn stack_vertical_below_offset(&mut self, other: &Self) {
        let len = self.lines.iter().map(|s| s.len()).max().unwrap();
        let bottom_height = self.lines.len();

        for line in other.lines.iter() {
            self.lines.push(line.to_string());
        }

        for (i, line) in self.lines.iter_mut().enumerate() {
            if i >= bottom_height {
                line.insert_str(0, &" ".repeat(len));
            }
        }
    }

    fn get_index(&mut self, index: isize) -> usize {
        if index.is_negative() {
            if index.unsigned_abs() > self.center {
                let d = index.unsigned_abs() - self.center;
                self.center += d;

                for i in 0..d {
                    self.lines.insert(0, String::new());
                }
            }

            (index + self.center as isize) as usize
        } else {
            if index as usize > self.center {
                let d = index as usize - self.center;

                for i in 0..d {
                    self.lines.push(String::new());
                }
            }

            (index + self.center as isize) as usize
        }
    }

    pub fn get(&mut self, index: isize) -> &String {
        let i = self.get_index(index);
        &self.lines[i]
    }

    pub fn get_mut(&mut self, index: isize) -> &mut String {
        let i = self.get_index(index);
        &mut self.lines[i]
    }

    pub fn lines(&self) -> impl Iterator<Item = (isize, &String)> {
        self.lines
            .iter()
            .enumerate()
            .map(|(i, l)| (i as isize - self.center as isize, l))
    }

    pub fn lines_mut(&mut self) -> impl Iterator<Item = (isize, &mut String)> {
        self.lines
            .iter_mut()
            .enumerate()
            .map(|(i, l)| (i as isize - self.center as isize, l))
    }
}

impl Default for FormatBlock {
    fn default() -> Self {
        Self {
            lines: vec![String::new()],
            center: 0,
        }
    }
}

impl fmt::Display for FormatBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in self.lines.iter().rev() {
            writeln!(f, "{}", line)?;
        }

        Ok(())
    }
}

pub trait FormatExpr {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock;
}

impl FormatExpr for Expr {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        match self {
            Expr::Num(num) => num.fmt_block(ctx),
            Expr::Var(var) => var.fmt_block(ctx),
            Expr::Fun(fun) => fun.fmt_block(ctx),
            Expr::Pow(pow) => pow.fmt_block(ctx),
            Expr::Mul(mul) => mul.fmt_block(ctx),
            Expr::Add(add) => add.fmt_block(ctx),
        }
    }
}

impl FormatExpr for Num {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let mut block = FormatBlock::new();

        match self {
            Num::Rational(r) => {
                let to_string = if ctx.exclude_neg {
                    |x: &Integer| -> String { x.abs().to_string() }
                } else {
                    |x: &Integer| -> String { x.to_string() }
                };

                if r.im.is_zero() {
                    block.get_mut(0).push_str(&to_string(&r.re.numerator));

                    if !r.re.denominator.is_one() {
                        let d = FormatBlock::from_line(to_string(&r.re.denominator));
                        block.stack_vertical_above(&d, Some('-'));
                    }
                } else if r.re.is_zero() {
                    block.get_mut(0).push_str(&to_string(&r.im.numerator));

                    if !r.im.denominator.is_one() {
                        let d = FormatBlock::from_line(to_string(&r.im.denominator));
                        block.stack_vertical_above(&d, Some('-'));
                    }

                    block.get_mut(0).push('i');
                } else {
                    block.get_mut(0).push('(');

                    block.get_mut(0).push_str(&r.re.numerator.to_string());

                    if !r.re.denominator.is_one() {
                        let d = FormatBlock::from_line(r.re.denominator.to_string());
                        block.stack_vertical_above(&d, Some('-'));
                    }

                    if r.im.is_negative() {
                        block.get_mut(0).push_str(" - ");
                    } else {
                        block.get_mut(0).push_str(" + ");
                    }

                    block.get_mut(0).push_str(&r.im.numerator.to_string());

                    if !r.im.denominator.is_one() {
                        let d = FormatBlock::from_line(r.im.denominator.to_string());
                        block.stack_vertical_above(&d, Some('-'));
                    }

                    block.get_mut(0).push_str("i)");
                }
            },
            Num::Float(f) => {
                let to_string = if ctx.exclude_neg {
                    |x: &F64| -> String { x.abs().to_string() }
                } else {
                    |x: &F64| -> String { x.to_string() }
                };

                if f.im.is_zero() {
                    block.get_mut(0).push_str(&to_string(&f.re));
                } else if f.re.is_zero() {
                    block.get_mut(0).push_str(&to_string(&f.im));
                    block.get_mut(0).push('i');
                } else {
                    block.get_mut(0).push('(');
                    block.get_mut(0).push_str(&f.re.to_string());

                    if f.im.is_negative() {
                        block.get_mut(0).push_str(" - ");
                    } else {
                        block.get_mut(0).push_str(" + ");
                    }

                    block.get_mut(0).push_str(&f.im.to_string());
                    block.get_mut(0).push_str("i)");
                }
            },
        }

        block
    }
}

impl FormatExpr for Var {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let s = Context::get_name(self.id);
        FormatBlock::from_line(s)
    }
}

impl FormatExpr for Fun {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let last_expr = ctx.last_expr;
        ctx.last_expr = ExprType::Fun;

        let s = Context::get_name(self.id);
        let mut block = FormatBlock::from_line(s);

        block.stack_horizontal(&self.args[0].fmt_block(ctx), "[");

        for arg in self.iter().skip(1) {
            block.stack_horizontal(&arg.fmt_block(ctx), ", ");
        }

        block.get_mut(0).push(']');

        ctx.last_expr = last_expr;
        block
    }
}

impl FormatExpr for Pow {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let last_expr = ctx.last_expr;
        ctx.last_expr = ExprType::Pow;

        let mut block = FormatBlock::new();

        let in_base = ctx.in_base;
        if in_base {
            block.get_mut(0).push('(');
        }

        ctx.in_base = true;
        block.stack_horizontal(&self.base.fmt_block(ctx), "");
        ctx.in_base = in_base;

        let in_exp = ctx.in_exp;
        let exclude_neg = ctx.exclude_neg;
        ctx.in_exp = true;
        ctx.exclude_neg = true;
        block.stack_vertical_below_offset(&self.exp.fmt_block(ctx));
        ctx.in_exp = in_exp;
        ctx.exclude_neg = exclude_neg;

        if is_negated(&self.exp) {
            block.stack_vertical_below(&FormatBlock::from_line(String::from("1")), Some('-'));
        }

        if in_base {
            block.get_mut(0).push(')');
        }

        ctx.last_expr = last_expr;
        block
    }
}

impl FormatExpr for Mul {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let last_expr = ctx.last_expr;
        ctx.last_expr = ExprType::Mul;

        let mut block = FormatBlock::new();

        fn fmt_factors(m: &[Expr], ctx: &mut FormatContext) -> FormatBlock {
            if m.len() == 1 {
                return m[0].fmt_block(ctx);
            }

            let mut block = FormatBlock::new();

            let factors = if m.iter().any(|e| matches!(e, Expr::Num(_))) {
                let coeff = m.last().unwrap();
                let Expr::Num(n) = coeff else { unreachable!() };

                if n.is_negative_one() && !ctx.exclude_neg {
                    block.get_mut(0).push('-');
                } else if !n.is_negative_one() {
                    if ctx.exclude_neg {
                        block.stack_horizontal(&n.abs_real().fmt_block(ctx), "");
                    } else {
                        block.stack_horizontal(&n.fmt_block(ctx), "");
                    }
                    block.get_mut(0).push(' ');
                }

                block.stack_horizontal(&m[0].fmt_block(ctx), "");
                &m[1..m.len() - 1]
            } else {
                block.stack_horizontal(&m[0].fmt_block(ctx), "");
                &m[1..]
            };

            for factor in factors {
                block.stack_horizontal(&factor.fmt_block(ctx), " ");
            }

            block
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

        let has_denom = self.iter().any(is_denom);

        if has_denom {
            let num: Vec<_> = self.factors.iter().filter(|&e| !is_denom(e)).cloned().collect();
            let den: Vec<_> = self
                .factors
                .iter()
                .filter_map(|e| {
                    if let Expr::Pow(p) = e {
                        if let Expr::Num(n) = &*p.exp {
                            if n.is_negative() {
                                if n.is_negative_one() {
                                    Some(*p.base.clone())
                                } else {
                                    Some(Expr::Pow(Pow::new(*p.base.clone(), Expr::Num(n.abs_real()))))
                                }
                            } else {
                                None
                            }
                        } else if let Expr::Mul(m) = &*p.exp {
                            if let Some(Expr::Num(n)) = m.factors.last() {
                                if n.is_negative() {
                                    let mut factors: Vec<_> =
                                        m.factors.iter().take(m.factors.len() - 1).cloned().collect();
                                    factors.push(Expr::Num(n.abs_real()));
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

            block.stack_horizontal(&fmt_factors(&num, ctx), "");
            block.stack_vertical_above(&fmt_factors(&den, ctx), Some('-'));
        } else {
            block.stack_horizontal(&fmt_factors(&self.factors, ctx), "");
        }

        if last_expr == ExprType::Pow {
            block.get_mut(0).insert(0, '(');
            block.get_mut(0).push(')');

            for (i, line) in block.lines_mut() {
                if i != 0 {
                    line.insert(0, ' ');
                }
            }
        }

        ctx.last_expr = last_expr;
        block
    }
}

impl FormatExpr for Add {
    fn fmt_block(&self, ctx: &mut FormatContext) -> FormatBlock {
        let last_expr = ctx.last_expr;
        ctx.last_expr = ExprType::Add;

        let mut block = FormatBlock::new();

        if last_expr == ExprType::Mul || last_expr == ExprType::Pow {
            block.get_mut(0).push('(');
        }

        block.stack_horizontal(&self.terms[0].fmt_block(ctx), "");

        for term in self.iter().skip(1) {
            if let Expr::Mul(m) = term {
                if m.has_coefficient() {
                    let coeff = m.factors.last().unwrap();
                    let Expr::Num(n) = coeff else { unreachable!() };

                    if n.is_negative() {
                        let exclude_neg = ctx.exclude_neg;
                        ctx.exclude_neg = true;
                        block.stack_horizontal(&m.fmt_block(ctx), " - ");
                        ctx.exclude_neg = exclude_neg;
                        continue;
                    }
                }
            } else if let Expr::Num(n) = term
                && n.is_negative()
            {
                block.stack_horizontal(&n.abs_real().fmt_block(ctx), " - ");
                continue;
            }

            block.stack_horizontal(&term.fmt_block(ctx), " + ");
        }

        if last_expr == ExprType::Mul || last_expr == ExprType::Pow {
            block.get_mut(0).push(')');
        }

        ctx.last_expr = last_expr;
        block
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.fmt_block(&mut FormatContext::default()))
    }
}

fn is_negated(e: &Expr) -> bool {
    match e {
        Expr::Num(num) => match num {
            Num::Rational(r) => r.re.is_zero() && r.im.is_negative() || r.im.is_zero() && r.re.is_negative(),
            Num::Float(f) => f.re.is_zero() && f.im.is_negative() || f.im.is_zero() && f.re.is_negative(),
        },
        Expr::Mul(mul) => mul.factors.iter().filter(|e| is_negated(e)).count() % 2 == 1,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::fmt::FormatBlock;

    #[test]
    fn test_line_fmt() {
        let mut l = FormatBlock::new();
        l.get_mut(1).push_str(" 1 ");
        l.get_mut(0).push_str("---");
        l.get_mut(-1).push_str(" 2 ");

        assert_eq!(" 1 \n---\n 2 \n", format!("{}", l));

        let mut l = FormatBlock::new();
        l.get_mut(1).push_str(" 2");
        l.get_mut(0).push('x');

        assert_eq!(" 2\nx\n", format!("{}", l));
    }

    #[test]
    fn test_line_stack_horizontal() {
        let mut l1 = FormatBlock::new();
        let mut l2 = FormatBlock::new();

        l1.get_mut(1).push_str(" 1 ");
        l1.get_mut(0).push_str("---");
        l1.get_mut(-1).push_str(" 2 ");

        l2.get_mut(1).push_str(" 2");
        l2.get_mut(0).push('x');

        let l3 = l1.clone();
        l1.stack_horizontal(&l2, " + ");

        assert_eq!(" 1     2\n--- + x\n 2    \n", format!("{}", l1));

        l2.stack_horizontal(&l3, " + ");
        assert_eq!(" 2    1 \nx  + ---\n      2 \n", format!("{}", l2));
    }

    #[test]
    fn test_line_stack_vertical() {
        let mut l1 = FormatBlock::new();
        let mut l2 = FormatBlock::new();
        let mut l3 = FormatBlock::new();

        l1.get_mut(1).push('1');
        l1.get_mut(0).push('-');
        l1.get_mut(-1).push('2');

        l2.get_mut(1).push_str(" 2");
        l2.get_mut(0).push('x');

        l3.get_mut(0).push('y');

        l1.stack_horizontal(&l3, " + ");

        let l4 = l1.clone();
        l1.stack_vertical_above(&l2, Some('-'));

        assert_eq!("1   \n- + y\n2   \n-----\n  2\n x\n", format!("{}", l1));

        l2.stack_vertical_above(&l4, Some('-'));
        assert_eq!("  2\n x\n-----\n1   \n- + y\n2   \n", format!("{}", l2));
    }
}
