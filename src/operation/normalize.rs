use std::cmp::Ordering;

use ordered_float::OrderedFloat;

use crate::{
    context::Symbol,
    expr::{Add, Expr, Fun, Mul, Num, Pow, Var},
};

impl Expr {
    fn cmp_factors(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Expr::Num(_), Expr::Num(_)) => Ordering::Equal,
            (Expr::Num(_), _) => Ordering::Greater,
            (_, Expr::Num(_)) => Ordering::Less,
            (Expr::Var(v1), Expr::Var(v2)) => v1.id.cmp(&v2.id),
            (Expr::Pow(p1), Expr::Pow(p2)) => p1.base.cmp(&p2.base),
            (_, Expr::Pow(p2)) => self.cmp(&p2.base).then(Ordering::Less),
            (Expr::Pow(p1), _) => (*p1.base).cmp(other).then(Ordering::Less),
            (Expr::Var(_), _) => Ordering::Less,
            (_, Expr::Var(_)) => Ordering::Greater,
            (Expr::Mul(_), _) | (_, Expr::Mul(_)) => {
                unreachable!()
            },
            (Expr::Add(a1), Expr::Add(a2)) => {
                let len_cmp = a1.terms.len().cmp(&a2.terms.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in a1.terms.iter().zip(a2.terms.iter()) {
                    let term_cmp = x1.cmp(x2);
                    if term_cmp != Ordering::Equal {
                        return term_cmp;
                    }
                }

                Ordering::Equal
            },
            (Expr::Add(_), _) => Ordering::Less,
            (_, Expr::Add(_)) => Ordering::Greater,
            (Expr::Fun(f1), Expr::Fun(f2)) => {
                let name_cmp = f1.id.cmp(&f2.id);
                if name_cmp != Ordering::Equal {
                    return name_cmp;
                }

                let len_cmp = f1.args.len().cmp(&f2.args.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in f1.args.iter().zip(f2.args.iter()) {
                    let arg_cmp = x1.cmp(x2);
                    if arg_cmp != Ordering::Equal {
                        return arg_cmp;
                    }
                }

                Ordering::Equal
            },
        }
    }

    fn cmp_terms(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Expr::Num(_), Expr::Num(_)) => Ordering::Equal,
            (Expr::Num(_), _) => Ordering::Greater,
            (_, Expr::Num(_)) => Ordering::Less,
            (Expr::Var(v1), Expr::Var(v2)) => v1.id.cmp(&v2.id),
            (Expr::Pow(p1), Expr::Pow(p2)) => p1.base.cmp(&p2.base).then_with(|| p1.exp.cmp(&p2.exp)),
            (Expr::Mul(m1), Expr::Mul(m2)) => {
                let len1 = if m1.has_coefficient() {
                    m1.factors.len() - 1
                } else {
                    m1.factors.len()
                };

                let len2 = if m2.has_coefficient() {
                    m2.factors.len() - 1
                } else {
                    m2.factors.len()
                };

                let len_cmp = len1.cmp(&len2);
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in m1.factors.iter().zip(m2.factors.iter()) {
                    if let Expr::Num(_) = x1 {
                        break;
                    }

                    if let Expr::Num(_) = x2 {
                        break;
                    }

                    let factor_cmp = x1.cmp(x2);
                    if factor_cmp != Ordering::Equal {
                        return factor_cmp;
                    }
                }

                Ordering::Equal
            },
            (Expr::Mul(m1), e2) => {
                if !m1.has_coefficient() || m1.factors.len() != 2 {
                    return Ordering::Greater;
                }

                m1.factors.first().unwrap().cmp(e2)
            },
            (e1, Expr::Mul(m2)) => {
                if !m2.has_coefficient() || m2.factors.len() != 2 {
                    return Ordering::Greater;
                }

                e1.cmp(m2.factors.first().unwrap())
            },
            (Expr::Var(_), _) => Ordering::Less,
            (_, Expr::Var(_)) => Ordering::Greater,
            (_, Expr::Pow(_)) => Ordering::Greater,
            (Expr::Pow(_), _) => Ordering::Less,
            (Expr::Fun(f1), Expr::Fun(f2)) => {
                let name_cmp = f1.id.cmp(&f2.id);
                if name_cmp != Ordering::Equal {
                    return name_cmp;
                }

                let len_cmp = f1.args.len().cmp(&f2.args.len());
                if len_cmp != Ordering::Equal {
                    return len_cmp;
                }

                for (x1, x2) in f1.args.iter().zip(f2.args.iter()) {
                    let arg_cmp = x1.cmp(x2);
                    if arg_cmp != Ordering::Equal {
                        return arg_cmp;
                    }
                }

                Ordering::Equal
            },
            (Expr::Add(_), _) | (_, Expr::Add(_)) => unreachable!(),
        }
    }

    fn merge_factors(&self, other: &Self) -> Option<Self> {
        if let Expr::Pow(p1) = self {
            // x^a * x^b = x^(a + b)
            if let Expr::Pow(p2) = other {
                if p1.base != p2.base {
                    return None;
                }

                if let Expr::Num(n1) = &*p1.exp
                    && let Expr::Num(n2) = &*p2.exp
                {
                    let new_exp = n1 + n2;

                    if new_exp.is_zero() {
                        return Some(Expr::Num(Num::ONE));
                    } else if new_exp.is_one() {
                        return Some(*p2.base.clone());
                    } else {
                        return Some(Expr::Pow(Pow::new(*p2.base.clone(), Expr::Num(new_exp))));
                    }
                }

                let new_exp = Expr::Add(Add::new(vec![*p1.exp.clone(), *p2.exp.clone()])).normalize();

                if let Expr::Num(n) = &new_exp {
                    if n.is_zero() {
                        return Some(Expr::Num(Num::ONE));
                    } else if n.is_one() {
                        return Some(*p2.base.clone());
                    }
                }

                return Some(Expr::Pow(Pow::new(*p2.base.clone(), new_exp)));
            }

            // x^n * x = x^(n + 1) but don't merge numbers
            if !matches!(other, Expr::Num(_)) && *other == *p1.base {
                if let Expr::Num(n) = &*p1.exp {
                    let new_exp = n + &Num::ONE;

                    if new_exp.is_zero() {
                        return Some(Expr::Num(Num::ONE));
                    } else if new_exp == Num::ONE {
                        return Some(other.clone());
                    } else {
                        return Some(Expr::Pow(Pow::new(other.clone(), Expr::Num(new_exp))));
                    }
                } else {
                    let new_exp = Expr::Add(Add::new(vec![Expr::Num(Num::ONE), *p1.exp.clone()])).normalize();
                    return Some(Expr::Pow(Pow::new(*p1.base.clone(), new_exp)));
                }
            }

            return None;
        }

        // x * x^n = x^(n + 1) but don't merge numbers
        if let Expr::Pow(p) = other {
            if !matches!(self, Expr::Num(_)) && *self == *p.base {
                if let Expr::Num(n) = &*p.exp {
                    let new_exp = n + &Num::ONE;

                    if new_exp.is_zero() {
                        return Some(Expr::Num(Num::ONE));
                    } else if new_exp == Num::ONE {
                        return Some(*p.base.clone());
                    } else {
                        return Some(Expr::Pow(Pow::new(*p.base.clone(), Expr::Num(new_exp))));
                    }
                } else {
                    let new_exp = Expr::Add(Add::new(vec![Expr::Num(Num::ONE), *p.exp.clone()])).normalize();
                    return Some(Expr::Pow(Pow::new(*p.base.clone(), new_exp)));
                }
            }

            return None;
        }

        // multiply numbers
        if let Expr::Num(n1) = self {
            if let Expr::Num(n2) = other {
                return Some(Expr::Num(n1 * n2));
            } else {
                return None;
            }
        }

        // x * x = x^2
        if self == other {
            return Some(Expr::Pow(Pow::new(self.clone(), Expr::Num(Num::Integer(2)))));
        }

        None
    }

    fn merge_terms(&self, other: &Self) -> Option<Self> {
        // add numbers
        if let Expr::Num(n1) = self {
            if let Expr::Num(n2) = other {
                return Some(Expr::Num(n1 + n2));
            } else {
                return None;
            }
        }

        // add coefficients
        if let Expr::Mul(m) = self {
            let last_term = m.factors.last().unwrap();

            let (term1, has_coeff) = if let Expr::Num(_) = &last_term {
                (&m.factors[0..m.factors.len() - 1], true)
            } else {
                (&m.factors[..], false)
            };

            if let Expr::Mul(m2) = other {
                let last_term2 = m2.factors.last().unwrap();

                let term2 = if let Expr::Num(_) = &last_term2 {
                    &m2.factors[0..m2.factors.len() - 1]
                } else {
                    &m2.factors[..]
                };

                if term1.eq(term2) {
                    let num = if let Expr::Num(n) = &last_term { n } else { &Num::ONE };

                    let new_coeff = if let Expr::Num(n) = &last_term2 {
                        num + n
                    } else {
                        num + &Num::ONE
                    };

                    let len = m.factors.len();

                    if new_coeff == Num::ONE {
                        assert!(has_coeff);

                        if len == 2 {
                            return Some(m2.factors[0].clone());
                        } else {
                            return Some(Expr::Mul(Mul::new(term2.into())));
                        }
                    }

                    if new_coeff.is_zero() {
                        return Some(Expr::Num(new_coeff));
                    }

                    let mut mc = m.clone();

                    if has_coeff {
                        mc.factors[m.factors.len() - 1] = Expr::Num(new_coeff);
                    } else {
                        mc.push(Expr::Num(new_coeff));
                    }

                    return Some(Expr::Mul(mc));
                }
            } else {
                if term1.len() != 1 || *other != m.factors[0] {
                    return None;
                }

                let new_coeff = if let Expr::Num(n) = &last_term {
                    n + &Num::ONE
                } else {
                    return None;
                };

                assert!(new_coeff != Num::ONE);

                if new_coeff.is_zero() {
                    return Some(Expr::Num(new_coeff));
                }

                let mut mc = m.clone();
                mc.factors[m.factors.len() - 1] = Expr::Num(new_coeff);

                return Some(Expr::Mul(mc));
            }
        } else if let Expr::Mul(m) = other {
            if m.factors.len() == 2 {
                return None;
            }

            let last_term = m.factors.last().unwrap();

            if *self == m.factors[0] {
                let new_coeff = if let Expr::Num(n) = &last_term {
                    n + &Num::ONE
                } else {
                    return None;
                };

                assert!(new_coeff != Num::ONE);

                if new_coeff.is_zero() {
                    return Some(Expr::Num(new_coeff));
                }

                let mut mc = m.clone();
                mc.factors[m.factors.len() - 1] = Expr::Num(new_coeff);

                return Some(Expr::Mul(mc));
            }
        } else if self == other {
            return Some(Expr::Mul(Mul::new(vec![self.clone(), Expr::Num(Num::Integer(2))])));
        };

        None
    }

    fn simplify_exp_ln(&self) -> Option<Expr> {
        if let Expr::Fun(f) = self {
            // exp[ln[x]] = x
            if f.id == Symbol::LN && f.len() == 1 {
                return Some(f.args[0].clone());
            }
        }

        if let Expr::Mul(m) = self {
            // exp[y ln[x]] = x^y
            let mut idx = None;
            let mut expr = None;
            for (i, a) in m.iter().enumerate() {
                if let Some(e) = a.simplify_exp_ln() {
                    if idx.is_some() {
                        return None;
                    }

                    idx = Some(i);
                    expr = Some(e);
                }
            }

            if let Some(i) = idx {
                let mut mul = Mul::default();
                for (j, a) in m.iter().enumerate() {
                    if j != i {
                        mul.push(a.clone());
                    }
                }

                return Some(Expr::Pow(Pow::new(expr.unwrap(), Expr::Mul(mul))).normalize());
            } else {
                return None;
            }
        }

        if let Expr::Add(a) = self {
            // exp[y + ln[x]] = x exp[y]
            let mut mul = Mul::default();
            let mut add = Add::default();

            let mut changed = false;
            for term in a.iter() {
                if let Some(e) = term.simplify_exp_ln() {
                    changed = true;
                    mul.push(e);
                } else {
                    add.push(term.clone());
                }
            }

            if changed {
                let new_exp = Expr::Pow(Pow::new(Expr::Var(Var::new(Symbol::E)), Expr::Add(add)));
                mul.push(new_exp);
                return Some(Expr::Mul(mul).normalize());
            } else {
                return None;
            }
        }

        None
    }

    pub fn normalize(self) -> Expr {
        match self {
            Expr::Num(num) => Expr::Num(num),
            Expr::Var(var) => Expr::Var(var),
            Expr::Fun(fun) => {
                let id = fun.id;
                let mut f = Fun::new(id, vec![]);

                for arg in fun {
                    f.push(arg.normalize());
                }

                if [Symbol::COS, Symbol::SIN, Symbol::LN].contains(&id) && f.len() == 1 {
                    let arg = &f.args[0];
                    if let Expr::Num(n) = arg {
                        if n.is_zero() && id != Symbol::LN || n.is_one() && id == Symbol::LN {
                            if id == Symbol::COS {
                                // cos[0] = 1
                                return Expr::Num(Num::ONE);
                            } else if id == Symbol::SIN || id == Symbol::LN {
                                // sin[0] = 0, ln[1] = 0
                                return Expr::Num(Num::ZERO);
                            }
                        }

                        if let Num::Float(f) = n {
                            match id {
                                Symbol::COS => return Expr::Num(Num::Float(OrderedFloat(f.cos()))),
                                Symbol::SIN => return Expr::Num(Num::Float(OrderedFloat(f.sin()))),
                                Symbol::LN => return Expr::Num(Num::Float(OrderedFloat(f.ln()))),
                                _ => (),
                            }
                        }
                    }
                }

                // TODO: normalize linear and symmetric function arguments

                // TODO: inline defined functions

                Expr::Fun(f)
            },
            Expr::Pow(pow) => {
                let base = pow.base.normalize();
                let exp = pow.exp.normalize();

                // 1^x = 1
                if let Expr::Num(b) = &base
                    && b.is_one()
                {
                    return Expr::Num(Num::ONE);
                }

                if let Expr::Num(e) = &exp {
                    if e.is_zero() {
                        // x^0 = 1
                        return Expr::Num(Num::ONE);
                    } else if e.is_one() {
                        // x^1 = x
                        return base;
                    } else if let Expr::Var(var) = &base {
                        // handle e^x where x is a float
                        if var.id == Symbol::E
                            && let Num::Float(f) = e
                        {
                            return Expr::Num(Num::Float(OrderedFloat(f.exp())));
                        }
                    } else if let Expr::Num(n) = base {
                        // exponentiate numbers
                        let (coeff, new_base, new_exp) = n.pow(e);

                        if !coeff.is_one() {
                            let m = Expr::Mul(Mul::new(vec![
                                Expr::Num(coeff),
                                Expr::Pow(Pow::new(Expr::Num(new_base), Expr::Num(new_exp))),
                            ]));

                            return m.normalize();
                        }

                        if new_exp.is_one() {
                            return Expr::Num(new_base);
                        }

                        return Expr::Pow(Pow::new(Expr::Num(new_base), Expr::Num(new_exp)));
                    } else if let Expr::Pow(p_base) = &base {
                        if let Num::Integer(_) = e {
                            // (x^y)^n = x^(y*n)
                            let m = Expr::Mul(Mul::new(vec![*p_base.exp.clone(), exp])).normalize();
                            return Expr::Pow(Pow::new(*p_base.base.clone(), m)).normalize();
                        }
                    } else if let Expr::Mul(m) = &base
                        && let Num::Integer(_) = e
                    {
                        // (x*y)^n = x^n * y^n
                        let mut mul = Mul::default();
                        for factor in m.iter() {
                            mul.push(Expr::Pow(Pow::new(factor.clone(), exp.clone())));
                        }

                        return Expr::Mul(mul).normalize();
                    }
                }

                if let Expr::Var(var) = &base {
                    // simplify ln inside exp
                    if var.id == Symbol::E
                        && exp.contains_symbol(Symbol::LN)
                        && let Some(e) = exp.simplify_exp_ln()
                    {
                        return e;
                    }
                }

                Expr::Pow(Pow::new(base, exp))
            },
            Expr::Mul(mul) => {
                let mut is_zero = false;
                let mut factors = Vec::new();

                for x in mul.factors {
                    let nx = x.normalize();

                    if let Expr::Mul(m) = nx {
                        for c in m.factors {
                            if let Expr::Num(n) = &c {
                                if n.is_one() {
                                    continue;
                                }

                                if n.is_zero() {
                                    is_zero = true;
                                    continue;
                                }
                            }

                            factors.push(c);
                        }
                    } else {
                        if let Expr::Num(n) = &nx {
                            if n.is_one() {
                                continue;
                            }

                            if n.is_zero() {
                                is_zero = true;
                                continue;
                            }
                        }

                        factors.push(nx);
                    }
                }

                if is_zero {
                    return Expr::Num(Num::ZERO);
                }

                factors.sort_by(|a, b| a.cmp_factors(b));

                let mut second_pass = false;
                if !factors.is_empty() {
                    let mut out = Mul::default();

                    factors.reverse();
                    let mut last_factor = factors.pop().unwrap();
                    let mut cur_len = 0;

                    while let Some(cur_factor) = factors.pop() {
                        let Some(merged_factor) = last_factor.merge_factors(&cur_factor) else {
                            if let Expr::Num(n) = &last_factor {
                                if !n.is_one() {
                                    // catch i*i = -1 not fully reducing
                                    factors.insert(0, last_factor);
                                }
                            } else {
                                out.push(last_factor);
                                cur_len += 1;
                            }

                            last_factor = cur_factor;
                            continue;
                        };

                        if let Expr::Mul(_) = &merged_factor {
                            // a sub-multiplication was created
                            second_pass = true;
                        }

                        last_factor = merged_factor;
                    }

                    if cur_len == 0 {
                        last_factor
                    } else {
                        if second_pass {
                            out.push(last_factor);
                            return Expr::Mul(out).normalize();
                        }

                        if let Expr::Num(n) = &last_factor {
                            if !n.is_one() {
                                out.push(last_factor);
                                Expr::Mul(out)
                            } else if cur_len == 1 {
                                out.factors[0].clone()
                            } else {
                                Expr::Mul(out)
                            }
                        } else {
                            out.push(last_factor);
                            Expr::Mul(out)
                        }
                    }
                } else {
                    Expr::Num(Num::ONE)
                }
            },
            Expr::Add(add) => {
                let mut terms = Vec::new();

                for x in add.terms {
                    let nx = x.normalize();

                    if let Expr::Add(a) = nx {
                        for c in a.terms {
                            if let Expr::Num(n) = &c
                                && n.is_zero()
                            {
                                continue;
                            }

                            terms.push(c);
                        }
                    } else {
                        if let Expr::Num(n) = &nx
                            && n.is_zero()
                        {
                            continue;
                        }

                        terms.push(nx);
                    }
                }

                terms.sort_by(|a, b| a.cmp_terms(b));

                if !terms.is_empty() {
                    let mut out = Add::default();

                    let mut last_term = terms[0].clone();
                    let mut cur_len = 0;

                    for cur_term in terms.iter().skip(1) {
                        let Some(merged_term) = last_term.merge_terms(cur_term) else {
                            if let Expr::Num(n) = &last_term {
                                if !n.is_zero() {
                                    out.push(last_term);
                                    cur_len += 1;
                                }
                            } else {
                                out.push(last_term);
                                cur_len += 1;
                            }

                            last_term = cur_term.clone();
                            continue;
                        };

                        last_term = merged_term;
                    }

                    if cur_len == 0 {
                        last_term
                    } else if let Expr::Num(n) = &last_term {
                        if !n.is_zero() {
                            out.push(last_term);
                            Expr::Add(out)
                        } else if cur_len == 1 {
                            out.terms[0].clone()
                        } else {
                            Expr::Add(out)
                        }
                    } else {
                        out.push(last_term);
                        Expr::Add(out)
                    }
                } else {
                    Expr::Num(Num::ZERO)
                }
            },
        }
    }
}
