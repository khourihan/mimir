use std::cmp::Ordering;

use crate::expr::{Add, Expr, Mul, Num};

impl Expr {
    pub fn cmp_factors(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Expr::Num(_), Expr::Num(_)) => Ordering::Equal,
            (Expr::Num(_), _) => Ordering::Greater,
            (_, Expr::Num(_)) => Ordering::Less,
            (Expr::Var(v1), Expr::Var(v2)) => v1.name.cmp(&v2.name),
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
                let name_cmp = f1.name.cmp(&f2.name);
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

    pub fn cmp_terms(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Expr::Num(_), Expr::Num(_)) => Ordering::Equal,
            (Expr::Num(_), _) => Ordering::Greater,
            (_, Expr::Num(_)) => Ordering::Less,
            (Expr::Var(v1), Expr::Var(v2)) => v1.name.cmp(&v2.name),
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
                let name_cmp = f1.name.cmp(&f2.name);
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

    pub fn normalize(self) -> Expr {
        match self {
            Expr::Num(num) => Expr::Num(num),
            Expr::Var(var) => Expr::Var(var),
            Expr::Fun(fun) => Expr::Fun(fun),
            Expr::Pow(pow) => Expr::Pow(pow),
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
                    return Expr::Num(Num::Integer(0));
                }

                factors.sort_by(|a, b| a.cmp_factors(b));

                Expr::Mul(Mul::new(factors))
            },
            Expr::Add(add) => {
                let mut terms = Vec::new();

                for x in add.terms {
                    let nx = x.normalize();

                    if let Expr::Add(a) = nx {
                        for c in a.terms {
                            if let Expr::Num(n) = &c {
                                if n.is_zero() {
                                    continue;
                                }
                            }

                            terms.push(c);
                        }
                    } else {
                        if let Expr::Num(n) = &nx {
                            if n.is_zero() {
                                continue;
                            }
                        }

                        terms.push(nx);
                    }
                }

                terms.sort_by(|a, b| a.cmp_terms(b));

                Expr::Add(Add::new(terms))
            },
        }
    }
}
