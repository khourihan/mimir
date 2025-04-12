use itertools::Itertools;

use crate::expr::{Expr, Statement};

use super::Operation;

#[derive(Clone, Debug)]
pub struct ReduceOp {
    pub reductions: Vec<Reduction>,
    pub prev_reduction: usize,
}

impl ReduceOp {
    pub fn new(reductions: Vec<Reduction>) -> Self {
        Self {
            reductions,
            prev_reduction: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Reduction {
    pub pattern: Box<Expr>,
    pub result: Box<Expr>,
}

#[derive(Debug)]
pub struct ReduceError {}

impl Operation for ReduceOp {
    type Error = ReduceError;

    fn apply(&mut self, expr: Box<Expr>) -> Result<Vec<Box<Expr>>, Self::Error> {
        let mut leaf_reductions = Vec::new();
        leaf_reductions.push(expr.clone());

        leaf_reductions.extend(match *expr {
            Expr::BinaryOp { left, op, right } => self
                .apply(left)?
                .into_iter()
                .cartesian_product(self.apply(right)?)
                .map(|(l, r)| Box::new(Expr::BinaryOp { left: l, op, right: r }))
                .collect(),
            Expr::UnaryOp { op, expr } => self
                .apply(expr)?
                .into_iter()
                .map(|e| Box::new(Expr::UnaryOp { op, expr: e }))
                .collect(),
            Expr::Call { name, args } => args
                .into_iter()
                .map(|arg| self.apply(arg))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .multi_cartesian_product()
                .map(|args| {
                    Box::new(Expr::Call {
                        name: name.clone(),
                        args,
                    })
                })
                .collect(),
            Expr::Block {
                stmts,
                expr: block_expr,
            } => {
                self.reductions.extend(stmts.into_iter().flat_map(|stmt| {
                    if let Statement::Reduction { pattern, result } = *stmt {
                        Some(Reduction { pattern, result })
                    } else {
                        None
                    }
                }));

                if let Some(e) = block_expr {
                    self.apply(e)?
                } else {
                    Vec::new()
                }
            },
            _ => Vec::new(),
        });

        let mut results = Vec::new();
        for result in leaf_reductions {
            let mut reduced = Vec::new();
            for (i, reduction) in self.reductions.iter().enumerate() {
                if self.prev_reduction == i {
                    continue;
                }

                if let Some(vars) = result.matches(&reduction.pattern) {
                    reduced.push((i, Box::new(reduction.result.expand(&vars))));
                }
            }

            results.extend(
                reduced
                    .into_iter()
                    .map(|(i, e)| {
                        self.prev_reduction = i;
                        self.apply(e)
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten(),
            );
        }

        Ok(results)
    }
}
