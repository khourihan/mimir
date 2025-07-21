use std::collections::HashSet;

use itertools::Itertools;

use crate::expr::{Expr, Pat};

use super::Operation;

#[derive(Clone, Debug)]
pub struct ReduceOp {
    pub reductions: Vec<Reduction>,
    pub visited: HashSet<Expr>,
}

impl ReduceOp {
    pub fn new(reductions: Vec<Reduction>) -> Self {
        Self {
            reductions,
            visited: HashSet::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Reduction {
    pub pattern: Box<Pat>,
    pub result: Box<Expr>,
}

#[derive(Debug)]
pub struct ReduceError {}

impl Operation for ReduceOp {
    type Error = ReduceError;

    fn apply(&mut self, expr: Box<Expr>) -> Result<Vec<Box<Expr>>, Self::Error> {
        if self.visited.contains(&expr) {
            return Ok(Vec::new());
        }

        let mut leaf_reductions = Vec::new();
        let expr_clone = expr.clone();

        leaf_reductions.extend(match *expr {
            Expr::Call { name, args } => args
                .into_iter()
                .map(|arg| self.apply(arg))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .multi_cartesian_product()
                .map(|a| {
                    Box::new(Expr::Call {
                        name: name.clone(),
                        args: a,
                    })
                })
                .collect(),
            _ => vec![expr_clone],
        });

        let mut results = Vec::new();
        for result in leaf_reductions {
            let mut reduced = Vec::new();
            for reduction in self.reductions.iter() {
                if let Some(vars) = result.matches(&reduction.pattern) {
                    reduced.push(Box::new(reduction.result.expand(&vars)));
                }
            }

            self.visited.insert(*result.clone());

            results.extend(
                reduced
                    .into_iter()
                    .map(|e| self.apply(e))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten(),
            );

            self.visited.remove(&result);
            results.push(result);
        }

        Ok(results)
    }
}

// _a + _b => b + a; _a * _b => b * a; _b * _a + _c * _a => (b + c) * a; ((5x)x + (5x)2) + ((3)x + (3)2)
// Also: (a + b) + c => a + (b + c)
