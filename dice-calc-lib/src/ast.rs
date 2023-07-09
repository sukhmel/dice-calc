use crate::types::{Configuration, Expr, Value};
use itertools::Itertools;

pub struct Output {
    configuration: Configuration,
    description: Vec<(String, usize)>,
}

impl Expr {
    pub fn compile(self) -> Result<Output, ()> {
        self.compile_impl(0)
    }

    fn compile_impl(self, height: usize) -> Result<Output, ()> {
        match self {
            Expr::Help(_) => Err(()),
            Expr::Value(value) => Ok(Output {
                description: vec![(
                    format!("value {value} converted to a single-sided die thrown once"),
                    height,
                )],
                configuration: Configuration::singular(value),
            }),
            Expr::Sides(_) => {
                todo!()
            }
            Expr::Call(_, _) => {
                todo!()
            }
            Expr::Throw(what, times) => {
                let what = what.compile()?;
                let times = times.compile()?;
                todo!()
            }
            Expr::Until(_, _, _) => {
                todo!()
            }
            Expr::Add(_, _) => {
                todo!()
            }
            Expr::Sub(_, _) => {
                todo!()
            }
            Expr::Mul(_, _) => {
                todo!()
            }
            Expr::Div(_, _) => {
                todo!()
            }
            Expr::Parenthesis(expr) => expr.compile_impl(height),
        }
    }

    pub fn help(self) -> Result<String, ()> {
        let expr = match self {
            Expr::Help(expr) => *expr,
            other => other,
        };
        Ok(expr
            .compile_impl(0)?
            .description
            .into_iter()
            .map(|(help, height)| format!("{} - {help}", " ".repeat(height * 2)))
            .join("\n"))
    }
}
