use std::fmt::Formatter;

use itertools::Itertools;
use parse_display::Display;

use crate::types::{BasicFilter, Configuration, DotExpr, Expr, Filter, Sides, Value};

#[derive(Debug, Display)]
#[display("{value}", bound(T : std::fmt::Display))]
pub struct Output<T> {
    value: T,
    description: Vec<(String, usize)>,
}

impl<T> Output<T> {
    pub fn with_added_height(self, add: usize) -> Self {
        Self {
            description: self
                .description
                .into_iter()
                .map(|(help, height)| (help, height + add))
                .collect(),
            ..self
        }
    }

    pub fn value(self) -> T {
        self.value
    }
}

pub trait Compiled {
    type Output;
    type Error;

    fn compile(self) -> Result<Output<Self::Output>, Self::Error>;
}

impl Compiled for Expr {
    type Output = Configuration;
    type Error = ();

    fn compile(self) -> Result<Output<Self::Output>, ()> {
        self.compile_impl(0)
    }
}

impl Compiled for Sides {
    type Output = Configuration;
    type Error = ();

    fn compile(self) -> Result<Output<Self::Output>, Self::Error> {
        todo!()
    }
}

impl Compiled for Filter<Expr> {
    type Output = Filter<Configuration>;
    type Error = ();

    fn compile(self) -> Result<Output<Self::Output>, Self::Error> {
        todo!()
    }
}

impl Compiled for BasicFilter<Expr> {
    type Output = BasicFilter<Configuration>;
    type Error = ();

    fn compile(self) -> Result<Output<Self::Output>, Self::Error> {
        todo!()
    }
}

impl Compiled for DotExpr<Expr> {
    type Output = DotExpr<Configuration>;
    type Error = ();

    fn compile(self) -> Result<Output<Self::Output>, Self::Error> {
        let (description, result) = match self {
            DotExpr::Filter(filter) => {
                let filter_output = filter.compile()?.with_added_height(1);
                let mut description = vec![("keep only matching results".into(), 0)];
                description.extend(filter_output.description);
                (description, DotExpr::Filter(filter_output.value))
            }
            DotExpr::Deduplicate(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("remove duplicates above certain amount".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Deduplicate(expr_output.value))
            }
            DotExpr::Low(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("keep only certain amount of lowest ranks".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Low(expr_output.value))
            }
            DotExpr::High(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("keep only certain amount of highest ranks".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::High(expr_output.value))
            }
            DotExpr::Min(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description =
                    vec![("keep only certain amount of minimum results".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Min(expr_output.value))
            }
            DotExpr::Max(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description =
                    vec![("keep only certain amount of maximum results".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Max(expr_output.value))
            }
            DotExpr::Retain(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("keep only fitting results".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Retain(expr_output.value))
            }
            DotExpr::Remove(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("remove only fitting results".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Remove(expr_output.value))
            }
            DotExpr::Union(_) => todo!(),
            DotExpr::Intersection(_) => todo!(),
            DotExpr::Difference(_) => todo!(),
            DotExpr::Xor(_) => todo!(),
            DotExpr::Sample(_) => todo!(),
            DotExpr::Count() => (
                vec![("count outcomes in the configuration".into(), 0)],
                DotExpr::Count(),
            ),
            DotExpr::Sum() => (
                vec![("count sum of each outcome in the configuration".into(), 0)],
                DotExpr::Sum(),
            ),
            DotExpr::Prod() => (
                vec![(
                    "count product of each outcome in the configuration".into(),
                    0,
                )],
                DotExpr::Prod(),
            ),
        };
        Ok(Output {
            description,
            value: result,
        })
    }
}

impl DotExpr<Configuration> {
    pub fn apply_to(
        call: Output<Self>,
        conf: Output<Configuration>,
    ) -> Result<Output<Configuration>, ()> {
        let mut description = conf.description;
        let value = match call.value {
            DotExpr::Count() => conf.value.apply(call.value),
            DotExpr::Sum() => todo!(),
            DotExpr::Prod() => todo!(),
            DotExpr::Filter(_) => todo!(),
            DotExpr::Deduplicate(_) => todo!(),
            DotExpr::Low(_) => todo!(),
            DotExpr::High(_) => todo!(),
            DotExpr::Min(_) => todo!(),
            DotExpr::Max(_) => todo!(),
            DotExpr::Retain(_) => todo!(),
            DotExpr::Remove(_) => todo!(),
            DotExpr::Union(_) => todo!(),
            DotExpr::Intersection(_) => todo!(),
            DotExpr::Difference(_) => todo!(),
            DotExpr::Xor(_) => todo!(),
            DotExpr::Sample(_) => todo!(),
        };
        description.extend(call.description);
        Ok(Output { description, value })
    }
}

impl Expr {
    fn compile_impl(
        self,
        height: usize,
    ) -> Result<Output<<Self as Compiled>::Output>, <Self as Compiled>::Error> {
        let result = match self {
            Expr::Help(_) => return Err(()),
            Expr::Value(value) => Output {
                description: vec![(
                    format!("value {value} converted to a single-sided die thrown once"),
                    height,
                )],
                value: Configuration::singular(value),
            },
            Expr::Sides(sides) => sides.compile()?.with_added_height(height + 1),
            Expr::Call(target, call) => {
                let target = target.compile_impl(height)?;
                let call = call.compile()?.with_added_height(height);
                DotExpr::apply_to(call, target)?
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
            Expr::Parenthesis(expr) => expr.compile_impl(height)?,
        };
        Ok(result)
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
