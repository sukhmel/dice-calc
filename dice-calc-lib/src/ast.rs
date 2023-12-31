use crate::error::CalcError;
use itertools::Itertools;
use num::BigUint;
use num::FromPrimitive;
use parse_display::Display;

use crate::types::BasicFilter;
use crate::types::CalcResult;
use crate::types::Configuration;
use crate::types::DotExpr;
use crate::types::Expr;
use crate::types::Filter;
use crate::types::NumValue;
use crate::types::Sides;
use crate::types::StepSequence;
use crate::types::Value;

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

    fn compile(self) -> CalcResult<Output<Self::Output>>;
}

impl Compiled for Expr {
    type Output = Configuration;

    fn compile(self) -> CalcResult<Output<Self::Output>> {
        self.compile_impl(0)
    }
}

impl Compiled for Sides {
    type Output = Configuration;

    fn compile(self) -> CalcResult<Output<Self::Output>> {
        let content = self.compile_impl(0)?;
        Ok(Output {
            value: Configuration::sides(content.value),
            description: content.description,
        })
    }
}

impl Sides {
    fn compile_impl(self, height: usize) -> CalcResult<Output<Vec<Value>>> {
        let mut result = vec![];
        let description = match self {
            Sides::Value(value) => {
                let descr = format!("single value {value}");
                result.push(value);
                vec![(descr, height)]
            }
            Sides::RepeatedValue(value, times) => {
                for _ in 0..times {
                    result.push(value.clone())
                }
                vec![(format!("value {value} repeated {times} times"), height)]
            }
            Sides::Sequence(start, stop) => {
                if *start.denom() != 1 || *stop.denom() != 1 {
                    return Err(CalcError::UnexpectedArgument {
                        arg: format!("{start}..{stop}"),
                        details: "simple sequence can only be produced for whole numbers"
                            .to_string(),
                    });
                }
                let begin = i32::min(*start.numer(), *stop.numer());
                let end = i32::max(*start.numer(), *stop.numer());
                for index in begin..end + 1 {
                    result.push(index.into());
                }
                vec![(
                    format!("all values from {begin} to {end} inclusive"),
                    height,
                )]
            }
            Sides::StepSequence(seq) => {
                let mut value = seq.first();
                for _ in 1..seq.count() {
                    result.push(value.clone().into());
                    value += seq.step();
                }
                result.push(value.into());
                vec![(
                    format!(
                        "all values from {} to {} inclusive with a step of {} (total {})",
                        seq.first(),
                        seq.last(),
                        seq.step(),
                        seq.count()
                    ),
                    height,
                )]
            }
            Sides::Union(left, right) => {
                let first = left.compile_impl(height + 1)?;
                let last = right.compile_impl(height + 1)?;
                result.extend(first.value);
                result.extend(last.value);
                let mut description = first.description;
                description.extend(last.description);
                description
            }
        };
        Ok(Output {
            value: result,
            description,
        })
    }
}

impl Compiled for Filter<Expr> {
    type Output = Filter<Configuration>;

    fn compile(self) -> CalcResult<Output<Self::Output>> {
        todo!()
    }
}

impl Compiled for BasicFilter<Expr> {
    type Output = BasicFilter<Configuration>;

    fn compile(self) -> CalcResult<Output<Self::Output>> {
        todo!()
    }
}

impl Compiled for DotExpr<Expr> {
    type Output = DotExpr<Configuration>;

    fn compile(self) -> CalcResult<Output<Self::Output>> {
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
            DotExpr::Union(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("union outcomes with configuration".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Union(expr_output.value))
            }
            DotExpr::Meet(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description =
                    vec![("intersect outcomes with configuration as a set".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Meet(expr_output.value))
            }
            DotExpr::Intersection(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("intersect outcomes with configuration".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Intersection(expr_output.value))
            }
            DotExpr::Difference(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("subtract outcomes of configuration".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Difference(expr_output.value))
            }
            DotExpr::Except(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description =
                    vec![("subtract outcomes of configuration as a set".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Except(expr_output.value))
            }
            DotExpr::NotEq(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("exclusive or with configuration as a set".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::NotEq(expr_output.value))
            }
            DotExpr::Xor(expr) => {
                let expr_output = expr.compile()?.with_added_height(1);
                let mut description = vec![("exclusive or with configuration".into(), 0)];
                description.extend(expr_output.description);
                (description, DotExpr::Xor(expr_output.value))
            }
            DotExpr::Sample(count) => (
                vec![(format!("take {count} samples"), 0)],
                DotExpr::Sample(count),
            ),
            DotExpr::Rand(count) => (
                vec![(format!("take {count} random outcomes"), 0)],
                DotExpr::Rand(count),
            ),
            DotExpr::Count() => (
                vec![("count results in each outcome of configuration".into(), 0)],
                DotExpr::Count(),
            ),
            DotExpr::Sum() => (
                vec![("sum of results in each outcome of configuration".into(), 0)],
                DotExpr::Sum(),
            ),
            DotExpr::Prod() => (
                vec![(
                    "product of results in each outcome of configuration".into(),
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
    ) -> CalcResult<Output<Configuration>> {
        let mut description = conf.description;
        let value = conf.value.apply(call.value)?;
        description.extend(call.description);
        Ok(Output { description, value })
    }
}

impl Expr {
    fn compile_impl(self, height: usize) -> CalcResult<Output<<Self as Compiled>::Output>> {
        let result = match self {
            Expr::Help(_) => return Err(CalcError::HelpMustBeCalled),
            Expr::Value(value) => Output {
                description: vec![(
                    format!("value {value} converted to a single-sided die thrown once"),
                    height,
                )],
                value: Configuration::singular(value),
            },
            Expr::Sides(sides) => sides.compile()?.with_added_height(height),
            Expr::Call(target, call) => {
                let target = target.compile_impl(height)?;
                let call = call.compile()?.with_added_height(height);
                DotExpr::apply_to(call, target)?
            }
            Expr::Throw(what, times) => {
                let what = what.compile()?;
                let times = times.compile()?;
                if let Ok(sides) = <Configuration as TryInto<Vec<Value>>>::try_into(what.value) {
                    if let Ok(times) = <Configuration as TryInto<NumValue>>::try_into(times.value) {
                        if *times.denom() != 1 || *times.numer() < 1 {
                            return Err(CalcError::UnexpectedArgument {
                                arg: times.to_string(),
                                details: "can only perform throws with positive whole numbers"
                                    .to_string(),
                            });
                        }
                        let times = *times.numer() as usize;
                        return Ok(Output {
                            description: vec![(
                                format!(
                                    "dice with sides [{}] thrown {times} times",
                                    sides.iter().map(ToString::to_string).join(","),
                                ),
                                height,
                            )],
                            value: Configuration::simple_throw(sides, times)?,
                        });
                    }
                }
                todo!()
            }
            Expr::ThrowDie(times, what) => {
                return Ok(Output {
                    description: vec![(
                        format!("dice with sides [1..{what}] thrown {times} times"),
                        height,
                    )],
                    value: Configuration::simple_throw(
                        (1..what + 1).map(Value::from).collect(),
                        times,
                    )?,
                });
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
            Expr::Div(left, right) => {
                let left = left.compile_impl(height)?;
                let right = right.compile_impl(height + 1)?;
                let mut description = left.description;
                description.push((
                    format!("divide results of each outcome of configuration by results from"),
                    height,
                ));
                description.extend(right.description);
                (left.value / right.value).map(|value| Output { description, value })?
            }
            Expr::Parenthesis(expr) => expr.compile_impl(height)?,
        };
        Ok(result)
    }

    pub fn help(self) -> CalcResult<String> {
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
