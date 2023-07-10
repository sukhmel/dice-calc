use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;

use anyhow::{anyhow, bail};
use itertools::Itertools;
use num::rational::Ratio;
use num::{BigUint, FromPrimitive, Zero};
use parse_display::{Display, FromStr};
use winnow::error::Error;
use winnow::Parser;

use crate::parser::expr;

pub type NumValue = num::Rational32;

#[derive(Clone, Debug, Display, FromStr, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    #[display("{0}")]
    Numeric(NumValue),
    #[display("\"{0}\"")]
    String(String),
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Self::Numeric(Ratio::from_usize(value).unwrap())
    }
}

impl From<NumValue> for Value {
    fn from(value: NumValue) -> Self {
        Self::Numeric(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

/// A result of specific amount of dice being thrown
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Throw(Vec<Value>);

impl Display for Throw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0.iter().map(ToString::to_string).join(", "))
    }
}

impl Throw {
    pub fn apply(self, function: &DotExpr<Configuration>) -> Self {
        if self.0.is_empty() {
            // TODO: except for xor operator
            return self;
        }
        match function {
            DotExpr::Count() => Throw(vec![self.0.len().into()]),
            DotExpr::Sum() => todo!("Throw(vec![self.0.into_iter().sum()]) but maybe error"),
            DotExpr::Prod() => todo!("Throw(vec![self.0.into_iter().product()]) but maybe error"),
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
        }
    }
}

/// A [throw] with the count of outcomes that lead to it out of total outcomes
///
/// [throw]: Throw
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display("{throw}x{count}")]
pub struct Outcome {
    throw: Throw,
    count: BigUint,
}

impl Outcome {
    /// This will produce vector of outcomes, really
    pub fn apply(self, function: &DotExpr<Configuration>) -> Self {
        Self {
            throw: self.throw.apply(function),
            ..self
        }
    }
}

/// A total configuration of all possible [outcomes]
///
/// [outcomes]: Outcome
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Configuration {
    /// All possible events
    events: Vec<Outcome>,
    /// Count of total number of events
    total_outcomes: BigUint,
}

impl Display for Configuration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.events.iter().map(ToString::to_string).join(", ")
        )
    }
}

impl Configuration {
    /// Empty configuration with no events
    pub fn empty() -> Self {
        Self {
            events: Vec::new(),
            total_outcomes: BigUint::zero(),
        }
    }

    /// Configuration consisting from a single event
    ///
    /// whatever we do with a single-sided die, it will produce the same result
    pub fn singular(value: Value) -> Self {
        Self {
            events: vec![Outcome {
                throw: Throw(vec![value]),
                count: BigUint::from(1u8),
            }],
            total_outcomes: BigUint::from(1u8),
        }
    }

    /// Configuration produced by throwing a specified die several times
    pub fn simple_throw(sides: Sides, times: NumValue) -> Self {
        todo!("throw {} {} times", sides, times)
    }

    /// Get the total count of possible (not necessarily different) events in this configuration
    pub fn total(&self) -> BigUint {
        self.total_outcomes.clone()
    }

    /// Apply the specified dot expression to the self
    pub fn apply(self, function: DotExpr<Configuration>) -> Self {
        Configuration {
            total_outcomes: self.total_outcomes * function.total_change(),
            events: self
                .events
                .into_iter()
                .map(|outcome| outcome.apply(&function))
                .collect(),
        }
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Sides {
    #[display("{0}")]
    Value(Value),
    #[display("{0}x{1}")]
    RepeatedValue(Value, usize),
    #[display("{0}..{1}")]
    Sequence(NumValue, NumValue),
    #[display("{first},{second}..{last}")]
    StepSequence {
        first: NumValue,
        second: NumValue,
        last: NumValue,
    },
    #[display("{0}; {1}")]
    Union(Box<Sides>, Box<Sides>),
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Expr {
    #[display("help {0}")]
    Help(Box<Expr>),
    #[display("{0}")]
    Value(Value),
    #[display("{{{0}}}")]
    Sides(Sides),
    #[display("{0}.{1}")]
    Call(Box<Expr>, Box<DotExpr<Expr>>), // .X
    #[display("{1} d {0}")]
    Throw(Box<Expr>, Box<Expr>), // X d Y, throw(Y).limit(X)
    #[display("throw({0}).until({1}).limit({2})")]
    Until(Box<Expr>, Box<Filter<Expr>>, Box<Expr>), // throw(X).until(Y).limit(Z)
    #[display("{0} + {1}")]
    Add(Box<Expr>, Box<Expr>), // X + Y
    #[display("{0} - {1}")]
    Sub(Box<Expr>, Box<Expr>), // X - Y
    #[display("{0} * {1}")]
    Mul(Box<Expr>, Box<Expr>), // X * Y
    #[display("{0} / {1}")]
    Div(Box<Expr>, Box<Expr>), // X / Y
    #[display("({0})")]
    Parenthesis(Box<Expr>), // ( X )
}

impl FromStr for Expr {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (unparsed, result) = expr
            .parse_next(s)
            .map_err(|err| anyhow!("Error parsing: {err}"))?;
        if !unparsed.is_empty() {
            bail!("String was not fully parsed: `{}` left", unparsed)
        }
        Ok(result)
    }
}

/// This is the AST function call, but there must also be a compilation function call counterpart
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(bound(T : std::fmt::Display, Filter<T> : std::fmt::Display))]
pub enum DotExpr<T: Eq + PartialEq> {
    #[display("count()")]
    Count(),
    #[display("sum()")]
    Sum(),
    #[display("product()")]
    Prod(),
    /// Leave only conforming elements
    #[display("filter({0})")]
    Filter(Filter<T>),
    /// Deduplicate elements leaving a maximum of N elements, default is 1
    #[display("deduplicate({0})")]
    Deduplicate(T),
    /// Leave only N lowest rank elements, default is 1
    #[display("low({0})")]
    Low(T),
    /// Leave only N highest rank elements, default is 1
    #[display("high({0})")]
    High(T),
    /// Leave only N smallest elements, default is 1
    #[display("min({0})")]
    Min(T),
    /// Leave only N biggest elements, default is 1
    #[display("max({0})")]
    Max(T),
    #[display("retain({0})")]
    Retain(Filter<T>),
    #[display("remove({0})")]
    Remove(Filter<T>),
    #[display("union({0})")]
    Union(T),
    #[display("intersect({0})")]
    Intersection(T),
    #[display("diff({0})")]
    Difference(T),
    #[display("xor({0})")]
    Xor(T),
    // TODO: sample should be top level operation like Help
    #[display("sample({0})")]
    Sample(usize),
}

impl DotExpr<Configuration> {
    pub fn total_change(&self) -> BigUint {
        match self {
            DotExpr::Count() | DotExpr::Sum() | DotExpr::Prod() | DotExpr::Sample(_) => {
                BigUint::from(1u8)
            }
            DotExpr::Retain(_) | DotExpr::Remove(_) | DotExpr::Filter(_) => todo!(),
            DotExpr::Deduplicate(conf)
            | DotExpr::Low(conf)
            | DotExpr::High(conf)
            | DotExpr::Min(conf)
            | DotExpr::Max(conf)
            | DotExpr::Union(conf)
            | DotExpr::Intersection(conf)
            | DotExpr::Difference(conf)
            | DotExpr::Xor(conf) => conf.total(),
        }
    }
}

/// Todo: split into Filters that can be used for throwing until, and the rest
#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(bound(T : std::fmt::Display, BasicFilter<T> : std::fmt::Display))]
pub enum Filter<T: Eq + PartialEq> {
    #[display("{0}")]
    Basic(BasicFilter<T>),

    #[display("duplicated()")]
    Duplicated(),
    #[display("duplicated({0})")]
    DuplicatedTimes(BasicFilter<T>),
    #[display("unique()")]
    Unique(),
    #[display("unique({0})")]
    UniqueTimes(BasicFilter<T>),
    #[display("times({0})")]
    Times(BasicFilter<T>),

    #[display("{0}")]
    Logical(Box<LogicFilter<Filter<T>>>),
    #[display("({0})")]
    Parenthesis(Box<Filter<T>>),
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(bound(T : std::fmt::Display, Filter<T> : std::fmt::Display))]
pub enum LogicFilter<T: Eq + PartialEq> {
    #[display("not {0}")]
    Not(T),
    #[display("{0} and {1}")]
    And(T, T),
    #[display("{0} or {1}")]
    Or(T, T),
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub enum LogicOperator {
    #[display("and")]
    And,
    #[display("or")]
    Or,
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
#[display(bound(T : std::fmt::Display))]
pub enum BasicFilter<T: Eq + PartialEq> {
    #[display("= {0}")]
    Equal(T),
    #[display("> {0}")]
    GreaterThan(T),
    #[display("< {0}")]
    LessThan(T),
    #[display("in {0}")]
    In(T),
    #[display("{0}")]
    Logical(Box<LogicFilter<BasicFilter<T>>>),
    #[display("({0})")]
    Parenthesis(Box<BasicFilter<T>>),
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub enum Operator {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("/")]
    Div,
    #[display("*")]
    Mul,
    #[display("d")]
    Throw,
}
