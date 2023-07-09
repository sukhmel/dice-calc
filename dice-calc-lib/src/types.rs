use crate::parser::expr;
use anyhow::{anyhow, bail};
use num::{BigUint, Zero};
use parse_display::{Display, FromStr};
use std::str::FromStr;
use winnow::error::Error;
use winnow::Parser;

pub type NumValue = num::Rational32;

#[derive(Clone, Debug, Display, FromStr, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    #[display("{0}")]
    Numeric(NumValue),
    #[display("\"{0}\"")]
    String(String),
}

/// A result of specific amount of dice being thrown
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Throw(Vec<Value>);

/// A [throw] with the count of outcomes that lead to it out of total outcomes
///
/// [throw]: Throw
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outcome {
    throw: Throw,
    count: BigUint,
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
        unimplemented!()
    }

    /// Get the total count of possible (not necessarily different) events in this configuration
    pub fn total(&self) -> BigUint {
        self.total_outcomes.clone()
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
    Call(Box<Expr>, Box<DotExpr>), // .X
    #[display("{1} d {0}")]
    Throw(Box<Expr>, Box<Expr>), // X d Y, throw(Y).limit(X)
    #[display("throw({0}).until({1}).limit({2})")]
    Until(Box<Expr>, Box<Filter>, Box<Expr>), // throw(X).until(Y).limit(Z)
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

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum DotExpr {
    #[display("count()")]
    Count(),
    #[display("sum()")]
    Sum(),
    #[display("product()")]
    Prod(),
    /// Leave only conforming elements
    #[display("filter({0})")]
    Filter(Filter),
    /// Deduplicate elements leaving a maximum of N elements, default is 1
    #[display("deduplicate({0})")]
    Deduplicate(Expr),
    /// Leave only N lowest rank elements, default is 1
    #[display("low({0})")]
    Low(Expr),
    /// Leave only N highest rank elements, default is 1
    #[display("high({0})")]
    High(Expr),
    /// Leave only N smallest elements, default is 1
    #[display("min({0})")]
    Min(Expr),
    /// Leave only N biggest elements, default is 1
    #[display("max({0})")]
    Max(Expr),
    #[display("retain({0})")]
    Retain(Expr),
    #[display("remove({0})")]
    Remove(Expr),
    #[display("union({0})")]
    Union(Expr),
    #[display("intersect({0})")]
    Intersection(Expr),
    #[display("diff({0})")]
    Difference(Expr),
    #[display("xor({0})")]
    Xor(Expr),
    #[display("sample({0})")]
    Sample(usize),
}

/// Todo: split into Filters that can be used for throwing until, and the rest
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Filter {
    #[display("{0}")]
    Basic(BasicFilter),

    #[display("duplicated()")]
    Duplicated(),
    #[display("duplicated({0})")]
    DuplicatedTimes(BasicFilter),
    #[display("unique()")]
    Unique(),
    #[display("unique({0})")]
    UniqueTimes(BasicFilter),
    #[display("times({0})")]
    Times(BasicFilter),

    #[display("{0}")]
    Logical(LogicFilter<Filter>),
    #[display("({0})")]
    Parenthesis(Box<Filter>),
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum LogicFilter<T: Eq + PartialEq + std::fmt::Display> {
    #[display("not {0}")]
    Not(Box<T>),
    #[display("{0} and {1}")]
    And(Box<T>, Box<T>),
    #[display("{0} or {1}")]
    Or(Box<T>, Box<T>),
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub enum LogicOperator {
    #[display("and")]
    And,
    #[display("or")]
    Or,
}

#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum BasicFilter {
    #[display("= {0}")]
    Equal(Expr),
    #[display("> {0}")]
    GreaterThan(Expr),
    #[display("< {0}")]
    LessThan(Expr),

    #[display("in {0}")]
    In(Expr),

    #[display("{0}")]
    Logical(LogicFilter<BasicFilter>),
    #[display("({0})")]
    Parenthesis(Box<BasicFilter>),
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
