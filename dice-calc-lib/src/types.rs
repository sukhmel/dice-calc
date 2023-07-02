use num::BigUint;
use parse_display::{Display, FromStr};

pub type NumValue = num::Rational32;

#[derive(Debug, Display, FromStr, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    #[display("{0}")]
    Rational(NumValue),
    #[display("\"{0}\"")]
    String(String),
}

/// A result of specific amount of dice being thrown
#[derive(Debug, PartialEq, Eq)]
pub struct Throw(Vec<Value>);

/// A [throw] with the count of outcomes that lead to it out of total outcomes
///
/// [throw]: Throw
#[derive(Debug, PartialEq, Eq)]
pub struct Outcome {
    throw: Throw,
    count: BigUint,
}

/// A total configuration of all possible [outcomes]
///
/// [outcomes]: Outcome
#[derive(Debug, PartialEq, Eq)]
pub struct Configuration {
    /// All possible events
    events: Vec<Outcome>,
    /// Count of total number of events
    total_outcomes: BigUint,
}

impl Configuration {
    /// Get the total count of possible (not necessarily different) events in this configuration
    fn total(&self) -> BigUint {
        self.total_outcomes.clone()
    }
}

#[derive(Debug, Display, Eq, PartialEq)]
#[display("{0}..{1}")]
pub struct Sequence(pub NumValue, pub NumValue);

#[derive(Debug, Display, Eq, PartialEq)]
#[display("{first},{second}..{last}")]
pub struct StepSequence {
    pub first: NumValue,
    pub second: NumValue,
    pub last: NumValue,
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Sides {
    #[display("{0}")]
    Value(Value),
    #[display("{0}")]
    Sequence(Sequence),
    #[display("{0}")]
    StepSequence(StepSequence),
    #[display("{0}; {1}")]
    Union(Box<Sides>, Box<Sides>),
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Expr {
    #[display("{0}")]
    Value(Value),
    #[display("{{{0}}}")]
    Sides(Sides),
    #[display(".{0}")]
    DotExpr(Box<DotExpr>), // .X
    #[display("{0} d {1}")]
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

#[derive(Debug, Display, Eq, PartialEq)]
pub enum DotExpr {
    #[display("{0}.filter({1})")]
    Filter(Expr, Filter),
    #[display("{0}.count()")]
    Count(Expr),
    #[display("{0}.sum()")]
    Sum(Expr),
    #[display("{0}.product()")]
    Prod(Expr),
    #[display("{0}.deduplicate({1})")]
    Deduplicate(Expr, Expr),
    #[display("{0}.low({1})")]
    Low(Expr, Expr),
    #[display("{0}.high({1})")]
    High(Expr, Expr),
    #[display("{0}.min({1})")]
    Min(Expr, Expr),
    #[display("{0}.max({1})")]
    Max(Expr, Expr),
    #[display("{0}.retain({1})")]
    Retain(Expr, Expr),
    #[display("{0}.remove({1})")]
    Remove(Expr, Expr),
    #[display("{0}.union({1})")]
    Union(Expr, Expr),
    #[display("{0}.intersect({1})")]
    Intersection(Expr, Expr),
    #[display("{0}.diff({1})")]
    Difference(Expr, Expr),
    #[display("{0}.sample({1})")]
    Sample(Expr, usize),
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Filter {
    #[display("{0}")]
    Basic(BasicFilter),

    #[display("duplicated({0})")]
    Duplicated(Expr),
    #[display("duplicated({0} {1})")]
    DuplicatedTimes(Expr, BasicFilter),
    #[display("unique({0})")]
    Unique(Expr),
    #[display("unique({0} {1})")]
    UniqueTimes(Expr, BasicFilter),
    #[display("times({0} {1})")]
    Times(Expr, BasicFilter),

    #[display("{0}")]
    Logical(LogicFilter<Filter>),
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum LogicFilter<T: Eq + PartialEq + std::fmt::Display> {
    #[display("not {0}")]
    Not(Box<T>),
    #[display("{0} and {1}")]
    And(Box<T>, Box<T>),
    #[display("{0} or {1}")]
    Or(Box<T>, Box<T>),
    #[display("({0})")]
    Parenthesis(Box<T>),
}

#[derive(Debug, Display, Eq, PartialEq)]
pub enum BasicFilter {
    #[display("= {0}")]
    Eq(Expr),
    #[display("> {0}")]
    GreaterThan(Expr),
    #[display("< {0}")]
    LessThan(Expr),

    #[display("in {0}")]
    In(Expr),

    #[display("{0}")]
    Logical(LogicFilter<BasicFilter>),
}

#[derive(Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub enum Operation {
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