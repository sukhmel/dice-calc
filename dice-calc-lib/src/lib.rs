pub type Value = num::Rational32;

/// A result of specific amount of dice being thrown
#[derive(Debug)]
pub struct Throw(Vec<Value>);

/// A [throw] with the count of outcomes that lead to it out of total outcomes
///
/// [throw]: Throw
#[derive(Debug)]
pub struct Outcome {
    throw: Throw,
    count: num::BigUint,
}

/// A total configuration of all possible [outcomes]
///
/// [outcomes]: Outcome
#[derive(Debug)]
pub struct Configuration {
    events: Vec<Outcome>,
}

impl Configuration {
    /// Get the total count of possible (not necessarily different) events in this configuration
    fn total(&self) -> num::BigUint {
        self.events.iter().sum()
    }
}

#[derive(Debug)]
pub enum Sides {
    Value(Value),
    Sequence(Value, Value),
    StepSequence(Value, Sequence),
    Combination(Box<Sides>, Box<Sides>)
}

#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Sides(Value),
    Throw(Box<Expr>, Box<Expr>),
    Until(Box<Expr>, Box<Filter>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Parenthesis(Box<Expr>),
    Filter(Box<Expr>, Box<Filter>),
    Count(Box<Expr>),
    Sum(Box<Expr>),
    Prod(Box<Expr>),
    Deduplicate(Box<Expr>, Box<Expr>),
    Low(Box<Expr>, Box<Expr>),
    High(Box<Expr>, Box<Expr>),
    Min(Box<Expr>, Box<Expr>),
    Max(Box<Expr>, Box<Expr>),
    Retain(Box<Expr>, Box<Expr>),
    Remove(Box<Expr>, Box<Expr>),
    Sample(Box<Expr>, usize),
}

#[derive(Debug)]
pub enum LogicFilter<T> {
    Not(Box<T>),
    And(Box<T>),
    Or(Box<T>),
}

#[derive(Debug)]
pub enum BasicFilter {
    Eq(Box<Expr>),
    GreaterThan(Box<Expr>),
    LessThan(Box<Expr>),

    In(Box<Expr>),

    Logical(LogicFilter<BasicFilter>),
}

#[derive(Debug)]
pub enum Filter {
    Basic(Box<BasicFilter>),

    Duplicated(Box<Expr>, Option<BasicFilter>),
    Unique(Box<Expr>, Option<BasicFilter>),
    Times(Box<Expr>, BasicFilter),

    Logical(LogicFilter<Filter>),
}

#[derive(Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}