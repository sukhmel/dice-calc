mod division;

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::mem;
use std::str::FromStr;

use anyhow::anyhow;
use anyhow::bail;
use itertools::Itertools;
use num::rational::Ratio;
use num::BigUint;
use num::FromPrimitive;
use num::One;
use num::ToPrimitive;
use num::Zero;
use parse_display::Display;
use parse_display::FromStr;
use rand::distributions::Distribution;
use rand::distributions::WeightedIndex;
use rand::random;
use rand::Rng;
use winnow::error::Error;
use winnow::Parser;

use crate::error::CalcError;
use crate::parser::expr;
use crate::parser::step_interval;

pub type NumValue = num::Rational32;
pub type CalcResult<T> = Result<T, CalcError>;

#[derive(Clone, Debug, Display, FromStr, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    #[display("{0}")]
    Numeric(NumValue),
    #[display("\"{0}\"")]
    String(String),
}

impl TryFrom<&Value> for NumValue {
    type Error = CalcError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Numeric(value) => Ok(value.clone()),
            Value::String(str) => Err(CalcError::UnexpectedArgument {
                arg: str.clone(),
                details: "expected a numeric value".to_string(),
            }),
        }
    }
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

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::Numeric(Ratio::from_i32(value).unwrap())
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

struct ApplyOutput {
    throw: Vec<Value>,
    count: BigUint,
}

impl ApplyOutput {
    fn single(value: Value) -> Vec<Self> {
        vec![Self {
            throw: vec![value],
            count: BigUint::one(),
        }]
    }

    fn new(throw: Vec<Value>, count: BigUint) -> Self {
        Self { throw, count }
    }
}

/// A result of specific amount of dice being thrown
#[derive(Clone, Debug, PartialEq, Eq, Ord)]
pub struct Throw(Vec<Value>);
// TODO: use set?

impl PartialOrd for Throw {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.len().partial_cmp(&other.0.len()).and_then(|order| {
            if order == Ordering::Equal {
                self.0.partial_cmp(&other.0)
            } else {
                Some(order)
            }
        })
    }
}

impl Display for Throw {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0.iter().map(ToString::to_string).join(", "))
    }
}

impl Throw {
    pub fn new(mut values: Vec<Value>) -> Self {
        values.sort();
        Self(values)
    }

    fn apply(self, function: &DotExpr<Configuration>) -> CalcResult<Vec<ApplyOutput>> {
        if self.0.is_empty() {
            return match function {
                DotExpr::Xor(other) => Ok(other
                    .events
                    .iter()
                    .map(|outcome| ApplyOutput::new(outcome.throw.0.clone(), outcome.count.clone()))
                    .collect()),
                _ => Ok(vec![ApplyOutput::new(self.0, BigUint::one())]),
            };
        }
        let result = match function {
            DotExpr::Count() => ApplyOutput::single(self.0.len().into()),
            DotExpr::Sum() => {
                ApplyOutput::single(Value::Numeric(self.try_into_num_vec()?.into_iter().sum()))
            }
            DotExpr::Prod() => ApplyOutput::single(Value::Numeric(
                self.try_into_num_vec()?.into_iter().product(),
            )),
            DotExpr::Filter(_) => todo!(),
            DotExpr::Deduplicate(_) => todo!(),
            DotExpr::Low(_) => todo!(),
            DotExpr::High(_) => todo!(),
            DotExpr::Min(_) => todo!(),
            DotExpr::Max(_) => todo!(),
            DotExpr::Retain(_) => todo!(),
            DotExpr::Remove(_) => todo!(),
            DotExpr::Union(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = outcome.throw.0.clone();
                    result.extend(self.0.clone());
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Meet(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = self.0.clone();
                    result.retain(|value| outcome.throw.0.contains(value));
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Intersection(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = Vec::new();
                    let mut buffer = outcome.throw.0.clone();
                    for value in &self.0 {
                        if let Some(pos) = buffer.iter().position(|v| v == value) {
                            buffer.remove(pos);
                            result.push(value.clone());
                        }
                    }
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Except(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = self.0.clone();
                    result.retain(|value| !outcome.throw.0.contains(value));
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Difference(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = Vec::new();
                    let mut buffer = outcome.throw.0.clone();
                    for value in &self.0 {
                        if let Some(pos) = buffer.iter().position(|v| v == value) {
                            buffer.remove(pos);
                        } else {
                            result.push(value.clone());
                        }
                    }
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Xor(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = Vec::new();
                    let mut buffer = outcome.throw.0.clone();
                    for value in &self.0 {
                        if let Some(pos) = buffer.iter().position(|v| v == value) {
                            buffer.remove(pos);
                        } else {
                            result.push(value.clone());
                        }
                    }
                    result.extend(buffer);
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::NotEq(conf) => conf
                .events
                .iter()
                .map(|outcome| {
                    let mut result = Vec::new();
                    outcome.throw.0.iter().for_each(|value| {
                        if !self.0.contains(value) {
                            result.push(value.clone());
                        }
                    });
                    self.0.iter().for_each(|value| {
                        if !outcome.throw.0.contains(value) {
                            result.push(value.clone());
                        }
                    });
                    ApplyOutput::new(result, outcome.count.clone())
                })
                .collect(),
            DotExpr::Sample(_) | DotExpr::Rand(_) => unreachable!(),
        };
        Ok(result)
    }

    pub fn try_into_num_vec(self) -> Result<Vec<NumValue>, CalcError> {
        self.0
            .into_iter()
            .map(|value| {
                let Value::Numeric(num) = value else {
                    return Err(CalcError::UnexpectedArgument {
                        arg: value.to_string(),
                        details: "throw has non-numeric parts".to_string(),
                    });
                };
                Ok(num)
            })
            .collect()
    }
}

/// A [throw] with the count of outcomes that lead to it out of total outcomes
///
/// [throw]: Throw
#[derive(Clone, Debug, Display, PartialEq, Eq, Ord)]
#[display("{throw}x{count}")]
pub struct Outcome {
    throw: Throw,
    count: BigUint,
}

impl PartialOrd for Outcome {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.throw.partial_cmp(&other.throw)
    }
}

impl From<Value> for Outcome {
    fn from(value: Value) -> Self {
        Self {
            throw: Throw::new(vec![value]),
            count: BigUint::one(),
        }
    }
}

impl Outcome {
    /// This will produce vector of outcomes, really
    pub fn apply(self, function: &DotExpr<Configuration>) -> CalcResult<Vec<Self>> {
        // TODO: when every DotExpr is implemented, check if apply may take a single outcome
        Ok(self
            .throw
            .apply(function)?
            .into_iter()
            .map(|output| Self {
                throw: Throw::new(output.throw),
                count: self.count.clone() * output.count,
            })
            .collect())
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
    /// Set of sides if those were the origin. For interoperability between sides and configuration
    as_sides: Option<Vec<Value>>,
}
// TODO: implement normalization that would collapse same outcomes

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
    /// Construct and normalize the configuration
    pub fn new(events: Vec<Outcome>, total_outcomes: BigUint) -> Self {
        Self {
            events,
            total_outcomes,
            as_sides: None,
        }
        .normalize()
    }

    /// Normalize by dividing counts by the greatest common divisor and collapsing same outcomes
    pub fn normalize(self) -> Self {
        let mut events: Vec<Outcome> = self
            .events
            .into_iter()
            .fold(BTreeMap::new(), |mut map, outcome| {
                *map.entry(outcome.throw).or_insert_with(BigUint::zero) += outcome.count;
                map
            })
            .into_iter()
            .map(|(throw, count)| Outcome { throw, count })
            .collect();

        let mut divisor = self.total_outcomes.clone();
        for Outcome { count, .. } in &events {
            divisor = num::integer::gcd(divisor, count.clone())
        }
        for x in &mut events {
            x.count /= divisor.clone();
        }
        events.sort();

        let total_outcomes = self.total_outcomes / divisor;

        Configuration {
            events,
            total_outcomes,
            as_sides: self.as_sides,
        }
    }

    /// Empty configuration with no events
    pub fn empty() -> Self {
        Self {
            events: Vec::new(),
            total_outcomes: BigUint::zero(),
            as_sides: Some(Vec::new()),
        }
    }

    /// Configuration consisting from a single event
    ///
    /// whatever we do with a single-sided die, it will produce the same result
    pub fn singular(value: Value) -> Self {
        Self {
            as_sides: Some(vec![value.clone()]),
            events: vec![value.into()],
            total_outcomes: BigUint::one(),
        }
    }

    /// Configuration representing a set of sides (or a single throw of them)
    pub fn sides(mut sides: Vec<Value>) -> Self {
        sides.sort();
        Self {
            total_outcomes: BigUint::from(sides.len()),
            as_sides: Some(sides.clone()),
            events: sides.into_iter().map(From::from).collect(),
        }
        .normalize()
    }

    /// Configuration produced by throwing a specified die several times
    pub fn simple_throw(mut sides: Vec<Value>, times: usize) -> CalcResult<Self> {
        sides.sort();
        let result = {
            let mut prev = vec![vec![]];
            for _ in 0..times {
                let mut next = vec![];
                for v in prev {
                    for side in &sides {
                        let mut vec = v.clone();
                        vec.push(side.clone());
                        next.push(vec);
                    }
                }
                prev = next;
            }
            prev
        };

        let total_outcomes = BigUint::from(result.len());
        let events = result
            .into_iter()
            .map(|mut values| {
                values.sort();
                Outcome {
                    throw: Throw::new(values),
                    count: BigUint::one(),
                }
            })
            .collect();
        Ok(Configuration::new(events, total_outcomes))
    }

    /// Get the total count of possible (not necessarily different) events in this configuration
    pub fn total(&self) -> BigUint {
        self.total_outcomes.clone()
    }

    /// Apply the specified dot expression to the self
    pub fn apply(self, function: DotExpr<Configuration>) -> CalcResult<Self> {
        if let DotExpr::Sample(count) = &function {
            return self.sample(*count);
        }

        if let DotExpr::Rand(count) = &function {
            return self.rand(*count);
        }

        Ok(Configuration::new(
            self.events
                .into_iter()
                .map(|outcome| outcome.apply(&function))
                .fold_ok(Vec::new(), |mut vec, outcome| {
                    vec.extend(outcome);
                    vec
                })?,
            self.total_outcomes * function.total_change(),
        ))
    }

    /// Sample from the configuration `count` times without exhausting it (may produce duplicates)
    fn sample(self, count: usize) -> CalcResult<Self> {
        if self.total_outcomes.is_zero() {
            return Ok(Configuration::empty());
        }

        let weights = self.weights()?;
        let mut distribution = WeightedIndex::new(weights).map_err(|err| {
            CalcError::InternalError(format!("Error in weighted sampling: {err:#}"))
        })?;
        let mut result = Vec::new();
        let mut rng = rand::thread_rng();
        for _ in 0..count {
            let mut outcome = self.events[distribution.sample(&mut rng)].clone();
            outcome.count = BigUint::one();
            result.push(outcome);
        }

        return Ok(Configuration::new(result, count.into()));
    }

    /// Sample from the configuration `count` times exhausting it
    fn rand(self, count: usize) -> CalcResult<Self> {
        if self.total_outcomes.is_zero() {
            return Ok(Configuration::empty());
        }

        if self.total_outcomes <= count.into() {
            return Ok(self);
        }

        let mut weights = self.weights()?;
        let mut distribution = WeightedIndex::new(&weights).map_err(|err| {
            CalcError::InternalError(format!("Error in weighted sampling: {err:#}"))
        })?;
        let mut rng = rand::thread_rng();
        let mut result = Vec::new();
        for _ in 0..count {
            let i = distribution.sample(&mut rng);
            weights[i] -= 1;
            distribution = WeightedIndex::new(&weights).map_err(|err| {
                CalcError::InternalError(format!("Error in weighted sampling: {err:#}"))
            })?;
            let mut outcome = self.events[i].clone();
            outcome.count = BigUint::one();
            result.push(outcome);
        }

        return Ok(Configuration::new(result, count.into()));
    }

    /// Obtain distribution weights associated with events of configuration
    fn weights(&self) -> Result<Vec<u128>, CalcError> {
        let weights = self
            .events
            .iter()
            .map(|outcome| {
                outcome.count.to_u128().ok_or(CalcError::InternalError(
                    "Can't represent count as u128".into(),
                ))
            })
            .collect::<CalcResult<Vec<u128>>>()?;
        Ok(weights)
    }

    #[cfg(test)]
    pub(crate) fn events(&self) -> &Vec<Outcome> {
        &self.events
    }
}

impl TryFrom<Configuration> for Vec<Value> {
    type Error = CalcError;

    fn try_from(value: Configuration) -> Result<Self, Self::Error> {
        value
            .as_sides
            .clone()
            .ok_or_else(|| CalcError::UnexpectedArgument {
                arg: value.to_string(),
                details: "Not a set of sides".to_string(),
            })
    }
}

impl TryFrom<Configuration> for NumValue {
    type Error = CalcError;

    fn try_from(value: Configuration) -> Result<Self, CalcError> {
        if value.total_outcomes.is_one() && value.events.len() == 1 {
            if let Some(num) = value.events.iter().next().and_then(|value| {
                if value.count.is_one() {
                    if let Some(Value::Numeric(num)) = value.throw.0.iter().next() {
                        return Some(*num);
                    }
                }
                return None;
            }) {
                return Ok(num);
            }
        }
        return Err(CalcError::UnexpectedArgument {
            arg: value.to_string(),
            details: "Not a single numeric value".to_string(),
        });
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
    #[display("{0}")]
    StepSequence(StepSequence),
    #[display("{0}; {1}")]
    Union(Box<Sides>, Box<Sides>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StepSequence {
    first: NumValue,
    step: NumValue,
    last: NumValue,
    count: usize,
}

impl FromStr for StepSequence {
    type Err = CalcError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        step_interval(s)
            .map_err(|err| CalcError::UnexpectedArgument {
                arg: s.to_string(),
                details: err.to_string(),
            })
            .and_then(|(_, (begin, (second, end)))| StepSequence::new(begin, second, end))
    }
}

impl StepSequence {
    pub fn new(begin: NumValue, second: NumValue, end: NumValue) -> CalcResult<Self> {
        let (step, first, last) = if begin < end {
            (second - begin, begin, end)
        } else {
            (begin - second, end, begin)
        };
        if step.numer().is_zero() {
            return Err(CalcError::UnexpectedArgument {
                arg: format!("{begin},{second}..{end}"),
                details: format!("step must not be zero"),
            });
        }

        let step_count = (last - first) / step;

        if !step_count.denom().is_one() {
            Err(CalcError::UnexpectedArgument {
                arg: format!("{begin},{second}..{end}"),
                details: format!(
                    "interval (= {}) must contain a whole number of steps (= {step})",
                    last - first
                ),
            })
        } else if step_count.numer().is_zero() {
            Err(CalcError::UnexpectedArgument {
                arg: format!("{begin},{second}..{end}"),
                details: format!(
                    "interval (= {}) must contain at least one step",
                    last - first
                ),
            })
        } else {
            Ok(StepSequence {
                first,
                step,
                last,
                count: *step_count.numer() as usize + 1,
            })
        }
    }

    pub fn count(&self) -> usize {
        self.count
    }

    pub fn step(&self) -> NumValue {
        self.step
    }

    pub fn first(&self) -> NumValue {
        self.first
    }

    pub fn last(&self) -> NumValue {
        self.last
    }
}

impl Display for StepSequence {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{},{}..{}",
            self.first,
            self.first + self.step,
            self.last
        )
    }
}

impl TryFrom<(NumValue, (NumValue, NumValue))> for Sides {
    type Error = CalcError;

    fn try_from(values: (NumValue, (NumValue, NumValue))) -> Result<Self, Self::Error> {
        StepSequence::try_from(values).map(Self::StepSequence)
    }
}

impl TryFrom<(NumValue, (NumValue, NumValue))> for StepSequence {
    type Error = CalcError;

    fn try_from(
        (begin, (second, end)): (NumValue, (NumValue, NumValue)),
    ) -> Result<Self, Self::Error> {
        StepSequence::new(begin, second, end)
    }
}

impl From<Value> for Sides {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
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
    #[display("throw({0}).limit({1})")]
    Throw(Box<Expr>, Box<Expr>), // throw(X).limit(Y)
    #[display("{0} d {1}")]
    ThrowDie(usize, i32), // throw(1..Y).limit(X)
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
    /// Keep only a subset of elements
    #[display("retain({0})")]
    Retain(Filter<T>),
    /// Opposite of [`Retain`]
    #[display("remove({0})")]
    Remove(Filter<T>),
    #[display("union({0})")]
    Union(T),
    #[display("intersect({0})")]
    Intersection(T),
    #[display("intersect({0})")]
    /// Same as [`Intersection`], but keep all elements that are present at least once in the
    /// argument
    Meet(T),
    #[display("diff({0})")]
    Difference(T),
    /// Same as [`Difference`], but remove all elements that are present at least once in the
    /// argument
    #[display("except({0})")]
    Except(T),
    #[display("xor({0})")]
    Xor(T),
    /// Same as [`Xor`], but treat second argument as a set
    #[display("xor({0})")]
    NotEq(T),
    /// Take a random sample N times each time using the original configuration
    // TODO: sample should be top level operation like Help
    #[display("sample({0})")]
    Sample(usize),
    /// Take a random sample of N elements exhausting original configuration
    #[display("rand({0})")]
    Rand(usize),
}

impl DotExpr<Configuration> {
    pub fn total_change(&self) -> BigUint {
        match self {
            DotExpr::Count()
            | DotExpr::Sum()
            | DotExpr::Prod()
            | DotExpr::Sample(_)
            | DotExpr::Rand(_) => BigUint::one(),
            DotExpr::Retain(_) | DotExpr::Remove(_) | DotExpr::Filter(_) => todo!(),
            DotExpr::Deduplicate(conf)
            | DotExpr::Low(conf)
            | DotExpr::High(conf)
            | DotExpr::Min(conf)
            | DotExpr::Max(conf)
            | DotExpr::Union(conf)
            | DotExpr::Meet(conf)
            | DotExpr::Intersection(conf)
            | DotExpr::Except(conf)
            | DotExpr::Difference(conf)
            | DotExpr::NotEq(conf)
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
