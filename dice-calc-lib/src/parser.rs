use crate::types::{
    BasicFilter, DotExpr, Expr, Filter, LogicFilter, LogicOperator, NumValue, Operator, Sides,
    Value,
};
use num::range;
use std::fmt::Display;
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Arc;
use winnow::ascii::{
    alphanumeric1 as alphanumeric, digit1 as digit, escaped, multispace0 as spaces,
};
use winnow::error::{ErrMode, Error, ErrorKind};
use winnow::token::one_of;
use winnow::{combinator, IResult, Parser};

pub fn string(i: &str) -> IResult<&str, &str> {
    escaped(one_of(|c| c != '"' && c != '\\'), '\\', one_of(r#"rnt"\"#)).parse_next(i)
}

pub fn sides(i: &str) -> IResult<&str, Sides> {
    let (i, initial) = side(i)?;
    let (i, remainder) = combinator::repeat(0.., combinator::preceded(";", side)).parse_next(i)?;

    Ok((i, fold_sides(initial, remainder)))
}

pub fn side(i: &str) -> IResult<&str, Sides> {
    combinator::delimited(
        spaces,
        combinator::alt((
            combinator::separated_pair(number, separator(".."), number)
                .map(|(start, stop)| Sides::Sequence(start, stop)),
            combinator::separated_pair(
                number,
                separator(","),
                combinator::separated_pair(number, separator(".."), number),
            )
            .map(|(first, (second, last))| Sides::StepSequence {
                first,
                second,
                last,
            }),
            combinator::separated_pair(value, separator("x"), digit.try_map(FromStr::from_str))
                .map(|(value, times)| Sides::RepeatedValue(value, times)),
            value.map(Sides::Value),
        )),
        spaces,
    )
    .parse_next(i)
}

pub fn value(i: &str) -> IResult<&str, Value> {
    combinator::alt((
        number.map(|value| Value::Numeric(value)),
        combinator::delimited("\"", string, "\"").map(|value| Value::String(value.to_string())),
    ))
    .parse_next(i)
}

pub fn number(i: &str) -> IResult<&str, NumValue> {
    combinator::delimited(
        spaces,
        combinator::separated_pair(combinator::alt(("-", "+", spaces)), spaces, digit),
        spaces,
    )
    .try_map(|(sign, number): (&str, &str)| FromStr::from_str(&(sign.to_string() + number)))
    .parse_next(i)
}

pub fn separator(sep: &'static str) -> impl Fn(&str) -> IResult<&str, ()> + '_ {
    move |input| {
        combinator::delimited(spaces, sep, spaces)
            .map(|_| {})
            .parse_next(input)
    }
}

pub fn throw_expr(i: &str) -> IResult<&str, Expr> {
    let (i, what) = combinator::delimited("throw(", expr, ").").parse_next(i)?;
    let (i, until) = combinator::delimited("until(", filter, ").")
        .parse_next(i)
        .map(|(i, until)| (i, Some(until)))
        .unwrap_or((i, None));
    let (i, limit) = combinator::delimited("limit(", expr, ")").parse_next(i)?;
    if let Some(until) = until {
        Ok((
            i,
            Expr::Until(Box::new(what), Box::new(until), Box::new(limit)),
        ))
    } else {
        Ok((i, Expr::Throw(Box::new(what), Box::new(limit))))
    }
}

pub fn dot_expr(i: &str) -> IResult<&str, DotExpr> {
    // If there was a dot, anything unexpected is a failure
    combinator::delimited(
        spaces,
        combinator::alt((
            combinator::delimited("sum(", spaces, ")").map(|_| DotExpr::Sum()),
            combinator::delimited("count(", spaces, ")").map(|_| DotExpr::Count()),
            combinator::delimited("product(", spaces, ")").map(|_| DotExpr::Prod()),
            combinator::delimited(
                "deduplicate(",
                combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Numeric(1.into()))))),
                ")",
            )
            .map(|expr| DotExpr::Deduplicate(expr)),
            combinator::delimited(
                "low(",
                combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Numeric(1.into()))))),
                ")",
            )
            .map(|expr| DotExpr::Low(expr)),
            combinator::delimited(
                "high(",
                combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Numeric(1.into()))))),
                ")",
            )
            .map(|expr| DotExpr::High(expr)),
            combinator::delimited(
                "min(",
                combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Numeric(1.into()))))),
                ")",
            )
            .map(|expr| DotExpr::Min(expr)),
            combinator::delimited(
                "max(",
                combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Numeric(1.into()))))),
                ")",
            )
            .map(|expr| DotExpr::Max(expr)),
            combinator::preceded(
                "retain(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Retain(expr)),
            ),
            combinator::preceded(
                "remove(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Remove(expr)),
            ),
            combinator::preceded(
                "union(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Union(expr)),
            ),
            combinator::preceded(
                "intersection(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Intersection(expr)),
            ),
            combinator::preceded(
                "difference(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Difference(expr)),
            ),
            combinator::preceded(
                "xor(",
                combinator::terminated(expr, ")").map(|expr| DotExpr::Xor(expr)),
            ),
            combinator::preceded(
                "filter(",
                combinator::terminated(filter, ")").map(|f| DotExpr::Filter(f)),
            ),
            combinator::preceded(
                "sample(",
                combinator::terminated(digit, ")")
                    .try_map(FromStr::from_str)
                    .map(|expr| DotExpr::Sample(expr)),
            ),
        )),
        spaces,
    )
    .parse_next(i)
}

pub fn filter(i: &str) -> IResult<&str, Filter> {
    generic_filter(
        generic_filter(predicate, Filter::Logical, || "and", |_| LogicOperator::And),
        Filter::Logical,
        || "or",
        |_| LogicOperator::Or,
    )
    .parse_next(i)
}

pub fn predicate(i: &str) -> IResult<&str, Filter> {
    combinator::delimited(
        spaces,
        combinator::alt((
            combinator::preceded(combinator::terminated("not", spaces), predicate)
                .map(|f| Filter::Logical(LogicFilter::Not(Box::new(f)))),
            basic_filter.map(|f| Filter::Basic(f)),
            combinator::delimited(
                "duplicated(",
                combinator::alt((
                    basic_filter.map(|f| Filter::DuplicatedTimes(f)),
                    spaces.map(|_| Filter::Duplicated()),
                )),
                ")",
            ),
            combinator::delimited(
                "unique(",
                combinator::alt((
                    basic_filter.map(|f| Filter::UniqueTimes(f)),
                    spaces.map(|_| Filter::Unique()),
                )),
                ")",
            ),
            combinator::delimited(
                "times(",
                combinator::delimited(spaces, basic_filter.map(|f| Filter::Times(f)), spaces),
                ")",
            ),
            parens(filter, Filter::Parenthesis),
        )),
        spaces,
    )
    .parse_next(i)
}

pub fn basic_filter(i: &str) -> IResult<&str, BasicFilter> {
    generic_filter(
        generic_filter(
            basic_predicate,
            BasicFilter::Logical,
            || "and",
            |_| LogicOperator::And,
        ),
        BasicFilter::Logical,
        || "or",
        |_| LogicOperator::Or,
    )
    .parse_next(i)
}

/// Parses logical expressions using parser inclusion level for separating precedence
///
/// ## Input
/// * `parser` should be either another `generic_filter` or the specific parser to target type
/// * `wrapper` is used to wrap [`LogicFilter<T>`] to get result of target type T
/// * `op` is used to generate parser that produces operators. It could be simple if there's only
///     one operator, but that may change in the future
/// * `selector` is used to map results of operator parsing to specific [`LogicOperator`]
pub fn generic_filter<'a, T, P, W, G, Op, S>(
    mut parser: P,
    wrapper: W,
    op: G,
    selector: S,
) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: Eq + PartialEq + Display,
    P: FnMut(&'a str) -> IResult<&'a str, T>,
    W: Fn(LogicFilter<T>) -> T,
    G: Fn() -> Op,
    Op: Parser<&'a str, &'a str, Error<&'a str>>,
    S: Fn(&'a str) -> LogicOperator,
{
    move |i| {
        let (i, initial) = parser.parse_next(i)?;
        let (i, remainder) = combinator::repeat(0.., |i| {
            let (i, operation) = combinator::delimited(spaces, op(), spaces).parse_next(i)?;
            let op = selector(operation);
            parser.parse_next(i).map(|(i, e)| (i, (op, e)))
        })
        .parse_next(i)?;

        Ok((i, fold_filters(initial, remainder, |input| wrapper(input))))
    }
}

pub fn basic_predicate(i: &str) -> IResult<&str, BasicFilter> {
    combinator::delimited(
        spaces,
        combinator::alt((
            combinator::preceded(combinator::terminated("not", spaces), basic_predicate)
                .map(|f| BasicFilter::Logical(LogicFilter::Not(Box::new(f)))),
            basic_check,
            parens(basic_filter, BasicFilter::Parenthesis),
        )),
        spaces,
    )
    .parse_next(i)
}

pub fn basic_check(i: &str) -> IResult<&str, BasicFilter> {
    let (i, op) = combinator::alt(("<", ">", "=", "in")).parse_next(i)?;
    let (i, expr) = expr.parse_next(i)?;
    Ok((
        i,
        match op {
            ">" => BasicFilter::GreaterThan(expr),
            "<" => BasicFilter::LessThan(expr),
            "=" => BasicFilter::Equal(expr),
            "in" => BasicFilter::In(expr),
            _ => unreachable!("combinator should only return given values"),
        },
    ))
}

pub fn expr(i: &str) -> IResult<&str, Expr> {
    let (i, help) = combinator::opt(combinator::delimited(spaces, "help", spaces)).parse_next(i)?;
    let (i, initial) = term.parse_next(i)?;
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = one_of("+-").parse_next(i)?;
        let op = match operation {
            '+' => Operator::Add,
            '-' => Operator::Sub,
            _ => unreachable!("combinator should only return given values"),
        };
        term.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    let mut expr = fold_expressions(initial, remainder);
    if help.is_some() {
        expr = Expr::Help(Box::new(expr));
    }
    Ok((i, expr))
}

fn term(i: &str) -> IResult<&str, Expr> {
    let (i, initial) = factor(i)?;
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = one_of("*/d").parse_next(i)?;
        let op = match operation {
            '*' => Operator::Mul,
            '/' => Operator::Div,
            'd' => Operator::Throw,
            _ => unreachable!("combinator should only return given values"),
        };
        factor.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    Ok((i, fold_expressions(initial, remainder)))
}

fn factor(i: &str) -> IResult<&str, Expr> {
    combinator::alt((
        combinator::separated_pair(atom, separator("."), dot_expr)
            .map(|(value, args)| Expr::Call(Box::new(value), Box::new(args))),
        // HACK: we can't distinguish between naked sides with numeric content and expression with
        //       the same content. They also should be generally interchangeable in expressions.
        side.map(|side| match side {
            Sides::Value(value) if matches!(value, Value::Numeric(_)) => Expr::Value(value),
            sides => Expr::Sides(sides),
        }),
        atom,
    ))
    .parse_next(i)
}

fn atom(i: &str) -> IResult<&str, Expr> {
    combinator::delimited(
        spaces,
        combinator::alt((
            number.map(|number| Expr::Value(Value::Numeric(number))),
            combinator::delimited("{", sides, "}").map(Expr::Sides),
            throw_expr,
            parens(expr, Expr::Parenthesis),
        )),
        spaces,
    )
    .parse_next(i)
}

fn parens<T, P, W>(mut parser: P, wrapper: W) -> impl FnMut(&str) -> IResult<&str, T>
where
    P: FnMut(&str) -> IResult<&str, T>,
    W: Fn(Box<T>) -> T,
{
    move |input| {
        let parser = |i| parser.parse_next(i);
        combinator::delimited(spaces, combinator::delimited("(", parser, ")"), spaces)
            .map(|e| wrapper(Box::new(e)))
            .parse_next(input)
    }
}

fn fold_sides(initial: Sides, remainder: Vec<Sides>) -> Sides {
    remainder.into_iter().fold(initial, |acc, side| {
        Sides::Union(Box::new(acc), Box::new(side))
    })
}

fn fold_expressions(initial: Expr, remainder: Vec<(Operator, Expr)>) -> Expr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            Operator::Add => Expr::Add(Box::new(acc), Box::new(expr)),
            Operator::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
            Operator::Div => Expr::Div(Box::new(acc), Box::new(expr)),
            Operator::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
            // Note: inverted because alternative syntax describes it the opposite way
            Operator::Throw => Expr::Throw(Box::new(expr), Box::new(acc)),
        }
    })
}

fn fold_filters<T: Eq + PartialEq + Display>(
    initial: T,
    remainder: Vec<(LogicOperator, T)>,
    wrapper: impl Fn(LogicFilter<T>) -> T,
) -> T {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            LogicOperator::And => wrapper(LogicFilter::And(Box::new(acc), Box::new(expr))),
            LogicOperator::Or => wrapper(LogicFilter::Or(Box::new(acc), Box::new(expr))),
        }
    })
}
