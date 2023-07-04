use crate::types::{
    BasicFilter, DotExpr, Expr, Filter, LogicFilter, LogicOperator, NumValue, Operator, Sequence,
    Sides, StepSequence, Value,
};
use num::range;
use std::str::FromStr;
use winnow::ascii::{
    alphanumeric1 as alphanumeric, digit1 as digit, escaped, multispace0 as spaces,
};
use winnow::error::{ErrMode, Error, ErrorKind};
use winnow::token::one_of;
use winnow::{combinator, IResult, Parser};
use crate::types::Filter::Basic;

pub fn string(i: &str) -> IResult<&str, &str> {
    escaped(alphanumeric, '\\', one_of(r#""\"#)).parse_next(i)
}

pub fn sides(i: &str) -> IResult<&str, Sides> {
    let (i, initial) = side(i)?;
    let (i, remainder) = combinator::repeat(0.., combinator::preceded(";", side)).parse_next(i)?;

    Ok((i, fold_sides(initial, remainder)))
}

pub fn side(i: &str) -> IResult<&str, Sides> {
    combinator::alt((
        combinator::separated_pair(number, separator(".."), number)
            .map(|(start, stop)| Sides::Sequence(Sequence(start, stop))),
        combinator::separated_pair(
            number,
            separator(","),
            combinator::separated_pair(number, separator(".."), number),
        )
        .map(|(first, (second, last))| {
            Sides::StepSequence(StepSequence {
                first,
                second,
                last,
            })
        }),
        combinator::delimited(spaces, number, spaces)
            .map(|value| Sides::Value(Value::Rational(value))),
        combinator::delimited(spaces, combinator::delimited("\"", string, "\""), spaces)
            .map(|value| Sides::Value(Value::String(value.to_string()))),
    ))
    .parse_next(i)
}

pub fn number(i: &str) -> IResult<&str, NumValue> {
    combinator::delimited(spaces, digit, spaces)
        .try_map(FromStr::from_str)
        .parse_next(i)
}

pub fn separator(sep: &'static str) -> impl Fn(&str) -> IResult<&str, ()> + '_ {
    move |input| {
        combinator::delimited(spaces, sep, spaces)
            .map(|_| {})
            .parse_next(input)
    }
}

pub fn dot_expr(i: &str) -> IResult<&str, Expr> {
    let (i, value) = combinator::terminated(expr, separator(".")).parse_next(i)?;
    // If there was a dot, anything unexpected is a failure
    combinator::cut_err(combinator::alt((
        combinator::delimited("sum(", spaces, ")").map(|_| DotExpr::Sum()),
        combinator::delimited("count(", spaces, ")").map(|_| DotExpr::Count()),
        combinator::delimited("product(", spaces, ")").map(|_| DotExpr::Prod()),
        combinator::delimited(
            "deduplicate(",
            combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Rational(1.into()))))),
            ")",
        )
        .map(|expr| DotExpr::Deduplicate(expr)),
        combinator::delimited(
            "low(",
            combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Rational(1.into()))))),
            ")",
        )
        .map(|expr| DotExpr::Low(expr)),
        combinator::delimited(
            "high(",
            combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Rational(1.into()))))),
            ")",
        )
        .map(|expr| DotExpr::High(expr)),
        combinator::delimited(
            "min(",
            combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Rational(1.into()))))),
            ")",
        )
        .map(|expr| DotExpr::Min(expr)),
        combinator::delimited(
            "max(",
            combinator::alt((expr, spaces.map(|_| Expr::Value(Value::Rational(1.into()))))),
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
            "filter(",
            combinator::terminated(filter, ")").map(|f| DotExpr::Filter(f)),
        ),
        combinator::preceded(
            "sample(",
            combinator::terminated(digit, ")")
                .try_map(FromStr::from_str)
                .map(|expr| DotExpr::Sample(expr)),
        ),
    )))
    .parse_next(i)
    .map(|(i, args)| (i, Expr::Call(Box::new(value), Box::new(args))))
}

pub fn filter(i: &str) -> IResult<&str, Filter> {
    let (i, initial) = predicate.parse_next(i)?;
    println!("{i}, {initial}");
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = combinator::alt((
            combinator::delimited(spaces, "and", spaces),
            combinator::delimited(spaces, "or", spaces),
        ))
        .parse_next(i)?;
        println!("{i}, {operation}");
        let op = match operation {
            "and" => LogicOperator::And,
            "or" => LogicOperator::Or,
            _ => unreachable!("combinator should only return given values"),
        };
        predicate.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    Ok((i, fold_filters(initial, remainder)))
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
            logic_parens,
        )),
        spaces,
    )
    .parse_next(i)
}

// TODO: Split onto separate levels to make `and` and `or` precedence correct
pub fn basic_filter(i: &str) -> IResult<&str, BasicFilter> {
    let (i, initial) = basic_predicate.parse_next(i)?;
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = combinator::alt((
            combinator::delimited(spaces, "and", spaces),
            combinator::delimited(spaces, "or", spaces),
        ))
        .parse_next(i)?;
        let op = match operation {
            "and" => LogicOperator::And,
            "or" => LogicOperator::Or,
            _ => unreachable!("combinator should only return given values"),
        };
        basic_predicate.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    Ok((i, fold_basic_filters(initial, remainder)))
}

pub fn basic_predicate(i: &str) -> IResult<&str, BasicFilter> {
    combinator::delimited(
        spaces,
        combinator::alt((
            combinator::preceded(combinator::terminated("not", spaces), basic_predicate)
                .map(|f| BasicFilter::Logical(LogicFilter::Not(Box::new(f)))),
            basic_check,
            basic_logic_parens,
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
    let (i, initial) = term(i)?;
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = combinator::alt(("+", "-")).parse_next(i)?;
        let op = match operation {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            _ => unreachable!("combinator should only return given values"),
        };
        term.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    Ok((i, fold_expressions(initial, remainder)))
}

fn term(i: &str) -> IResult<&str, Expr> {
    let (i, initial) = factor(i)?;
    let (i, remainder) = combinator::repeat(0.., |i| {
        let (i, operation) = combinator::alt(("*", "/")).parse_next(i)?;
        let op = match operation {
            "*" => Operator::Mul,
            "/" => Operator::Div,
            _ => unreachable!("combinator should only return given values"),
        };
        factor.parse_next(i).map(|(i, e)| (i, (op, e)))
    })
    .parse_next(i)?;

    Ok((i, fold_expressions(initial, remainder)))
}

fn factor(i: &str) -> IResult<&str, Expr> {
    combinator::alt((
        combinator::delimited(spaces, digit, spaces)
            .try_map(FromStr::from_str)
            .map(Expr::Value),
        parens,
    ))
    .parse_next(i)
}

fn parens(i: &str) -> IResult<&str, Expr> {
    combinator::delimited(
        spaces,
        combinator::delimited("(", expr.map(|e| Expr::Parenthesis(Box::new(e))), ")"),
        spaces,
    )
    .parse_next(i)
}

fn logic_parens(i: &str) -> IResult<&str, Filter> {
    combinator::delimited(
        spaces,
        combinator::delimited("(", filter.map(|e| Filter::Parenthesis(Box::new(e))), ")"),
        spaces,
    )
    .parse_next(i)
}

fn basic_logic_parens(i: &str) -> IResult<&str, BasicFilter> {
    combinator::delimited(
        spaces,
        combinator::delimited(
            "(",
            basic_filter.map(|e| BasicFilter::Parenthesis(Box::new(e))),
            ")",
        ),
        spaces,
    )
    .parse_next(i)
}

fn fold_sides(initial: Sides, remainder: Vec<Sides>) -> Sides {
    if remainder.is_empty() {
        initial
    } else {
        remainder.into_iter().fold(initial, |acc, side| {
            Sides::Union(Box::new(acc), Box::new(side))
        })
    }
}

fn fold_expressions(initial: Expr, remainder: Vec<(Operator, Expr)>) -> Expr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            Operator::Add => Expr::Add(Box::new(acc), Box::new(expr)),
            Operator::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
            Operator::Div => Expr::Div(Box::new(acc), Box::new(expr)),
            Operator::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
            Operator::Throw => Expr::Throw(Box::new(acc), Box::new(expr)),
        }
    })
}

fn fold_filters(initial: Filter, remainder: Vec<(LogicOperator, Filter)>) -> Filter {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            LogicOperator::And => Filter::Logical(LogicFilter::And(Box::new(acc), Box::new(expr))),
            LogicOperator::Or => Filter::Logical(LogicFilter::Or(Box::new(acc), Box::new(expr))),
        }
    })
}

fn fold_basic_filters(
    initial: BasicFilter,
    remainder: Vec<(LogicOperator, BasicFilter)>,
) -> BasicFilter {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            LogicOperator::And => {
                BasicFilter::Logical(LogicFilter::And(Box::new(acc), Box::new(expr)))
            }
            LogicOperator::Or => {
                BasicFilter::Logical(LogicFilter::Or(Box::new(acc), Box::new(expr)))
            }
        }
    })
}

// .dot_expr
// ( )
// *
// /

// -
// +
