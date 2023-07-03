use crate::string_parser::parse_string;
use crate::types::{
    BasicFilter, DotExpr, Expr, Filter, LogicFilter, NumValue, Operation, Sequence, Sides,
    StepSequence, Value,
};
use std::str::FromStr;
use winnow::ascii::{digit1 as digit, multispace0 as spaces};
use winnow::combinator::cut_err;
use winnow::error::{ErrMode, Error, ErrorKind};
use winnow::{combinator, IResult, Parser};

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
        combinator::delimited(spaces, parse_string, spaces)
            .map(|value| Sides::Value(Value::String(value))),
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
    combinator::alt((
        combinator::delimited("sum(", spaces, ")").map(|_| DotExpr::Sum()),
        combinator::delimited("count(", spaces, ")").map(|_| DotExpr::Count()),
        combinator::delimited("product(", spaces, ")").map(|_| DotExpr::Prod()),
        combinator::delimited("deduplicate(", spaces, ")")
            .map(|_| DotExpr::Deduplicate(Expr::Value(Value::Rational(1.into())))),
        combinator::preceded(
            "deduplicate(",
            combinator::terminated(expr, ")").map(|expr| DotExpr::Deduplicate(expr)),
        ),
        combinator::delimited("low(", spaces, ")")
            .map(|_| DotExpr::Low(Expr::Value(Value::Rational(1.into())))),
        combinator::preceded(
            "low(",
            combinator::terminated(expr, ")").map(|expr| DotExpr::Low(expr)),
        ),
        combinator::delimited("high(", spaces, ")")
            .map(|_| DotExpr::High(Expr::Value(Value::Rational(1.into())))),
        combinator::preceded(
            "high(",
            combinator::terminated(expr, ")").map(|expr| DotExpr::High(expr)),
        ),
        combinator::delimited("min(", spaces, ")")
            .map(|_| DotExpr::Min(Expr::Value(Value::Rational(1.into())))),
        combinator::preceded(
            "min(",
            combinator::terminated(expr, ")").map(|expr| DotExpr::Min(expr)),
        ),
        combinator::delimited("max(", spaces, ")")
            .map(|_| DotExpr::Max(Expr::Value(Value::Rational(1.into())))),
        combinator::preceded(
            "max(",
            combinator::terminated(expr, ")").map(|expr| DotExpr::Max(expr)),
        ),
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
    ))
    .parse_next(i)
    .map(|(i, args)| (i, Expr::Call(Box::new(value), Box::new(args))))
}

pub fn filter(_i: &str) -> IResult<&str, Filter> {
    todo!("parse filter")
}
//
// pub fn logic_filter<T>(i: &str) -> IResult<&str, LogicFilter<T>> {
//
// }
//
// pub fn basic_filter(i: &str) -> IResult<&str, BasicFilter> {
//
// }

pub fn expr(i: &str) -> IResult<&str, Expr> {
    let (i, initial) = term(i)?;
    let (i, remainder) = combinator::repeat(
        0..,
        combinator::alt((
            |i| {
                let (i, add) = combinator::preceded("+", term).parse_next(i)?;
                Ok((i, (Operation::Add, add)))
            },
            |i| {
                let (i, sub) = combinator::preceded("-", term).parse_next(i)?;
                Ok((i, (Operation::Sub, sub)))
            },
        )),
    )
    .parse_next(i)?;

    Ok((i, fold_expressions(initial, remainder)))
}

fn term(i: &str) -> IResult<&str, Expr> {
    let (i, initial) = factor(i)?;
    let (i, remainder) = combinator::repeat(
        0..,
        combinator::alt((
            |i| {
                let (i, mul) = combinator::preceded("*", factor).parse_next(i)?;
                Ok((i, (Operation::Mul, mul)))
            },
            |i| {
                let (i, div) = combinator::preceded("/", factor).parse_next(i)?;
                Ok((i, (Operation::Div, div)))
            },
        )),
    )
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

fn fold_sides(initial: Sides, remainder: Vec<Sides>) -> Sides {
    if remainder.is_empty() {
        initial
    } else {
        remainder.into_iter().fold(initial, |acc, side| {
            Sides::Union(Box::new(acc), Box::new(side))
        })
    }
}

fn fold_expressions(initial: Expr, remainder: Vec<(Operation, Expr)>) -> Expr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (operation, expr) = pair;
        match operation {
            Operation::Add => Expr::Add(Box::new(acc), Box::new(expr)),
            Operation::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
            Operation::Div => Expr::Div(Box::new(acc), Box::new(expr)),
            Operation::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
            Operation::Throw => Expr::Throw(Box::new(acc), Box::new(expr)),
        }
    })
}

// .dot_expr
// ( )
// *
// /

// -
// +
