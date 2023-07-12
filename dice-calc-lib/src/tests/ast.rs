use crate::ast::Compiled;
use crate::types::Expr;

#[test]
fn test_single_num_value() {
    let expr = "2".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - value 2 converted to a single-sided die thrown once"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1}"#
    )
}

#[test]
fn test_count_single() {
    let expr = "2.count()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - value 2 converted to a single-sided die thrown once
 - count results in each outcome of configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[1]x1}"#
    )
}

#[test]
fn test_sum_single() {
    let expr = "2.sum()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - value 2 converted to a single-sided die thrown once
 - sum of results in each outcome of configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1}"#
    )
}

#[test]
fn test_prod_single() {
    let expr = "2.product()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - value 2 converted to a single-sided die thrown once
 - product of results in each outcome of configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1}"#
    )
}


#[test]
fn test_sides() {
    let expr = "2 x 3".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - value 2 repeated 3 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1}"#
    )
}

#[test]
fn test_count() {
    let expr = "2..5.count()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - all values from 2 to 5 inclusive
 - count results in each outcome of configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[1]x1}"#
    )
}

#[test]
fn test_sum() {
    let expr = "2..5.sum()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - all values from 2 to 5 inclusive
 - sum of results in each outcome of configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1, [3]x1, [4]x1, [5]x1}"#
    )
}
