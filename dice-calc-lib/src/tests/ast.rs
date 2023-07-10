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
fn test_count() {
    let expr = "2.count()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        r#" - value 2 converted to a single-sided die thrown once
 - count outcomes in the configuration"#
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[1]x1}"#
    )
}
