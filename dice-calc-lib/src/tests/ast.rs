use crate::types::Expr;

#[test]
fn test_single_num_value() {
    assert_eq!(
        "1".parse::<Expr>().unwrap().help().unwrap().as_str(),
        " - value 1 converted to a single-sided die thrown once"
    )
}
