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
        " - value 2 converted to a single-sided die thrown once\n \
          - count results in each outcome of configuration"
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
        " - value 2 converted to a single-sided die thrown once\n \
          - sum of results in each outcome of configuration"
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
        " - value 2 converted to a single-sided die thrown once\n \
          - product of results in each outcome of configuration"
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
        " - all values from 2 to 5 inclusive\n \
          - count results in each outcome of configuration"
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
        " - all values from 2 to 5 inclusive\n \
          - sum of results in each outcome of configuration"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1, [3]x1, [4]x1, [5]x1}"#
    )
}

#[test]
fn test_simple_throw() {
    let expr = "2 d 5..2".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [2,3,4,5] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2, 2]x1, [2, 3]x2, [2, 4]x2, [2, 5]x2, [3, 3]x1, [3, 4]x2, [3, 5]x2, [4, 4]x1, [4, 5]x2, [5, 5]x1}"#
    )
}

#[test]
fn test_simple_throw_count() {
    let expr = "(2 d 5..2).count()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [2,3,4,5] thrown 2 times\n \
          - count results in each outcome of configuration"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1}"#
    )
}

#[test]
fn test_simple_throw_sum() {
    let expr = "(2 d 5..2).sum()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [2,3,4,5] thrown 2 times\n \
          - sum of results in each outcome of configuration"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[4]x1, [5]x2, [6]x3, [7]x4, [8]x3, [9]x2, [10]x1}"#
    )
}

#[test]
fn test_simple_throw_prod() {
    let expr = "(2 d 5..2).product()".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [2,3,4,5] thrown 2 times\n \
          - product of results in each outcome of configuration"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[4]x1, [6]x2, [8]x2, [9]x1, [10]x2, [12]x2, [15]x2, [16]x1, [20]x2, [25]x1}"#
    )
}

#[test]
fn test_simple_throw_union() {
    let expr = "(2 d 1..2).union(2 d 3..4)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - union outcomes with configuration\n   \
            - dice with sides [3,4] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{\
            [1, 1, 3, 3]x1, [1, 1, 3, 4]x2, [1, 1, 4, 4]x1, \
            [1, 2, 3, 3]x2, [1, 2, 3, 4]x4, [1, 2, 4, 4]x2, \
            [2, 2, 3, 3]x1, [2, 2, 3, 4]x2, [2, 2, 4, 4]x1\
        }"
    )
}

#[test]
fn test_simple_throw_diff() {
    let expr = "(2 d 1..2).difference(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - subtract outcomes of configuration\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{[]x1, [1]x6, [2]x2, [1, 1]x4, [1, 2]x2, [2, 2]x1}"
    )
}

#[test]
fn test_simple_throw_set_diff() {
    let expr = "(2 d 1..2).except(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - subtract outcomes of configuration as a set\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{[]x3, [1]x6, [1, 1]x4, [1, 2]x2, [2, 2]x1}"
    )
}

// [2,2].intersection([2,3]) = [2]
#[test]
fn test_simple_throw_intersect() {
    let expr = "(2 d 1..2).intersection(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - intersect outcomes with configuration\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{[]x7, [2]x8, [2, 2]x1}"
    )
}

// [2,2].meet([2,3]) = [2,2]
// TODO maybe this will be superseded by a filter?
#[test]
fn test_simple_throw_set_intersect() {
    let expr = "(2 d 1..2).meet(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - intersect outcomes with configuration as a set\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{[]x7, [2]x6, [2, 2]x3}"
    )
}

// xor with [multiset] rules, i.e. [2,2,3,4].xor([1,2,3,3]) = [1,2,4]
//
// [multiset]: https://trizenx.blogspot.com/2016/05/multisets.html
#[test]
fn test_simple_throw_xor() {
    let expr = "(2 d 1..2).xor(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - exclusive or with configuration\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{\
            []x1, [1, 2]x2, [1, 3]x4, [2, 3]x2, \
            [1, 1, 2, 2]x1, [1, 1, 2, 3]x2, \
            [1, 1, 3, 3]x1, [1, 2, 3, 3]x2, \
            [2, 2, 3, 3]x1\
        }"
    )
}


// xor with set rules, i.e. [2,2,3,4].xor([1,2,3,3]) = [1,2,3,4]
#[test]
fn test_simple_throw_set_xor() {
    let expr = "(2 d 1..2).not_eq(2 d 2..3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1,2] thrown 2 times\n \
          - exclusive or with configuration as a set\n   \
            - dice with sides [2,3] thrown 2 times"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{\
            []x1, [1]x2, [3]x2, [1, 3]x4, \
            [1, 1, 2, 2]x1, [1, 1, 2, 3]x2, \
            [1, 1, 3, 3]x1, [1, 2, 3, 3]x2, \
            [2, 2, 3, 3]x1\
        }"
    )
}