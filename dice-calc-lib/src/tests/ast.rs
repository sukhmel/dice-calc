use crate::ast::Compiled;
use crate::types::Expr;
use crate::types::NumValue;
use crate::types::Outcome;
use crate::types::Value;
use num::BigUint;
use num::ToPrimitive;
use num::Zero;

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
fn test_div_num_value() {
    let expr = "(11)/(10)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - value 11 converted to a single-sided die thrown once\n \
          - divide results of each outcome of configuration by results from\n   \
            - value 10 converted to a single-sided die thrown once"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[11/10]x1}"#
    )
}

#[test]
fn test_div() {
    let expr = "(2 d 4) / (1 d {2;4})".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - dice with sides [1..4] thrown 2 times\n \
          - divide results of each outcome of configuration by results from\n   \
            - dice with sides [2,4] thrown 1 times"
    );
    //                     2                                       4
    //        1        2        3        4          1         2         3        4
    // 1 | 1/2,1/2 | 1/2,1 | 1/2,3/2 | 1/2,2 ||  1/4,1/4 | 1/4,1/2 | 1/4,3/4 | 1/4,1 |
    // 2 |  1, 1/2 |   1,1 |  1, 3/2 |  1, 2 ||  1/2,1/4 | 1/2,1/2 | 1/2,3/4 | 1/2,1 |
    // 3 | 3/2,1/2 | 3/2,1 | 3/2,3/2 | 3/2,2 ||  3/4,1/4 | 3/4,1/2 | 3/4,3/4 | 3/4,1 |
    // 4 |  2, 1/2 |   2,1 |  2 ,3/2 |  2, 2 ||   1, 1/4 |  1, 1/2 |  1, 3/4 |  1, 1 |
    //
    //        1  2  3  4  1  2  3  4  \
    //    1   2  4  2  2  1  2  2  2   |
    //    2   .  2  2  2  .  .  2  .   } only [3/2,3/2], [2,2], [1/4,1/4], and [3/4,3/4] have a
    //    3   .  .  1  2  .  .  1  2   | single occurrence; only [1/2,1] has 4 occurrences; others
    //    4   .  .  .  1  .  .  .  .  /  have 2 occurrences.
    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        "{[1/4, 1/4]x1, \
          [1/4, 1/2]x2, [1/4, 3/4]x2, [1/4, 1]x2, [1/2, 1/2]x2, [1/2, 3/4]x2, \
          [1/2, 1]x4, \
          [1/2, 3/2]x2, [1/2, 2]x2, \
          [3/4, 3/4]x1, \
          [3/4, 1]x2, [1, 1]x2, [1, 3/2]x2, [1, 2]x2, \
          [3/2, 3/2]x1, \
          [3/2, 2]x2, [2, 2]x1\
        }"
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
fn test_sample() {
    let expr = "1..10.sample(3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 1 to 10 inclusive\n \
          - take 3 samples"
    );

    // Sample randomizes
    for _ in 0..10 {
        let result = expr.clone().compile().unwrap().value();
        assert_ne!(result.events().len(), 0);
        assert_ne!(result.total(), Zero::zero());
        match result.events().len() {
            // means we hit same value 3 times in a row
            1 => {
                assert_eq!(result.total().to_usize().unwrap(), 1);
                assert!(result.to_string().as_str().ends_with(r#"]x1}"#));
                assert!(result.to_string().as_str().starts_with(r#"{["#));
                assert!(result.events()[0] < Outcome::from(Value::from(11)));
                assert!(result.events()[0] > Outcome::from(Value::from(0)));
            }
            // means we hit same value 2 times in a row, but the third is different
            2 => {
                // this configuration can't be simplified thus total is still 3
                assert_eq!(result.total().to_usize().unwrap(), 3);
                assert!(result.to_string().as_str().contains(r#"]x1"#));
                assert!(result.to_string().as_str().contains(r#"]x2"#));
                assert!(result.events()[0] < Outcome::from(Value::from(11)));
                assert!(result.events()[0] > Outcome::from(Value::from(0)));
                assert!(result.events()[1] < Outcome::from(Value::from(11)));
                assert!(result.events()[1] > Outcome::from(Value::from(0)));
            }
            // means all the samples were different
            3 => {
                assert_eq!(result.total().to_usize().unwrap(), 3);
                assert!(result.to_string().as_str().contains(r#"]x1}"#));
                assert!(result.to_string().as_str().contains(r#"]x1, ["#));
                for i in 0..3 {
                    assert!(result.events()[i] < Outcome::from(Value::from(11)));
                    assert!(result.events()[i] > Outcome::from(Value::from(0)));
                }
            }
            _ => unreachable!("should be 1, 2, or 3"),
        }
    }
}
#[test]
fn test_sample_big_n() {
    let expr = "1.sample(3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - value 1 converted to a single-sided die thrown once\n \
          - take 3 samples"
    );
    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[1]x1}"#
    )
}

#[test]
fn test_rand() {
    let expr = "1..10.rand(3)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 1 to 10 inclusive\n \
          - take 3 random outcomes"
    );

    // Sample randomizes
    for _ in 0..10 {
        let result = expr.clone().compile().unwrap().value();
        assert_eq!(result.total().to_usize().unwrap(), 3);
        assert!(result.to_string().as_str().contains(r#"]x1}"#));
        assert!(result.to_string().as_str().contains(r#"]x1, ["#));
        for i in 0..3 {
            assert!(result.events()[i] < Outcome::from(Value::from(11)));
            assert!(result.events()[i] > Outcome::from(Value::from(0)));
        }
    }
}
#[test]
fn test_rand_big_n() {
    let expr = "0..100.rand(111)".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 0 to 100 inclusive\n \
          - take 111 random outcomes"
    );
    for attempt in 0..10 {
        let configuration = expr.clone().compile().unwrap().value();
        let events = configuration.events();
        assert_eq!(events.len(), 101, "{configuration:?}");
        for i in 0..101 {
            assert_eq!(
                events[i],
                Outcome::from(Value::from(i)),
                "{configuration:?} @ {attempt}"
            );
        }
    }
}

#[test]
fn test_seq() {
    let expr = "2..5".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 2 to 5 inclusive"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1, [3]x1, [4]x1, [5]x1}"#
    )
}

#[test]
fn test_seq_in_parens() {
    let expr = "{2..5}".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 2 to 5 inclusive"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1, [3]x1, [4]x1, [5]x1}"#
    )
}

#[test]
fn test_step_seq() {
    let expr = "{2, -2, .. -6}".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from -6 to 2 inclusive with a step of 4 (total 3)"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[-6]x1, [-2]x1, [2]x1}"#
    )
}

#[test]
fn test_naked_step_seq() {
    let expr = "2, -2 .. -6".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from -6 to 2 inclusive with a step of 4 (total 3)"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[-6]x1, [-2]x1, [2]x1}"#
    )
}

#[test]
fn test_naked_step_seq_fwd() {
    let expr = "2, 32 .. 62".parse::<Expr>().unwrap();
    assert_eq!(
        expr.clone().help().unwrap().as_str(),
        " - all values from 2 to 62 inclusive with a step of 30 (total 3)"
    );

    assert_eq!(
        expr.compile().unwrap().value().to_string().as_str(),
        r#"{[2]x1, [32]x1, [62]x1}"#
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
