use super::*;
use crate::parser::{dot_expr, sides};

#[test]
fn test_sides() {
    assert_eq!(
        sides(r#"  1 .. 3; 6,8..22   ; 7; "some other"; "new""#).map(|(i, x)| (
            i,
            format!("{:#?}", x),
            format!("{}", x)
        )),
        Ok((
            "",
            String::from(
                r#"Union(
    Union(
        Union(
            Union(
                Sequence(
                    Sequence(
                        Ratio {
                            numer: 1,
                            denom: 1,
                        },
                        Ratio {
                            numer: 3,
                            denom: 1,
                        },
                    ),
                ),
                StepSequence(
                    StepSequence {
                        first: Ratio {
                            numer: 6,
                            denom: 1,
                        },
                        second: Ratio {
                            numer: 8,
                            denom: 1,
                        },
                        last: Ratio {
                            numer: 22,
                            denom: 1,
                        },
                    },
                ),
            ),
            Value(
                Rational(
                    Ratio {
                        numer: 7,
                        denom: 1,
                    },
                ),
            ),
        ),
        Value(
            String(
                "some other",
            ),
        ),
    ),
    Value(
        String(
            "new",
        ),
    ),
)"#
            ),
            String::from(r#"1..3; 6,8..22; 7; "some other"; "new""#)
        ))
    );
}

#[test]
fn test_dot_expr() {
    assert_eq!(
        dot_expr(r#"2 . sum( )"#)
            .map(|(i, x)| {
                println!("{x:#?}");
                (i, x)
            })
            .map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Rational(
            Ratio {
                numer: 2,
                denom: 1,
            },
        ),
    ),
    Sum,
)"#
            .into(),
            "2.sum()".into()
        )),
    );

    assert_eq!(
        dot_expr(r#"2 . deduplicate(  )"#)
            .map(|(i, x)| {
                println!("{x:#?}");
                (i, x)
            })
            .map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Rational(
            Ratio {
                numer: 2,
                denom: 1,
            },
        ),
    ),
    Deduplicate(
        Value(
            Rational(
                Ratio {
                    numer: 1,
                    denom: 1,
                },
            ),
        ),
    ),
)"#
            .into(),
            "2.deduplicate(1)".into()
        )),
    );

    assert_eq!(
        dot_expr(r#"2 . deduplicate( 5 )"#)
            .map(|(i, x)| {
                println!("{x:#?}");
                (i, x)
            })
            .map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Rational(
            Ratio {
                numer: 2,
                denom: 1,
            },
        ),
    ),
    Deduplicate(
        Value(
            Rational(
                Ratio {
                    numer: 5,
                    denom: 1,
                },
            ),
        ),
    ),
)"#
            .into(),
            "2.deduplicate(5)".into()
        )),
    );
}
