use super::*;
use crate::parser::sides;

#[test]
fn test_sides() {
    assert_eq!(
        sides(r#"  1 .. 3; 6,8..22   ; 7; "some other"; "new""#).map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok(("", String::from(r#"Union(
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
)"#), String::from(r#"1..3; 6,8..22; 7; "some other"; "new""#)))
    );
}
