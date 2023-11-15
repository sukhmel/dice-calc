use super::*;
use crate::parser::basic_filter;
use crate::parser::expr;
use crate::parser::filter;
use crate::parser::interval;
use crate::parser::number;
use crate::parser::sides;
use crate::parser::step_interval;
use crate::types::StepSequence;
use winnow::Parser;

#[test]
fn test_number() {
    assert_eq!(
        number(r#"1"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Ratio { numer: 1, denom: 1 }"),
            String::from("1"),
        ))
    );

    assert_eq!(
        number(r#"+1"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Ratio { numer: 1, denom: 1 }"),
            String::from("1"),
        ))
    );

    assert_eq!(
        number(r#"-1"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Ratio { numer: -1, denom: 1 }"),
            String::from("-1"),
        ))
    );

    assert_eq!(
        expr(r#"-1"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Value(Numeric(Ratio { numer: -1, denom: 1 }))"),
            String::from("-1"),
        ))
    );

    assert_eq!(
        number(r#"11/10"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Ratio { numer: 11, denom: 10 }"),
            String::from("11/10"),
        ))
    );

    assert_eq!(
        number(r#"-0.1"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Ratio { numer: -1, denom: 10 }"),
            String::from("-1/10"),
        ))
    );
}

#[test]
fn test_interval() {
    assert_eq!(
        interval(r#"1..2"#).map(|(i, (a, b))| (i, format!("{:?}", (a, b)), format!("{a}, {b}"))),
        Ok((
            "",
            String::from("(Ratio { numer: 1, denom: 1 }, Ratio { numer: 2, denom: 1 })"),
            String::from("1, 2")
        ))
    );
    assert_eq!(
        interval(r#"1,..2"#).map(|(i, (a, b))| (i, format!("{:?}", (a, b)), format!("{a}, {b}"))),
        Ok((
            "",
            String::from("(Ratio { numer: 1, denom: 1 }, Ratio { numer: 2, denom: 1 })"),
            String::from("1, 2")
        ))
    );
    assert_eq!(
        step_interval(r#"1,2..3"#).map(|(i, (a, (b, c)))| (i, format!("{:?}", (a, b, c)), format!("{a}, {b}, {c}"))),
        Ok(("", String::from("(Ratio { numer: 1, denom: 1 }, Ratio { numer: 2, denom: 1 }, Ratio { numer: 3, denom: 1 })"), String::from("1, 2, 3")))
    );
    assert_eq!(
        step_interval(r#"1,2,..3"#).map(|(i, (a, (b, c)))| (i, format!("{:?}", (a, b, c)), format!("{a}, {b}, {c}"))),
        Ok(("", String::from("(Ratio { numer: 1, denom: 1 }, Ratio { numer: 2, denom: 1 }, Ratio { numer: 3, denom: 1 })"), String::from("1, 2, 3")))
    );
    assert_eq!(
        step_interval(r#"1,2..3"#)
            .map(|(_, value)| StepSequence::try_from(value).unwrap())
            .unwrap(),
        StepSequence::new(1.into(), 2.into(), 3.into()).unwrap()
    );
    assert_eq!(
        step_interval(r#"1,2,..3"#)
            .map(|(_, value)| StepSequence::try_from(value).unwrap())
            .unwrap(),
        StepSequence::new(1.into(), 2.into(), 3.into()).unwrap()
    );
}

#[test]
fn test_naked_sides() {
    assert_eq!(
        expr(r#"1 + 2"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Value(Numeric(Ratio { numer: 1, denom: 1 })), Value(Numeric(Ratio { numer: 2, denom: 1 })))"),
            String::from("1 + 2"),
        ))
    );
    assert_eq!(
        expr(r#"1 + 2..3"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Value(Numeric(Ratio { numer: 1, denom: 1 })), Sides(Sequence(Ratio { numer: 2, denom: 1 }, Ratio { numer: 3, denom: 1 })))"),
            String::from("1 + {2..3}"),
        ))
    );
    assert_eq!(
        expr(r#"1 + 2,3,..33"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Value(Numeric(Ratio { numer: 1, denom: 1 })), Sides(StepSequence(StepSequence { first: Ratio { numer: 2, denom: 1 }, step: Ratio { numer: 1, denom: 1 }, last: Ratio { numer: 33, denom: 1 }, count: 32 })))"),
            String::from("1 + {2,3..33}"),
        ))
    );
    assert_eq!(
        expr(r#"1 + 2,3..33"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Value(Numeric(Ratio { numer: 1, denom: 1 })), Sides(StepSequence(StepSequence { first: Ratio { numer: 2, denom: 1 }, step: Ratio { numer: 1, denom: 1 }, last: Ratio { numer: 33, denom: 1 }, count: 32 })))"),
            String::from("1 + {2,3..33}"),
        ))
    );
    assert_eq!(
        expr(r#"1 + 2x3"#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Value(Numeric(Ratio { numer: 1, denom: 1 })), Sides(RepeatedValue(Numeric(Ratio { numer: 2, denom: 1 }), 3)))"),
            String::from("1 + {2x3}"),
        ))
    );
    assert_eq!(
        expr(r#""1" + "2""#).map(|(i, x)| (i, format!("{:?}", x), format!("{}", x))),
        Ok((
            "",
            String::from("Add(Sides(Value(String(\"1\"))), Sides(Value(String(\"2\"))))"),
            String::from("{\"1\"} + {\"2\"}"),
        ))
    );
}

#[test]
fn test_sides() {
    assert_eq!(
        sides(r#"  1 .. 3; 6,8..22   ; 7; "some other"; "new"; "+" x 3; 9 x 2"#).map(|(i, x)| (
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
                Union(
                    Union(
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
                        StepSequence(
                            StepSequence {
                                first: Ratio {
                                    numer: 6,
                                    denom: 1,
                                },
                                step: Ratio {
                                    numer: 2,
                                    denom: 1,
                                },
                                last: Ratio {
                                    numer: 22,
                                    denom: 1,
                                },
                                count: 9,
                            },
                        ),
                    ),
                    Value(
                        Numeric(
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
        ),
        RepeatedValue(
            String(
                "+",
            ),
            3,
        ),
    ),
    RepeatedValue(
        Numeric(
            Ratio {
                numer: 9,
                denom: 1,
            },
        ),
        2,
    ),
)"#
            ),
            String::from(r#"1..3; 6,8..22; 7; "some other"; "new"; "+"x3; 9x2"#)
        ))
    );

    assert_eq!(
        sides("\"complicated & very \\\"strange\\\" strings\"; \"like this\none\n\tfor\n\t\tinstance\"").map(|(i, x)| {
            println!("{:#?}", x);
            (
                i,
                format!("{:#?}", x),
                format!("{}", x)
            )
        }),
        Ok((
            "",
            String::from(
                r#"Union(
    Value(
        String(
            "complicated & very \\\"strange\\\" strings",
        ),
    ),
    Value(
        String(
            "like this\none\n\tfor\n\t\tinstance",
        ),
    ),
)"#
            ),
            String::from("\"complicated & very \\\"strange\\\" strings\"; \"like this\none\n\tfor\n\t\tinstance\"")
        ))
    );
}

#[test]
fn test_filter() {
    assert_eq!(
        filter(r#"  not (  duplicated( ) and unique( = 3 )) or (times( =1 ) )"#).map(|(i, x)| (
            i,
            format!("{:#?}", x),
            format!("{}", x)
        )),
        Ok((
            "",
            String::from(
                r#"Logical(
    Or(
        Logical(
            Not(
                Parenthesis(
                    Logical(
                        And(
                            Duplicated,
                            UniqueTimes(
                                Equal(
                                    Value(
                                        Numeric(
                                            Ratio {
                                                numer: 3,
                                                denom: 1,
                                            },
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        Parenthesis(
            Times(
                Equal(
                    Value(
                        Numeric(
                            Ratio {
                                numer: 1,
                                denom: 1,
                            },
                        ),
                    ),
                ),
            ),
        ),
    ),
)"#
            ),
            String::from(r#"not (duplicated() and unique(= 3)) or (times(= 1))"#)
        ))
    );
}

#[test]
fn test_filter_precedence() {
    assert_eq!(
        basic_filter(r#"not = -1 and < 2 or not < 5 or > 10 and < 20 and not = 17"#)
            .map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            String::from(
                r#"Logical(
    Or(
        Logical(
            Or(
                Logical(
                    And(
                        Logical(
                            Not(
                                Equal(
                                    Value(
                                        Numeric(
                                            Ratio {
                                                numer: -1,
                                                denom: 1,
                                            },
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        LessThan(
                            Value(
                                Numeric(
                                    Ratio {
                                        numer: 2,
                                        denom: 1,
                                    },
                                ),
                            ),
                        ),
                    ),
                ),
                Logical(
                    Not(
                        LessThan(
                            Value(
                                Numeric(
                                    Ratio {
                                        numer: 5,
                                        denom: 1,
                                    },
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        Logical(
            And(
                Logical(
                    And(
                        GreaterThan(
                            Value(
                                Numeric(
                                    Ratio {
                                        numer: 10,
                                        denom: 1,
                                    },
                                ),
                            ),
                        ),
                        LessThan(
                            Value(
                                Numeric(
                                    Ratio {
                                        numer: 20,
                                        denom: 1,
                                    },
                                ),
                            ),
                        ),
                    ),
                ),
                Logical(
                    Not(
                        Equal(
                            Value(
                                Numeric(
                                    Ratio {
                                        numer: 17,
                                        denom: 1,
                                    },
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    ),
)"#
            ),
            String::from(r#"not = -1 and < 2 or not < 5 or > 10 and < 20 and not = 17"#)
        ))
    );
}

#[test]
fn test_basic_filter() {
    assert_eq!(
        basic_filter(r#" not < 1 "#).map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            String::from(
                r#"Logical(
    Not(
        LessThan(
            Value(
                Numeric(
                    Ratio {
                        numer: 1,
                        denom: 1,
                    },
                ),
            ),
        ),
    ),
)"#
            ),
            String::from(r#"not < 1"#)
        ))
    );
}

#[test]
fn test_dot_expr() {
    assert_eq!(
        expr(r#"2 . sum( )"#).map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Numeric(
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
        expr(r#"2 . deduplicate(  )"#).map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Numeric(
            Ratio {
                numer: 2,
                denom: 1,
            },
        ),
    ),
    Deduplicate(
        Value(
            Numeric(
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
        expr(r#"2 . deduplicate( 5 )"#).map(|(i, x)| (i, format!("{:#?}", x), format!("{}", x))),
        Ok((
            "",
            r#"Call(
    Value(
        Numeric(
            Ratio {
                numer: 2,
                denom: 1,
            },
        ),
    ),
    Deduplicate(
        Value(
            Numeric(
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

#[test]
fn test_expr() {
    assert_eq!(
        expr("  { 1..4; 2,5 .. 14; 0  }").map(|(i, x)| (i, format!("{x:#?}"), format!("{x}"))),
        Ok((
            "",
            r#"Sides(
    Union(
        Union(
            Sequence(
                Ratio {
                    numer: 1,
                    denom: 1,
                },
                Ratio {
                    numer: 4,
                    denom: 1,
                },
            ),
            StepSequence(
                StepSequence {
                    first: Ratio {
                        numer: 2,
                        denom: 1,
                    },
                    step: Ratio {
                        numer: 3,
                        denom: 1,
                    },
                    last: Ratio {
                        numer: 14,
                        denom: 1,
                    },
                    count: 5,
                },
            ),
        ),
        Value(
            Numeric(
                Ratio {
                    numer: 0,
                    denom: 1,
                },
            ),
        ),
    ),
)"#
            .into(),
            "{1..4; 2,5..14; 0}".into()
        ))
    );

    assert_eq!(
        expr(
            "throw({ 1..4; 2,5 .. 14; 0  }).until( < 3 ).limit(5) + throw({1..6}).limit(2) + 5 d 9"
        )
        .map(|(i, x)| (i, format!("{x:#?}"), format!("{x}"))),
        Ok((
            "",
            r#"Add(
    Add(
        Until(
            Sides(
                Union(
                    Union(
                        Sequence(
                            Ratio {
                                numer: 1,
                                denom: 1,
                            },
                            Ratio {
                                numer: 4,
                                denom: 1,
                            },
                        ),
                        StepSequence(
                            StepSequence {
                                first: Ratio {
                                    numer: 2,
                                    denom: 1,
                                },
                                step: Ratio {
                                    numer: 3,
                                    denom: 1,
                                },
                                last: Ratio {
                                    numer: 14,
                                    denom: 1,
                                },
                                count: 5,
                            },
                        ),
                    ),
                    Value(
                        Numeric(
                            Ratio {
                                numer: 0,
                                denom: 1,
                            },
                        ),
                    ),
                ),
            ),
            Basic(
                LessThan(
                    Value(
                        Numeric(
                            Ratio {
                                numer: 3,
                                denom: 1,
                            },
                        ),
                    ),
                ),
            ),
            Value(
                Numeric(
                    Ratio {
                        numer: 5,
                        denom: 1,
                    },
                ),
            ),
        ),
        Throw(
            Sides(
                Sequence(
                    Ratio {
                        numer: 1,
                        denom: 1,
                    },
                    Ratio {
                        numer: 6,
                        denom: 1,
                    },
                ),
            ),
            Value(
                Numeric(
                    Ratio {
                        numer: 2,
                        denom: 1,
                    },
                ),
            ),
        ),
    ),
    Throw(
        Value(
            Numeric(
                Ratio {
                    numer: 9,
                    denom: 1,
                },
            ),
        ),
        Value(
            Numeric(
                Ratio {
                    numer: 5,
                    denom: 1,
                },
            ),
        ),
    ),
)"#
            .into(),
            "throw({1..4; 2,5..14; 0}).until(< 3).limit(5) + 2 d {1..6} + 5 d 9".into()
        ))
    )
}
