use std::collections::HashMap;
use std::error::Error;

use approx::assert_relative_eq;
use banquo::expressions::{Polynomial, Predicate};
use banquo::formula::{evaluate_robustness, Formula};
use banquo::operators::{Always, And, Eventually, Implies, Next, Not, Or};
use banquo::parser::parse_formula;
use banquo::trace::Trace;

const EPSILON: f64 = 1.0e-5;

fn get_trace() -> Trace<HashMap<String, f64>> {
    let entries = [
        (0.0000, 0.0000),
        (0.3947, 0.5881),
        (0.7587, 1.1068),
        (1.0660, 1.4967),
        (1.2998, 1.7169),
        (1.4546, 1.7508),
        (1.5377, 1.6075),
        (1.5675, 1.3204),
        (1.5708, 0.9412),
        (1.5787, 0.5313),
        (1.6216, 0.1525),
        (1.7242, -0.1431),
        (1.9019, -0.3207),
        (2.1583, -0.368),
        (2.4844, -0.2963),
        (2.8603, -0.1383),
        (3.2583, 0.0582),
        (3.6471, 0.2386),
        (3.9968, 0.3511),
        (4.2840, 0.3561),
        (4.4947, 0.2326),
        (4.6273, -0.017),
        (4.6925, -0.3667),
        (4.7114, -0.7708),
        (4.7128, -1.1705),
        (4.7280, -1.5029),
        (4.7861, -1.7113),
        (4.9095, -1.7537),
        (5.1104, -1.6104),
        (5.3886, -1.2874),
        (5.7317, -0.816),
    ];

    entries
        .into_iter()
        .map(|(time, var_value)| {
            let mut state = HashMap::new();
            state.insert("x".to_string(), var_value);

            (time, state)
        })
        .collect()
}

fn p1() -> Predicate {
    let mut coefficients = HashMap::new();
    coefficients.insert("x".to_string(), -1.0);

    let left = Polynomial::new(coefficients);
    let right = Polynomial::with_constant(HashMap::new(), 2.0);

    Predicate::new(left, right)
}

fn p2() -> Predicate {
    let mut coefficients = HashMap::new();
    coefficients.insert("x".to_string(), 1.0);

    let left = Polynomial::new(coefficients);
    let right = Polynomial::with_constant(HashMap::new(), 2.0);

    Predicate::new(left, right)
}

type TestResult<'a> = Result<(), Box<dyn Error + 'a>>;

fn conformance_test<'a, F>(f1: &'_ F, f2: &'a str, expected: f64) -> TestResult<'a>
where
    F: Formula<HashMap<String, f64>>,
    F::Error: 'a,
{
    let trace = get_trace();
    let robustness = evaluate_robustness(&f1, &trace)?;

    assert_relative_eq!(robustness, expected, epsilon = EPSILON);

    let parsed = parse_formula(f2)?;
    let robustness = evaluate_robustness(&parsed, &trace)?;

    assert_relative_eq!(robustness, expected, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case01() -> Result<(), Box<dyn Error>> {
    let f1 = Or::new(p1(), p2());
    let f2 = r"-1.0*x <= 2.0 /\ 1.0*x <= 2.0";

    conformance_test(&f1, f2, 2.0)
}

#[test]
fn case02() -> Result<(), Box<dyn Error>> {
    let f1 = Eventually::new_unbounded(p1());
    let f2 = "<> -1.0*x <= 2.0";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case03() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(p2());
    let f2 = "[] 1.0*x <= 2.0";

    conformance_test(&f1, f2, 0.2492)
}

#[test]
fn case04() -> Result<(), Box<dyn Error>> {
    let f1 = Next::new(p1());
    let f2 = "X -1.0*x <= 2.0";

    conformance_test(&f1, f2, 2.5881)
}

#[test]
fn case05() -> Result<(), Box<dyn Error>> {
    let f1 = Next::new(Next::new(p1()));
    let f2 = "X (X -1.0*x <= 2.0)";

    conformance_test(&f1, f2, 3.1068)
}

#[test]
fn case06() -> Result<(), Box<dyn Error>> {
    let f1 = Next::new(Next::new(Next::new(p1())));
    let f2 = "X (X (X -1.0*x <= 2.0))";

    conformance_test(&f1, f2, 3.4967)
}

#[test]
fn case07() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(Eventually::new_unbounded(And::new(p2(), Eventually::new_unbounded(p1()))));
    let f2 = r"[] (<> (1.0*x <= 2.0 /\ (<> -1.0*x <= 2.0)))";

    conformance_test(&f1, f2, 1.184)
}

#[test]
fn case08() -> Result<(), Box<dyn Error>> {
    let f1 = Implies::new(p1(), p2());
    let f2 = "-1.0*x <= 2.0 -> 1.0*x <= 2.0";

    conformance_test(&f1, f2, 2.0)
}

#[test]
fn case09() -> Result<(), Box<dyn Error>> {
    let f1 = And::new(
        Eventually::new_unbounded(p1()),
        Always::new_unbounded(Implies::new(p1(), Eventually::new_unbounded(p2()))),
    );
    let f2 = r"(<> -1.0*x <= 2.0) /\ ([] (-1.0*x <= 2.0 -> (<> 1.0*x <= 2.0)))";

    conformance_test(&f1, f2, 2.816)
}

#[test]
fn case10() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(Implies::new(p1(), Eventually::new_unbounded(Not::new(p1()))));
    let f2 = "[] (-1.0*x <= 2.0 -> (<> (not -1.0*x <= 2.0)))";

    conformance_test(&f1, f2, -1.184)
}

#[test]
fn case11() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(
        Or::new(Not::new(p1()), Eventually::new_unbounded(Always::new_unbounded(Not::new(p1())))),
    );
    let f2 = r"[] ((not -1.0*x <= 2.0) \/ (<> ([] (not -1.0*x <= 2.0))))";

    conformance_test(&f1, f2, -1.184)
}

#[test]
fn case12() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(Next::new(p1()));
    let f2 = "[] (X -1.0*x <= 2.0)";

    conformance_test(&f1, f2, f64::NEG_INFINITY)
}

#[test]
fn case13() -> Result<(), Box<dyn Error>> {
    let f1 = Eventually::new_unbounded(Next::new(p1()));
    let f2 = "<> (X -1.0*x <= 2.0)";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case14() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new_unbounded(Next::new(Next::new(p1())));
    let f2 = "[] (X (X -1.0*x <= 2.0))";

    conformance_test(&f1, f2, f64::NEG_INFINITY)
}

#[test]
fn case15() -> Result<(), Box<dyn Error>> {
    let f1 = Next::new(Eventually::new_unbounded(Next::new(p1())));
    let f2 = "X (<> (X -1.0*x <= 2.0))";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case23() -> Result<(), Box<dyn Error>> {
    let f1 = Not::new(Eventually::new_bounded(Not::new(And::new(p2(), p1())), (0.0, 3.5)));
    let f2 = r"not (<>{0,3.5} (not (1.0*x <= 2.0 /\ -1.0*x <= 2.0)))";

    conformance_test(&f1, f2, 0.2492)
}

#[test]
fn case24() -> Result<(), Box<dyn Error>> {
    let f1 = Not::new(Eventually::new_bounded(Not::new(p1()), (0.0, 1.0)));
    let f2 = "not (<>{0,1.0} (not -1.0*x <= 2.0))";

    conformance_test(&f1, f2, 2.0)
}

#[test]
fn case25() -> Result<(), Box<dyn Error>> {
    let f1 = Eventually::new_bounded(p1(), (0.1, 30.0));
    let f2 = "<>{0.1,30.0} (-1.0*x <= 2.0)";

    conformance_test(&f1, f2, 3.7508)
}
