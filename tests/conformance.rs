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
        (00000, 0.0000),
        (03947, 0.5881),
        (07587, 1.1068),
        (10660, 1.4967),
        (12998, 1.7169),
        (14546, 1.7508),
        (15377, 1.6075),
        (15675, 1.3204),
        (15708, 0.9412),
        (15787, 0.5313),
        (16216, 0.1525),
        (17242, -0.1431),
        (19019, -0.3207),
        (21583, -0.368),
        (24844, -0.2963),
        (28603, -0.1383),
        (32583, 0.0582),
        (36471, 0.2386),
        (39968, 0.3511),
        (42840, 0.3561),
        (44947, 0.2326),
        (46273, -0.017),
        (46925, -0.3667),
        (47114, -0.7708),
        (47128, -1.1705),
        (47280, -1.5029),
        (47861, -1.7113),
        (49095, -1.7537),
        (51104, -1.6104),
        (53886, -1.2874),
        (57317, -0.816),
    ];

    let elements = entries
        .into_iter()
        .map(|(time, var_value)| {
            let mut state = HashMap::new();
            state.insert("x".to_string(), var_value);

            (time, state)
        })
        .collect();

    Trace::new(elements)
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
    let f1 = Eventually::new(p1(), None);
    let f2 = "<> -1.0*x <= 2.0";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case03() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new(p2(), None);
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
    let f1 = Always::new(Eventually::new(And::new(p2(), Eventually::new(p1(), None)), None), None);
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
        Eventually::new(p1(), None),
        Always::new(Implies::new(p1(), Eventually::new(p2(), None)), None),
    );
    let f2 = r"(<> -1.0*x <= 2.0) /\ ([] (-1.0*x <= 2.0 -> (<> 1.0*x <= 2.0)))";

    conformance_test(&f1, f2, 2.816)
}

#[test]
fn case10() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new(Implies::new(p1(), Eventually::new(Not::new(p1()), None)), None);
    let f2 = "[] (-1.0*x <= 2.0 -> (<> (not -1.0*x <= 2.0)))";

    conformance_test(&f1, f2, -1.184)
}

#[test]
fn case11() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new(
        Or::new(Not::new(p1()), Eventually::new(Always::new(Not::new(p1()), None), None)),
        None,
    );
    let f2 = r"[] ((not -1.0*x <= 2.0) \/ (<> ([] (not -1.0*x <= 2.0))))";

    conformance_test(&f1, f2, -1.184)
}

#[test]
fn case12() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new(Next::new(p1()), None);
    let f2 = "[] (X -1.0*x <= 2.0)";

    conformance_test(&f1, f2, f64::NEG_INFINITY)
}

#[test]
fn case13() -> Result<(), Box<dyn Error>> {
    let f1 = Eventually::new(Next::new(p1()), None);
    let f2 = "<> (X -1.0*x <= 2.0)";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case14() -> Result<(), Box<dyn Error>> {
    let f1 = Always::new(Next::new(Next::new(p1())), None);
    let f2 = "[] (X (X -1.0*x <= 2.0))";

    conformance_test(&f1, f2, f64::NEG_INFINITY)
}

#[test]
fn case15() -> Result<(), Box<dyn Error>> {
    let f1 = Next::new(Eventually::new(Next::new(p1()), None));
    let f2 = "X (<> (X -1.0*x <= 2.0))";

    conformance_test(&f1, f2, 3.7508)
}

#[test]
fn case23() -> Result<(), Box<dyn Error>> {
    let f1 = Not::new(Eventually::new(Not::new(And::new(p2(), p1())), (0, 35000)));
    let f2 = r"not (<>{0,35000} (not (1.0*x <= 2.0 /\ -1.0*x <= 2.0)))";

    conformance_test(&f1, f2, 0.2492)
}

#[test]
fn case24() -> Result<(), Box<dyn Error>> {
    let f1 = Not::new(Eventually::new(Not::new(p1()), (0, 10000)));
    let f2 = "not (<>{0,10000} (not -1.0*x <= 2.0))";

    conformance_test(&f1, f2, 2.0)
}

#[test]
fn case25() -> Result<(), Box<dyn Error>> {
    let f1 = Eventually::new(p1(), (1000, 300000));
    let f2 = "<>{1000,300000} (-1.0*x <= 2.0)";

    conformance_test(&f1, f2, 3.7508)
}
