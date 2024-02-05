use std::collections::HashMap;
use std::error::Error;

use approx::assert_relative_eq;
use banquo_core::evaluate;
use banquo_core::predicate;
use banquo_core::operators::{Always, And, Eventually, Implies, Next, Not, Or, Until};
use banquo_core::predicate::Predicate;
use banquo_core::trace::Trace;

const EPSILON: f64 = 1.0e-5;

fn get_trace() -> Trace<HashMap<&'static str, f64>> {
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
        .map(|(time, value)| (time, HashMap::from([("x", value)])))
        .collect()
}

fn p1() -> Predicate {
    predicate!{ x * -1.0 <= 2.0 }
}

fn p2() -> Predicate {
    predicate!{ x <= 2.0 }
}

type TestResult = Result<(), Box<dyn Error>>;

#[test]
fn case01() -> TestResult {
    let phi = Or::new(p1(), p2());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.0, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case02() -> TestResult {
    let phi = Eventually::unbounded(p1());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.7508, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case03() -> TestResult {
    let phi = Always::unbounded(p2());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 0.2492, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case04() -> TestResult {
    let phi = Next::new(p1());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.5881, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case05() -> TestResult {
    let phi = Next::new(Next::new(p1()));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.1068, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case06() -> TestResult {
    let phi = Next::new(Next::new(Next::new(p1())));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.4967, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case07() -> TestResult {
    let phi = Always::unbounded(Eventually::unbounded(And::new(p2(), Eventually::unbounded(p1()))));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 1.184, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case08() -> TestResult {
    let phi = Implies::new(p1(), p2());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.0, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case09() -> TestResult {
    let phi = And::new(
        Eventually::unbounded(p1()),
        Always::unbounded(Implies::new(p1(), Eventually::unbounded(p2()))),
    );
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.816, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case10() -> TestResult {
    let phi = Always::unbounded(Implies::new(p1(), Eventually::unbounded(Not::new(p1()))));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, -1.184, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case11() -> TestResult {
    let phi = Always::unbounded(Or::new(
        Not::new(p1()),
        Eventually::unbounded(Always::unbounded(Not::new(p1()))),
    ));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, -1.184, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case12() -> TestResult {
    let phi = Always::unbounded(Next::new(p1()));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, f64::NEG_INFINITY, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case13() -> TestResult {
    let phi = Eventually::unbounded(Next::new(p1()));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.7508, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case14() -> TestResult {
    let phi = Always::unbounded(Next::new(Next::new(p1())));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, f64::NEG_INFINITY, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case15() -> TestResult {
    let phi = Next::new(Eventually::unbounded(Next::new(p1())));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.7508, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case16() -> TestResult {
    let phi = Until::new(p1(), p2());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.0, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case17() -> TestResult {
    let phi = Until::new(p1(), Until::new(p2(), p1()));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.0, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case23() -> TestResult {
    let phi = Not::new(Eventually::bounded(0.0..=3.5, Not::new(And::new(p2(), p1()))));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 0.2492, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case24() -> TestResult {
    let phi = Not::new(Eventually::bounded(0.0..=1.0, Not::new(p1())));
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 2.0, epsilon = EPSILON);

    Ok(())
}

#[test]
fn case25() -> TestResult {
    let phi = Eventually::bounded(0.1..=30.0, p1());
    let trace = get_trace();
    let rho = evaluate(&trace, phi)?;

    assert_relative_eq!(rho, 3.7508, epsilon = EPSILON);

    Ok(())
}
