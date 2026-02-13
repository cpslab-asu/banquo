//! Integration test: parse strings into Banquo policies (formulas) and evaluate them.
//!
//! Run with: `cargo test -p banquo-parser --test parse_string_to_policy`

use std::collections::HashMap;

use banquo_parser::{parse_formula, parse_predicate, Formula, ParsedFormula, Trace};

fn state(x: f64, y: f64) -> HashMap<String, f64> {
    HashMap::from([("x".into(), x), ("y".into(), y)])
}

#[test]
fn parse_predicate_string() {
    // Parse a single inequality (predicate) from a string.
    let p = parse_predicate("3.1*x + 22.4*y <= 12.0").expect("parse predicate");
    let trace = Trace::from([(0.0, state(1.0, 0.0)), (1.0, state(0.0, 0.5))]);
    let result = p.evaluate(&trace).expect("evaluate");
    assert_eq!(result.len(), 2);
}

#[test]
fn parse_formula_string_and_evaluate() {
    // Parse a full temporal formula from a string and evaluate on a trace.
    let formula: ParsedFormula = parse_formula("always 3.1*x <= 0.5*y").expect("parse formula");
    let trace = Trace::from([(0.0, state(0.1, 1.0)), (1.0, state(0.2, 2.0))]);
    let metrics = formula.evaluate(&trace).expect("evaluate");
    assert_eq!(metrics.len(), 2);
}

#[test]
fn parse_formula_with_and() {
    let formula = parse_formula("3.1*x <= 0.5*y and 2.0*x <= 4.0").expect("parse");
    let trace = Trace::from([(0.0, state(1.0, 2.0))]);
    let metrics = formula.evaluate(&trace).expect("evaluate");
    assert_eq!(metrics.len(), 1);
}

#[test]
fn parse_formula_with_not() {
    let formula = parse_formula("not (3.1*x <= 0.5*y)").expect("parse");
    let trace = Trace::from([(0.0, state(1.0, 2.0))]);
    let _ = formula.evaluate(&trace).expect("evaluate");
}

#[test]
fn invalid_string_returns_error() {
    let result = parse_formula("not a valid formula !!");
    assert!(result.is_err());
}
