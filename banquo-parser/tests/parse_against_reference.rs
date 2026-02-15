//! Integration test: parsed formula strings are compared to ground-truth reference
//! formulas (built with `banquo::predicate!` and `Always::unbounded(And::new(...))`).
//! Robustness must match on the same traces, proving the parser produces correct formulas.
//!
//! Run with: `cargo test -p banquo-parser --test parse_against_reference`

use std::collections::HashMap;

use banquo::operators::{Always, And};
use banquo::{predicate, Formula as BanquoFormula};
use banquo_parser::{parse_formula, ParsedFormula, Trace};

type State = HashMap<String, f64>;

fn min_robustness(metrics: &banquo_parser::Trace<f64>) -> f64 {
    metrics
        .iter()
        .fold(f64::INFINITY, |acc, (_t, v)| if *v < acc { *v } else { acc })
}

/// Reference: Always(-π/4 <= roll and roll <= π/4)
fn reference_roll_pi4() -> impl BanquoFormula<State, Metric = f64> {
    let min_p: banquo::Predicate = predicate! { -0.78539816339 <= roll };
    let max_p: banquo::Predicate = predicate! { roll <= 0.78539816339 };
    Always::unbounded(And::new(min_p, max_p))
}

/// Reference: Always(-0.3 <= roll and roll <= 0.3)
fn reference_roll_tight() -> impl BanquoFormula<State, Metric = f64> {
    let min_p: banquo::Predicate = predicate! { -0.3 <= roll };
    let max_p: banquo::Predicate = predicate! { roll <= 0.3 };
    Always::unbounded(And::new(min_p, max_p))
}

/// Reference: Always(-0.5 <= roll_rate and roll_rate <= 0.5)
fn reference_roll_rate() -> impl BanquoFormula<State, Metric = f64> {
    let min_p: banquo::Predicate = predicate! { -0.5 <= roll_rate };
    let max_p: banquo::Predicate = predicate! { roll_rate <= 0.5 };
    Always::unbounded(And::new(min_p, max_p))
}

/// Reference: Always(3.1*x <= 0.5*y) — single predicate (no "and")
fn reference_linear_xy() -> impl BanquoFormula<State, Metric = f64> {
    Always::unbounded(predicate! { 3.1 * x <= 0.5 * y })
}

fn ref_robustness(idx: usize, t: &Trace<State>) -> f64 {
    match idx {
        0 => min_robustness(&reference_roll_pi4().evaluate(t).unwrap_or_else(|_| panic!("ref 0"))),
        1 => min_robustness(&reference_roll_tight().evaluate(t).unwrap_or_else(|_| panic!("ref 1"))),
        2 => min_robustness(&reference_roll_rate().evaluate(t).unwrap_or_else(|_| panic!("ref 2"))),
        3 => min_robustness(&reference_linear_xy().evaluate(t).unwrap_or_else(|_| panic!("ref 3"))),
        _ => panic!("ref index out of range"),
    }
}

fn state(roll: f64, pitch: f64, roll_rate: f64, lateral_accel: f64) -> State {
    HashMap::from([
        ("roll".into(), roll),
        ("pitch".into(), pitch),
        ("roll_rate".into(), roll_rate),
        ("lateral_accel".into(), lateral_accel),
    ])
}

fn xy_state(x: f64, y: f64) -> State {
    HashMap::from([("x".into(), x), ("y".into(), y)])
}

#[test]
fn parsed_formulas_match_reference_on_traces() {
    const TOL: f64 = 1e-9;

    // Each entry: (formula string, index into reference formula)
    let formulas: [(&str, usize); 4] = [
        (
            "always -0.78539816339 <= roll and roll <= 0.78539816339",
            0,
        ),
        ("always -0.3 <= roll and roll <= 0.3", 1),
        ("always -0.5 <= roll_rate and roll_rate <= 0.5", 2),
        ("always 3.1*x <= 0.5*y", 3),
    ];

    let trace_in_bounds: Trace<State> = Trace::from([
        (0.0, state(0.0, 0.0, 0.0, 0.0)),
        (1.0, state(0.2, 0.0, 0.2, 0.0)),
    ]);

    let trace_xy_ok: Trace<State> =
        Trace::from([(0.0, xy_state(0.1, 1.0)), (1.0, xy_state(0.2, 2.0))]);
    let trace_xy_violate: Trace<State> = Trace::from([(0.0, xy_state(1.0, 0.1))]);

    let trace_violations: [Trace<State>; 4] = [
        Trace::from([(0.0, state(1.0, 0.0, 0.0, 0.0))]),
        Trace::from([(0.0, state(0.5, 0.0, 0.0, 0.0))]),
        Trace::from([(0.0, state(0.0, 0.0, 1.0, 0.0))]),
        trace_xy_violate.clone(),
    ];

    for (src, ref_idx) in formulas {
        let parsed: ParsedFormula = parse_formula(src).unwrap_or_else(|e| {
            panic!("parse formula {:?}: {}", src, e);
        });

        let (trace_ok, trace_v) = if ref_idx == 3 {
            (&trace_xy_ok, &trace_xy_violate)
        } else {
            (&trace_in_bounds, &trace_violations[ref_idx])
        };

        let ref_r = ref_robustness(ref_idx, trace_ok);
        let parsed_r = min_robustness(&parsed.evaluate(trace_ok).expect("parsed eval"));
        assert!(
            (ref_r - parsed_r).abs() < TOL,
            "formula {:?}: in-bounds trace: reference={}, parsed={}",
            src,
            ref_r,
            parsed_r
        );
        assert!(
            ref_r >= 0.0,
            "formula {:?}: in-bounds should have non-negative robustness; got {}",
            src,
            ref_r
        );

        let ref_rv = ref_robustness(ref_idx, trace_v);
        let parsed_rv = min_robustness(&parsed.evaluate(trace_v).expect("parsed eval"));
        assert!(
            (ref_rv - parsed_rv).abs() < TOL,
            "formula {:?}: violating trace: reference={}, parsed={}",
            src,
            ref_rv,
            parsed_rv
        );
        assert!(
            ref_rv < 0.0,
            "formula {:?}: violating trace should have negative robustness; got {}",
            src,
            ref_rv
        );
    }
}
