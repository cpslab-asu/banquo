//! Integration tests: parse hybrid formula strings and evaluate on traces of hybrid states.
//!
//! Run with: `cargo test -p banquo-parser --test parse_hybrid_formula`

use std::collections::HashMap;

use banquo_core::predicate::{Predicate, Term};
use banquo_hybrid_distance::automaton::{Automaton, Guard};
use banquo_hybrid_distance::{HybridPredicate, HybridState};
use banquo_parser::{parse_hybrid_formula, Formula, ParsedHybridFormula, Trace};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Mode {
    On,
    Off,
}

fn hybrid_state(label: Mode, x: f64) -> HybridState<HashMap<String, f64>, Mode> {
    HybridState {
        label,
        state: HashMap::from([("x".into(), x)]),
    }
}

#[test]
fn parse_hybrid_formula_always_p1() {
    // Automaton: On -> Off when x >= 10, Off -> On when x <= 5
    let guard_on_off = Guard::from(Predicate::from([
        Term::Variable("x".into(), -1.0),
        Term::Constant(-10.0),
    ]));
    let guard_off_on = Guard::from(Predicate::from([Term::Variable("x".into(), 1.0), Term::Constant(5.0)]));
    let automaton = Automaton::from([(Mode::On, Mode::Off, guard_on_off), (Mode::Off, Mode::On, guard_off_on)]);

    // p1: satisfied when in mode On (no inner predicate, just "in mode On")
    let p1 = HybridPredicate::new(None, [Mode::On], &automaton);
    let predicates = HashMap::from([("p1", p1)]);

    let formula: ParsedHybridFormula<Mode> = parse_hybrid_formula("always p1", predicates).expect("parse");

    // Trace: stay in On with x=1, then x=2
    let trace = Trace::from([(0.0, hybrid_state(Mode::On, 1.0)), (1.0, hybrid_state(Mode::On, 2.0))]);
    let metrics = formula.evaluate(&trace).expect("evaluate");
    assert_eq!(metrics.len(), 2);
}

#[test]
fn parse_hybrid_formula_p1_and_p2() {
    let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
    let automaton = Automaton::from([(Mode::On, Mode::Off, guard.clone()), (Mode::Off, Mode::On, guard)]);

    let p1 = HybridPredicate::new(None, [Mode::On], &automaton);
    let p2 = HybridPredicate::new(None, [Mode::Off], &automaton);
    let predicates = HashMap::from([("p1", p1), ("p2", p2)]);

    let formula = parse_hybrid_formula("p1 and p2", predicates).expect("parse");
    // p1 and p2 can't both hold at once (different modes), but parsing should succeed
    let trace = Trace::from([(0.0, hybrid_state(Mode::On, 0.0))]);
    let metrics = formula.evaluate(&trace).expect("evaluate");
    assert_eq!(metrics.len(), 1);
}

#[test]
fn parse_hybrid_formula_missing_predicate_errors() {
    let guard = Guard::from(Predicate::from([Term::Constant(0.0)]));
    let automaton = Automaton::from([(Mode::On, Mode::Off, guard)]);
    let p1 = HybridPredicate::new(None, [Mode::On], &automaton);
    let predicates = HashMap::from([("p1", p1)]);

    let result = parse_hybrid_formula::<_, Mode>("always p99", predicates);
    assert!(result.is_err());
}
