use std::collections::HashMap;
use std::error::Error;

use banquo::automaton::{Automaton, Guard};
use banquo::eval_hybrid_distance;
use banquo::expressions::{HybridPredicate, HybridState, Predicate, Variables};
use banquo::formulas::Formula;
use banquo::metric::{HybridDistance, StateDistance};
use banquo::operators::{Always, And, Eventually, Or};
use banquo::parser::parse_hybrid_formula;
use banquo::trace::Trace;

type VariableMap = HashMap<&'static str, f64>;

fn make_timed_state((time, var_value, location): (f64, f64, usize)) -> (f64, HybridState<usize>) {
    let variables = Variables::from([("x", var_value)]);
    let state = HybridState { variables, location };

    (time, state)
}

fn get_trace() -> Trace<HybridState<usize>> {
    let entries = [
        (1.0, 23.0, 2),
        (2.0, 21.0, 2),
        (3.0, 18.0, 3),
        (4.0, 16.0, 3),
        (5.0, 12.0, 3),
        (6.0, 7.0, 1),
        (7.0, 4.0, 2),
        (8.0, 2.0, 4),
        (9.0, 1.0, 3),
        (10.0, 1.0, 1),
        (11.0, 1.0, 1),
        (12.0, 1.0, 1),
        (13.0, 23.0, 1),
        (14.0, 34.0, 1),
        (15.0, 4.0, 2),
        (16.0, 2.0, 2),
        (17.0, 3.0, 2),
        (18.0, 4.0, 3),
        (19.0, 25.0, 3),
        (20.0, 23.0, 4),
        (21.0, 25.0, 4),
        (22.0, 2.0, 4),
        (23.0, 34.0, 4),
        (24.0, 23.0, 1),
        (25.0, 43.0, 1),
        (26.0, 34.0, 1),
        (27.0, 34.0, 1),
        (28.0, 34.0, 3),
        (29.0, 34.0, 3),
        (30.0, 45.0, 3),
        (31.0, 5.0, 3),
        (32.0, 5.0, 3),
        (33.0, 55.0, 3),
        (34.0, 52.0, 1),
        (35.0, 1.0, 2),
        (36.0, 1.0, 3),
        (37.0, 2.0, 4),
        (38.0, 3.0, 3),
        (39.0, 4.0, 3),
        (40.0, 4.0, 3),
        (41.0, 5.0, 2),
        (42.0, 6.0, 2),
        (43.0, 7.0, 2),
        (44.0, 8.0, 2),
        (45.0, 8.0, 2),
        (46.0, 18.0, 1),
        (47.0, 19.0, 1),
        (48.0, 19.0, 2),
        (49.0, 12.0, 2),
        (50.0, 2.0, 2),
        (51.0, 2.0, 2),
        (52.0, 23.0, 3),
        (53.0, 3.0, 3),
        (54.0, 4.0, 3),
        (55.0, 4.0, 4),
        (56.0, 23.0, 4),
        (57.0, 6.0, 4),
        (58.0, 5.0, 4),
        (59.0, 4.0, 4),
        (60.0, 4.0, 4),
        (61.0, 4.0, 3),
        (62.0, 3.0, 2),
        (63.0, 3.0, 1),
        (64.0, 2.0, 2),
        (65.0, 2.0, 2),
        (66.0, 12.0, 2),
        (67.0, 3.0, 2),
        (68.0, 3.0, 2),
        (69.0, 4.0, 2),
        (70.0, 4.0, 2),
        (71.0, 5.0, 2),
        (72.0, 5.0, 2),
        (73.0, 3.0, 2),
        (74.0, 34.0, 2),
        (75.0, 3.0, 2),
        (76.0, 2.0, 2),
        (77.0, 3.0, 3),
        (78.0, 2.0, 3),
        (79.0, 3.0, 3),
        (80.0, 3.0, 3),
        (81.0, 2.0, 3),
        (82.0, 3.0, 3),
        (83.0, 4.0, 3),
        (84.0, 5.0, 4),
        (85.0, 6.0, 4),
        (86.0, 7.0, 4),
        (87.0, 8.0, 4),
        (88.0, 9.0, 4),
        (89.0, 10.0, 1),
        (90.0, 11.0, 1),
        (91.0, 12.0, 1),
        (92.0, 13.0, 1),
        (93.0, 14.0, 1),
        (94.0, 15.0, 1),
        (95.0, 16.0, 1),
        (96.0, 14.0, 2),
        (97.0, 11.0, 2),
        (98.0, 10.0, 3),
        (99.0, 5.0, 3),
        (100.0, 3.0, 4),
    ];

    entries.into_iter().map(make_timed_state).collect()
}

fn predicate_1d(coefficient: f64, bound: f64) -> Predicate {
    Predicate::simple("x", coefficient, bound)
}

fn guard_1d(coefficient: f64, bound: f64) -> Guard {
    Guard::from_iter([predicate_1d(coefficient, bound)])
}

fn get_automaton() -> Automaton<usize> {
    let guard_map = HashMap::from_iter([
        ((1, 2), guard_1d(1.0, 2.0)),
        ((2, 3), guard_1d(1.0, 1.0)),
        ((2, 4), guard_1d(-1.0, -3.0)),
        ((4, 3), guard_1d(-1.0, -4.0)),
        ((3, 1), guard_1d(-1.0, 0.0)),
    ]);

    Automaton::from(guard_map)
}

fn p1<'a>(automaton: &'a Automaton<usize>) -> HybridPredicate<'a, usize> {
    HybridPredicate::new(predicate_1d(-1.0, 0.0), 1, automaton)
}

fn p2<'a>(automaton: &'a Automaton<usize>) -> HybridPredicate<'a, usize> {
    HybridPredicate::new(predicate_1d(-1.0, -5.0), 1, automaton)
}

fn p3<'a>(automaton: &'a Automaton<usize>) -> HybridPredicate<'a, usize> {
    HybridPredicate::new(predicate_1d(1.0, 30.0), 2, automaton)
}

fn trace_test_case<'a, F>(
    f1: F,
    f2: &'a str,
    trace: Trace<HybridState<usize>>,
    expected: HybridDistance,
) -> Result<(), Box<dyn Error + 'a>>
where
    F: Formula<HybridState<usize>, Metric = HybridDistance>,
    F::Error: 'a,
{
    let distance = eval_hybrid_distance(f1, &trace)?;

    assert_eq!(distance, expected, "f1 error");

    let automaton = get_automaton();
    let predicates = HashMap::from_iter([("p1", p1(&automaton)), ("p2", p2(&automaton)), ("p3", p3(&automaton))]);
    let parsed_formula = parse_hybrid_formula(f2, predicates)?;
    let distance = eval_hybrid_distance(parsed_formula, &trace)?;

    assert_eq!(distance, expected, "f2 error");

    Ok(())
}

fn test_case<'a, F>(f1: F, f2: &'a str, expected: HybridDistance) -> Result<(), Box<dyn Error + 'a>>
where
    F: Formula<HybridState<usize>, Metric = HybridDistance>,
    F::Error: 'a,
{
    trace_test_case(f1, f2, get_trace(), expected)
}

#[test]
fn case01() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = Eventually::unbounded(p1(&automaton));
    let formula_str = "<> p1";

    test_case(formula, formula_str, HybridDistance::from(52.0))
}

#[test]
fn case02() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = Eventually::unbounded(p2(&automaton));
    let formula_str = "<> p2";

    test_case(formula, formula_str, HybridDistance::from(47.0))
}

#[test]
fn case03() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = Eventually::unbounded(And::new(p1(&automaton), p2(&automaton)));
    let formula_str = r"<> (p1 /\ p2)";

    test_case(formula, formula_str, HybridDistance::from(47.0))
}

#[test]
fn case04() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = Always::unbounded(And::new(p1(&automaton), p2(&automaton)));
    let formula_str = r"[] (p1 /\ p2)";
    let expected_distance = StateDistance {
        discrete: 2,
        continuous: -33.0,
    };

    test_case(formula, formula_str, HybridDistance::from(expected_distance))
}

#[test]
fn case05() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = Eventually::unbounded(Or::new(p1(&automaton), p2(&automaton)));
    let formula_str = r"<> (p1 \/ p2)";

    test_case(formula, formula_str, HybridDistance::from(52.0))
}

#[test]
fn case10() -> Result<(), Box<dyn Error>> {
    let automaton = get_automaton();
    let formula = And::new(And::new(p1(&automaton), p2(&automaton)), p3(&automaton));
    let formula_str = r"(p1 /\ p2) /\ p3";
    let expected_distance = StateDistance {
        discrete: 2,
        continuous: -22.0,
    };

    test_case(formula, formula_str, HybridDistance::from(expected_distance))
}

#[test]
fn case11() -> Result<(), Box<dyn Error>> {
    let s1 = HybridState {
        variables: Variables::from_iter([("x", 1.0)]),
        location: 1,
    };

    let s2 = HybridState {
        variables: Variables::from_iter([("x", -2.0)]),
        location: 3,
    };

    let trace = Trace::from_iter([(0.1, s1), (0.2, s2)]);
    let automaton = get_automaton();
    let formula = Always::unbounded(And::new(p1(&automaton), p2(&automaton)));
    let formula_str = r"[] (p1 /\ p2)";
    let expected_distance = StateDistance {
        discrete: 1,
        continuous: -2.0,
    };

    trace_test_case(formula, formula_str, trace, HybridDistance::from(expected_distance))
}
