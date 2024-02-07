use std::collections::HashMap;

use banquo_core::{Trace, evaluate, predicate};
use banquo_core::operators::{Always, And, Eventually, Or};
use banquo_hybrid_distance::{HybridDistance, HybridPredicate, HybridState};
use banquo_hybrid_distance::automaton::Automaton;

type State = HashMap<&'static str, f64>;

fn make_state(state: f64, mode: usize) -> HybridState<State, usize> {
    HybridState {
        state: HashMap::from([("x", state)]),
        label: mode,
    }
}

fn make_trace() -> Trace<HybridState<State, usize>> {
    let states = [
        (1.0,   make_state(23.0, 2)),
        (2.0,   make_state(21.0, 2)),
        (3.0,   make_state(18.0, 3)),
        (4.0,   make_state(16.0, 3)),
        (5.0,   make_state(12.0, 3)),
        (6.0,   make_state(7.0, 1)),
        (7.0,   make_state(4.0, 2)),
        (8.0,   make_state(2.0, 4)),
        (9.0,   make_state(1.0, 3)),
        (10.0,  make_state(1.0, 1)),
        (11.0,  make_state(1.0, 1)),
        (12.0,  make_state(1.0, 1)),
        (13.0,  make_state(23.0, 1)),
        (14.0,  make_state(34.0, 1)),
        (15.0,  make_state(4.0, 2)),
        (16.0,  make_state(2.0, 2)),
        (17.0,  make_state(3.0, 2)),
        (18.0,  make_state(4.0, 3)),
        (19.0,  make_state(25.0, 3)),
        (20.0,  make_state(23.0, 4)),
        (21.0,  make_state(25.0, 4)),
        (22.0,  make_state(2.0, 4)),
        (23.0,  make_state(34.0, 4)),
        (24.0,  make_state(23.0, 1)),
        (25.0,  make_state(43.0, 1)),
        (26.0,  make_state(34.0, 1)),
        (27.0,  make_state(34.0, 1)),
        (28.0,  make_state(34.0, 3)),
        (29.0,  make_state(34.0, 3)),
        (30.0,  make_state(45.0, 3)),
        (31.0,  make_state(5.0, 3)),
        (32.0,  make_state(5.0, 3)),
        (33.0,  make_state(55.0, 3)),
        (34.0,  make_state(52.0, 1)),
        (35.0,  make_state(1.0, 2)),
        (36.0,  make_state(1.0, 3)),
        (37.0,  make_state(2.0, 4)),
        (38.0,  make_state(3.0, 3)),
        (39.0,  make_state(4.0, 3)),
        (40.0,  make_state(4.0, 3)),
        (41.0,  make_state(5.0, 2)),
        (42.0,  make_state(6.0, 2)),
        (43.0,  make_state(7.0, 2)),
        (44.0,  make_state(8.0, 2)),
        (45.0,  make_state(8.0, 2)),
        (46.0,  make_state(18.0, 1)),
        (47.0,  make_state(19.0, 1)),
        (48.0,  make_state(19.0, 2)),
        (49.0,  make_state(12.0, 2)),
        (50.0,  make_state(2.0, 2)),
        (51.0,  make_state(2.0, 2)),
        (52.0,  make_state(23.0, 3)),
        (53.0,  make_state(3.0, 3)),
        (54.0,  make_state(4.0, 3)),
        (55.0,  make_state(4.0, 4)),
        (56.0,  make_state(23.0, 4)),
        (57.0,  make_state(6.0, 4)),
        (58.0,  make_state(5.0, 4)),
        (59.0,  make_state(4.0, 4)),
        (60.0,  make_state(4.0, 4)),
        (61.0,  make_state(4.0, 3)),
        (62.0,  make_state(3.0, 2)),
        (63.0,  make_state(3.0, 1)),
        (64.0,  make_state(2.0, 2)),
        (65.0,  make_state(2.0, 2)),
        (66.0,  make_state(12.0, 2)),
        (67.0,  make_state(3.0, 2)),
        (68.0,  make_state(3.0, 2)),
        (69.0,  make_state(4.0, 2)),
        (70.0,  make_state(4.0, 2)),
        (71.0,  make_state(5.0, 2)),
        (72.0,  make_state(5.0, 2)),
        (73.0,  make_state(3.0, 2)),
        (74.0,  make_state(34.0, 2)),
        (75.0,  make_state(3.0, 2)),
        (76.0,  make_state(2.0, 2)),
        (77.0,  make_state(3.0, 3)),
        (78.0,  make_state(2.0, 3)),
        (79.0,  make_state(3.0, 3)),
        (80.0,  make_state(3.0, 3)),
        (81.0,  make_state(2.0, 3)),
        (82.0,  make_state(3.0, 3)),
        (83.0,  make_state(4.0, 3)),
        (84.0,  make_state(5.0, 4)),
        (85.0,  make_state(6.0, 4)),
        (86.0,  make_state(7.0, 4)),
        (87.0,  make_state(8.0, 4)),
        (88.0,  make_state(9.0, 4)),
        (89.0,  make_state(10.0, 1)),
        (90.0,  make_state(11.0, 1)),
        (91.0,  make_state(12.0, 1)),
        (92.0,  make_state(13.0, 1)),
        (93.0,  make_state(14.0, 1)),
        (94.0,  make_state(15.0, 1)),
        (95.0,  make_state(16.0, 1)),
        (96.0,  make_state(14.0, 2)),
        (97.0,  make_state(11.0, 2)),
        (98.0,  make_state(10.0, 3)),
        (99.0,  make_state(5.0, 3)),
        (100.0, make_state(3.0, 4)),
    ];

    Trace::from(states)
}

fn make_automaton() -> Automaton<usize> {
    let edges = [
        (1, 2, predicate!{ x <= 2.0 }),
        (2, 3, predicate!{ x <= 1.0 }),
        (2, 4, predicate!{ -1.0 * x <= -3.0 }),
        (4, 3, predicate!{ -1.0 * x <= -4.0 }),
        (3, 1, predicate!{ -1.0 * x <= 0.0 }),
    ];

    Automaton::from(edges)
}

fn make_p1(automaton: &Automaton<usize>) -> HybridPredicate<usize> {
    HybridPredicate::new(predicate!{ -1.0 * x <= 0.0 }, [1], automaton)
}

fn make_p2(automaton: &Automaton<usize>) -> HybridPredicate<usize> {
    HybridPredicate::new(predicate!{ -1.0 * x <= -5.0 }, [1], automaton)
}

fn make_p3(automaton: &Automaton<usize>) -> HybridPredicate<usize> {
    HybridPredicate::new(predicate!{ 1.0 * x <= 30.0 }, [2], automaton)
}

#[test]
fn case01() {
    let automaton = make_automaton();
    let formula = Eventually::unbounded(make_p1(&automaton));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::from(52.0));
}

#[test]
fn case02() {
    let automaton = make_automaton();
    let formula = Eventually::unbounded(make_p2(&automaton));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::from(47.0))
}

#[test]
fn case03() {
    let automaton = make_automaton();
    let formula = Eventually::unbounded(And::new(make_p1(&automaton), make_p2(&automaton)));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::from(47.0))
}

#[test]
fn case04() {
    let automaton = make_automaton();
    let formula = Always::unbounded(And::new(make_p1(&automaton), make_p2(&automaton)));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::StateDistance { hops: 2, dist: -33.0 });
}

#[test]
fn case05() {
    let automaton = make_automaton();
    let formula = Eventually::unbounded(Or::new(make_p1(&automaton), make_p2(&automaton)));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::from(52.0))
}

#[test]
fn case10() {
    let automaton = make_automaton();
    let formula = And::new(And::new(make_p1(&automaton), make_p2(&automaton)), make_p3(&automaton));
    let trace = make_trace();
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::StateDistance  { hops: 2, dist: -22.0, })
}

#[test]
fn case11() {
    let trace = Trace::from_iter([
        (0.1, make_state(1.0, 1)),
        (0.2, make_state(-2.0, 3)),
    ]);

    let automaton = make_automaton();
    let formula = Always::unbounded(And::new(make_p1(&automaton), make_p2(&automaton)));
    let distance = evaluate(trace, &formula).unwrap();

    assert_eq!(distance, HybridDistance::StateDistance { hops: 1, dist: -2.0 })
}
