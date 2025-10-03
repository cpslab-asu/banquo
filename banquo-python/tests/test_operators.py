from __future__ import annotations

from pytest import fixture
from typing_extensions import TypeAlias

from banquo import Predicate, Trace, operators

State: TypeAlias = dict[str, float]


@fixture
def trace() -> Trace[State]:
    elements: dict[float, State] = {
        0.0: {"x": 0.0, "y": 1.0},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    }

    return Trace(elements)


@fixture
def p1() -> Predicate:
    # x + y <= 40.0
    return Predicate({"x": 1.0, "y": 1.0}, 40.0)


@fixture
def p1_expected(trace: Trace[State]) -> Trace[float]:
    return Trace({
        time: 40.0 - (state["x"] + state["y"]) for time, state in trace
    })


@fixture
def p2() -> Predicate:
    # 3.0 * x - 0.5 * y <= -2.7
    return Predicate({"x": 3.0, "y": -0.5}, -2.7)


@fixture
def p2_expected(trace: Trace[State]) -> Trace[float]:
    return Trace({
        time: -2.7 - (3.0 * state["x"] - 0.5 * state["y"]) for time, state in trace
    })


def test_predicate_negation(trace: Trace[State], p1: Predicate, p1_expected: Trace[float]):
    formula = operators.Not(p1)
    expected = Trace({time: -rho for time, rho in p1_expected})
    result = formula.evaluate(trace)

    assert isinstance(result, Trace)
    assert result == expected


def test_predicate_conjuction(
    trace: Trace[State],
    p1: Predicate,
    p1_expected: Trace[float],
    p2: Predicate,
    p2_expected: Trace[float],
):
    formula = operators.And(p1, p2)
    expected = Trace({time: min(rho, p2_expected[time]) for time, rho in p1_expected})
    result = formula.evaluate(trace)

    assert isinstance(result, Trace)
    assert result == expected
