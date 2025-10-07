from __future__ import annotations

from math import nan

import pytest

from banquo import Predicate, Trace


@pytest.fixture
def p() -> Predicate:
    # x + y < 40
    return Predicate({"x": 1.0, "y": 1.0}, 40.0)


def test_evaluation(p: Predicate):
    input = Trace({
        0.0: {"x": 0.0, "y": 1.0},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    })

    expected = Trace({
        time: 40.0 - (state["x"] + state["y"]) for time, state in input
    })

    assert p.evaluate(input) == expected


def test_nan_value(p: Predicate):
    input = Trace({
        0.0: {"x": 0.0, "y": nan},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    })

    with pytest.raises(ValueError):
        _ = p.evaluate(input)


def test_nan_coefficient():
    input = Trace({
        0.0: {"x": 0.0, "y": 1.0},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    })
    p = Predicate({"x": 1.0, "y": nan}, 40.0)

    with pytest.raises(ValueError):
        _ = p.evaluate(input)


def test_missing_variable(p: Predicate):
    input = Trace({
        0.0: {"x": 0.0},
        1.0: {"x": 7.2},
        2.0: {"x": 13.3},
        3.0: {"x": 21.4},
        4.0: {"x": 30.1},
        5.0: {"x": 38.2},
    })

    with pytest.raises(ValueError):
        _ = p.evaluate(input)
