from __future__ import annotations

from typing import Final

import pytest
import typing_extensions

import banquo
import banquo.operators as ops

pytestmark = pytest.mark.conformance

EPSILON: Final[float] = 1.0e-5
State: typing_extensions.TypeAlias = dict[str, float]
Trace: typing_extensions.TypeAlias = banquo.Trace[State]
Predicate: typing_extensions.TypeAlias = banquo.Predicate


@pytest.fixture
def trace() -> Trace:
    entries = [
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
    ]

    return banquo.Trace({time: {"x": value} for time, value in entries})


@pytest.fixture
def p1() -> Predicate:
    return banquo.Predicate({"x": -1.0}, 2.0)


@pytest.fixture
def p2() -> Predicate:
    return banquo.Predicate({"x": 1.0}, 2.0)


def test_case01(trace: Trace, p1: Predicate, p2: Predicate):
    formula = ops.Or(p1, p2)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(2.0, abs=EPSILON)


def test_case02(trace: Trace, p1: Predicate):
    formula = ops.Eventually(p1)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.7508, abs=EPSILON)


def test_case03(trace: Trace, p2: Predicate):
    formula = ops.Always(p2)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(0.2492, abs=EPSILON)


def test_case04(trace: Trace, p1: Predicate):
    formula = ops.Next(p1)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(2.5881, abs=EPSILON)


def test_case05(trace: Trace, p1: Predicate):
    formula = ops.Next(ops.Next(p1))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.1068, abs=EPSILON)


def test_case06(trace: Trace, p1: Predicate):
    formula = ops.Next(ops.Next(ops.Next(p1)))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.4967, abs=EPSILON)


def test_case07(trace: Trace, p1: Predicate, p2: Predicate):
    formula = ops.Always(ops.Eventually(ops.And(p2, ops.Eventually(p1))))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(1.184, abs=EPSILON)


def test_case08(trace: Trace, p1: Predicate, p2: Predicate):
    formula = ops.Implies(p1, p2)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(2.0, abs=EPSILON)


def test_case09(trace: Trace, p1: Predicate, p2: Predicate):
    formula = ops.And(ops.Eventually(p1), ops.Always(ops.Implies(p1, ops.Eventually(p2))))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(2.816, abs=EPSILON)


def test_case10(trace: Trace, p1: Predicate):
    formula = ops.Always(ops.Implies(p1, ops.Eventually(ops.Not(p1))))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(-1.184, abs=EPSILON)


def test_case11(trace: Trace, p1: Predicate):
    formula = ops.Always(ops.Or(ops.Not(p1), ops.Eventually(ops.Always(ops.Not(p1)))))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(-1.184, abs=EPSILON)


def test_case12(trace: Trace, p1: Predicate):
    formula = ops.Always(ops.Next(p1))
    result = banquo.evaluate(formula, trace)

    assert result == banquo.Bottom


def test_case13(trace: Trace, p1: Predicate):
    formula = ops.Eventually(ops.Next(p1))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.7508, abs=EPSILON)


def test_case14(trace: Trace, p1: Predicate):
    formula = ops.Always(ops.Next(ops.Next(p1)))
    result = banquo.evaluate(formula, trace)

    assert result == banquo.Bottom


def test_case15(trace: Trace, p1: Predicate):
    formula = ops.Next(ops.Eventually(ops.Next(p1)))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.7508, abs=EPSILON)


"""
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
"""


def test_case23(trace: Trace, p1: Predicate, p2: Predicate):
    formula = ops.Not(ops.Eventually.with_bounds((0.0, 3.5), ops.Not(ops.And(p2, p1))))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(0.2492, abs=EPSILON)


def test_case24(trace: Trace, p1: Predicate):
    formula = ops.Not(ops.Eventually.with_bounds((0.0, 1.0), ops.Not(p1)))
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(2.0, abs=EPSILON)


def test_case25(trace: Trace, p1: Predicate):
    formula = ops.Eventually.with_bounds((0.1, 30.0), p1)
    result = banquo.evaluate(formula, trace)

    assert isinstance(result, float)
    assert banquo.evaluate(formula, trace) == pytest.approx(3.7508, abs=EPSILON)
