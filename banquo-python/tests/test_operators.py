from __future__ import annotations

from dataclasses import dataclass
from typing import TypeVar, override

from pytest import fixture, raises

from banquo import Trace, operators
from banquo.core import Formula

T = TypeVar("T")


@dataclass()
class BadMetric:
    value: float


@dataclass()
class GoodMetric(BadMetric):
    def __neg__(self) -> GoodMetric:
        return GoodMetric(-self.value)

    def __le__(self, other: object, /) -> bool:
        if not isinstance(other, GoodMetric):
            return NotImplemented

        return self.value <= other.value


class Const(Formula[T, T]):
    @override
    def evaluate(self, trace: Trace[T]) -> Trace[T]:
        return trace


class UnaryTest:
    @fixture
    def input(self) -> Trace[float]:
        return Trace({
            0.0: 1.0,
            1.0: 1.1,
            2.0: 1.2,
            3.0: 1.3,
            4.0: 1.2,
            5.0: 1.5,
        })


class TestNegation(UnaryTest):
    def test_evaluation(self, input: Trace[float]):
        formula = operators.Not(Const[float]())
        expected = Trace({time: -state for time, state in input})
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, input: Trace[float]):
        formula = operators.Not(Const[GoodMetric]())
        good_trace = Trace({time: GoodMetric(value) for time, value in input})
        expected = Trace({time: -state for time, state in good_trace})

        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, input: Trace[float]):
        formula = operators.Not(Const[BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]
        bad_trace = Trace({time: BadMetric(value) for time, value in input})

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]


L = TypeVar("L")
R = TypeVar("R")


class Left(Formula[tuple[L, R], L]):
    @override
    def evaluate(self, trace: Trace[tuple[L, R]]) -> Trace[L]:
        assert isinstance(trace, Trace)
        return Trace({time: state[0] for time, state in trace})


class Right(Formula[tuple[L, R], R]):
    @override
    def evaluate(self, trace: Trace[tuple[L, R]]) -> Trace[R]:
        return Trace({time: state[1] for time, state in trace})


class BinaryTest:
    @fixture
    def input(self) -> Trace[tuple[float, float]]:
        return Trace({
            0.0: (0.0, 1.0),
            1.0: (1.0, 0.0),
            2.0: (2.0, 4.0),
            3.0: (3.0, 6.0),
        })


class TestConjunction(BinaryTest):
    def test_evaluation(self, input: Trace[tuple[float, float]]):
        formula = operators.And(Left[float, float](), Right[float, float]())
        expected = Trace({time: min(state) for time, state in input})
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, input: Trace[tuple[float, float]]):
        formula = operators.And(Left[GoodMetric, GoodMetric](), Right[GoodMetric, GoodMetric]())
        good_trace = Trace({
            time: (GoodMetric(value[0]), GoodMetric(value[1])) for time, value in input
        })
        expected = Trace({time: GoodMetric(min(state)) for time, state in input})

        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, input: Trace[tuple[float, float]]):
        formula = operators.And(Left[BadMetric, BadMetric](), Right[BadMetric, BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]
        bad_trace = Trace({time: (BadMetric(value[0]), BadMetric(value[1])) for time, value in input})

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]
