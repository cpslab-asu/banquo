from __future__ import annotations

from dataclasses import dataclass
from typing import TypeVar, override

from pytest import fixture, raises

from banquo import Bottom, Trace, operators
from banquo.core import Formula

T = TypeVar("T")


@dataclass()
class BadMetric:
    value: float


@dataclass()
class GoodMetric(BadMetric):
    @override
    def __eq__(self, other: object) -> bool:
        if isinstance(other, GoodMetric):
            return self.value == other.value

        if isinstance(other, float):
            return self.value == other

        return NotImplemented

    def __neg__(self) -> GoodMetric:
        return GoodMetric(-self.value)

    def __le__(self, other: object, /) -> bool:
        if not isinstance(other, GoodMetric):
            return NotImplemented

        return self.value <= other.value

    def __ge__(self, other: object, /) -> bool:
        if not isinstance(other, GoodMetric):
            return NotImplemented

        return self.value >= other.value


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

    @fixture
    def good_trace(self, input: Trace[float]) -> Trace[GoodMetric]:
        return Trace({time: GoodMetric(state) for time, state in input})

    @fixture
    def bad_trace(self, input: Trace[float]) -> Trace[BadMetric]:
        return Trace({time: BadMetric(state) for time, state in input})


class TestNegation(UnaryTest):
    @fixture
    def expected(self, input: Trace[float]) -> Trace[float]:
        return Trace({time: -state for time, state in input})

    def test_evaluation(self, input: Trace[float], expected: Trace[float]):
        formula = operators.Not(Const[float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[GoodMetric], expected: Trace[float]):
        formula = operators.Not(Const[GoodMetric]())
        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, bad_trace: Trace[BadMetric]):
        formula = operators.Not(Const[BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

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

    @fixture
    def good_trace(self, input: Trace[tuple[float, float]]) -> Trace[tuple[GoodMetric, GoodMetric]]:
        return Trace({
            time: (GoodMetric(state[0]), GoodMetric(state[1])) for time, state in input
        })

    @fixture
    def bad_trace(sefl, input: Trace[tuple[float, float]]) -> Trace[tuple[BadMetric, BadMetric]]:
        return Trace({
            time: (BadMetric(state[0]), BadMetric(state[1])) for time, state in input
        })


class TestConjunction(BinaryTest):
    @fixture
    def expected(self, input: Trace[tuple[float, float]]) -> Trace[float]:
        return Trace({time: min(state) for time, state in input})

    def test_evaluation(self, input: Trace[tuple[float, float]], expected: Trace[float]):
        formula = operators.And(Left[float, float](), Right[float, float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[tuple[GoodMetric, GoodMetric]], expected: Trace[float]):
        formula = operators.And(Left[GoodMetric, GoodMetric](), Right[GoodMetric, GoodMetric]())
        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, bad_trace: Trace[tuple[BadMetric, BadMetric]]):
        formula = operators.And(Left[BadMetric, BadMetric](), Right[BadMetric, BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]


class TestDisjunction(BinaryTest):
    @fixture
    def expected(self, input: Trace[tuple[float, float]]) -> Trace[float]:
        return Trace({time: max(state) for time, state in input})

    def test_evaluation(self, input: Trace[tuple[float, float]], expected: Trace[float]):
        formula = operators.Or(Left[float, float](), Right[float, float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[tuple[GoodMetric, GoodMetric]], expected: Trace[float]):
        formula = operators.Or(Left[GoodMetric, GoodMetric](), Right[GoodMetric, GoodMetric]())
        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, bad_trace: Trace[tuple[BadMetric, BadMetric]]):
        formula = operators.Or(Left[BadMetric, BadMetric](), Right[BadMetric, BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]


class TestImplication(BinaryTest):
    @fixture
    def expected(self, input: Trace[tuple[float, float]]) -> Trace[float]:
        return Trace({time: max(-state[0], state[1]) for time, state in input})

    def test_evaluation(self, input: Trace[tuple[float, float]], expected: Trace[float]):
        formula = operators.Implies(Left[float, float](), Right[float, float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[tuple[GoodMetric, GoodMetric]], expected: Trace[float]):
        formula = operators.Implies(Left[GoodMetric, GoodMetric](), Right[GoodMetric, GoodMetric]())
        result = formula.evaluate(good_trace)
        assert result  == expected

    def test_unsupported_metric(self, bad_trace: Trace[tuple[BadMetric, BadMetric]]):
        formula = operators.Implies(Left[BadMetric, BadMetric](), Right[BadMetric, BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]


class TestNext(UnaryTest):
    @fixture
    def expected(self) -> Trace[float]:
        return Trace({
            0.0: 1.1,
            1.0: 1.2,
            2.0: 1.3,
            3.0: 1.2,
            4.0: 1.5,
            5.0: Bottom,
        })

    def test_evaluation(self, input: Trace[float], expected: Trace[float]):
        formula = operators.Next(Const[float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_userdefined_metric(self, good_trace: Trace[GoodMetric], expected: Trace[float]):
        formula = operators.Next(Const[GoodMetric]())
        assert formula.evaluate(good_trace) == expected


class TestGlobally(UnaryTest):
    @fixture
    def expected(self) -> Trace[float]:
        return Trace({
            0.0: 1.0,
            1.0: 1.1,
            2.0: 1.2,
            3.0: 1.2,
            4.0: 1.2,
            5.0: 1.5,
        })

    def test_bounded_evaluation(self, input: Trace[float], expected: Trace[float]):
        formula = operators.Always(Const[float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_unbounded_evaluation(self, input: Trace[float]):
        formula = operators.Always.with_bounds((0.0, 2.0), Const[float]())
        expected = Trace({
            0.0: 1.0,
            1.0: 1.1,
            2.0: 1.2,
            3.0: 1.2,
            4.0: 1.2,
            5.0: 1.5,
        })
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[GoodMetric], expected: Trace[float]):
        formula = operators.Always(Const[GoodMetric]())
        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, bad_trace: Trace[BadMetric]):
        formula = operators.Always(Const[BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]


class TestFinally(UnaryTest):
    @fixture
    def expected(self) -> Trace[float]:
        return Trace({
            0.0: 1.5,
            1.0: 1.5,
            2.0: 1.5,
            3.0: 1.5,
            4.0: 1.5,
            5.0: 1.5,
        })

    def test_bounded_evaluation(self, input: Trace[float], expected: Trace[float]):
        formula = operators.Eventually(Const[float]())
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_unbounded_evaluation(self, input: Trace[float]):
        formula = operators.Eventually.with_bounds((0.0, 2.0), Const[float]())
        expected = Trace({
            0.0: 1.2,
            1.0: 1.3,
            2.0: 1.3,
            3.0: 1.5,
            4.0: 1.5,
            5.0: 1.5,
        })
        result = formula.evaluate(input)

        assert isinstance(result, Trace)
        assert result == expected

    def test_supported_metric(self, good_trace: Trace[GoodMetric], expected: Trace[float]):
        formula = operators.Eventually(Const[GoodMetric]())
        assert formula.evaluate(good_trace) == expected

    def test_unsupported_metric(self, bad_trace: Trace[BadMetric]):
        formula = operators.Eventually(Const[BadMetric]())  # pyright: ignore[reportArgumentType, reportUnknownVariableType]

        with raises(operators.MetricAttributeError):
            _ = formula.evaluate(bad_trace)  # pyright: ignore[reportUnknownVariableType]
