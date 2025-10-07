from __future__ import annotations

import typing

from typing_extensions import TypeAlias

from ._banquo_impl import And as _And
from ._banquo_impl import Always as _Always
from ._banquo_impl import Eventually as _Eventually
from ._banquo_impl import Implies as _Implies
from ._banquo_impl import Not as _Not
from ._banquo_impl import Or as _Or
from ._banquo_impl import PanicException
from .core import Formula, EnsureInput, SupportsNeg, SupportsLE, SupportsGE, EnsureOutput, SupportsNegGE
from .trace import Trace

Bounds: TypeAlias = tuple[float, float]

S = typing.TypeVar("S")
M = typing.TypeVar("M", covariant=True)
M_neg = typing.TypeVar("M_neg", bound=SupportsNeg, covariant=True)
M_le = typing.TypeVar("M_le", bound=SupportsLE, covariant=True)
M_ge = typing.TypeVar("M_ge", bound=SupportsGE, covariant=True)
M_neg_ge = typing.TypeVar("M_neg_ge", bound=SupportsNegGE, covariant=True)


class OperatorMixin:
    def and_(self: Formula[S, M_le], other: Formula[S, M_le]) -> And[S, M_le]:
        return And(self, other)


class MetricAttributeError(AttributeError):
    def __init__(self, missing_method: str):
        super().__init__(f"Metric must implement {missing_method} method")


class Operator(EnsureOutput[S, M], OperatorMixin):
    def __init__(self, subformula: Formula[S, M], required_method: str):
        super().__init__(subformula)
        self.required_method: str = required_method

    @typing.override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        try:
            return super().evaluate(trace)
        except PanicException as e:
            raise MetricAttributeError(self.required_method) from e


def _inner_or_wrap(formula: Formula[S, M]) -> Formula[S, M]:
    if isinstance(formula, EnsureOutput):
        return formula.inner

    return EnsureInput(formula)


class Not(Operator[S, M_neg]):
    def __init__(self, formula: Formula[S, M_neg]):
        super().__init__(_Not(_inner_or_wrap(formula)), "__neg__")


class And(Operator[S, M_le]):
    def __init__(self, lhs: Formula[S, M_le], rhs: Formula[S, M_le]):
        super().__init__(_And(_inner_or_wrap(lhs), _inner_or_wrap(rhs)), "__le__")


class Or(Operator[S, M_ge]):
    def __init__(self, lhs: Formula[S, M_ge], rhs: Formula[S, M_ge]):
        super().__init__(_Or(_inner_or_wrap(lhs), _inner_or_wrap(rhs)), "__ge__")

class Implies(Operator[S, M_neg_ge]):
    def __init__(self, lhs: Formula[S, M_neg_ge], rhs: Formula[S, M_neg_ge]):
        super().__init__(_Implies(_inner_or_wrap(lhs), _inner_or_wrap(rhs)), "__neg__ and __ge__")


S_ = typing.TypeVar("S_")
M_le_ = typing.TypeVar("M_le_", bound=SupportsLE, covariant=True)

class Always(Operator[S, M_le]):
    def __init__(self, subformula: Formula[S, M_le]):
        if isinstance(subformula, _Always):
            inner = subformula
        else:
            inner = _Always(None, _inner_or_wrap(subformula))

        super().__init__(inner, "__le__")

    @staticmethod
    def with_bounds(bounds: Bounds, subformula: Formula[S_, M_le_]) -> Always[S_, M_le_]:
        return Always(_Always(bounds, _inner_or_wrap(subformula)))


M_ge_ = typing.TypeVar("M_ge_", bound=SupportsGE, covariant=True)


class Eventually(Operator[S, M_ge]):
    def __init__(self, subformula: Formula[S, M_ge]):
        if isinstance(subformula, _Eventually):
            inner = subformula
        else:
            inner = _Eventually(None, _inner_or_wrap(subformula))

        super().__init__(inner, "__le__")

    @staticmethod
    def with_bounds(bounds: Bounds, subformula: Formula[S_, M_ge_]) -> Eventually[S_, M_ge_]:
        return Eventually(_Eventually(bounds, _inner_or_wrap(subformula)))
