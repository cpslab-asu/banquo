from __future__ import annotations

import typing

from typing_extensions import TypeAlias

from ._banquo_impl import And as _And
from ._banquo_impl import Always as _Always
from ._banquo_impl import Not as _Not
from ._banquo_impl import PanicException
from .core import Formula, EnsureInput, SupportsNeg, SupportsLE, SupportsMeet, EnsureOutput
from .trace import Trace

Bounds: TypeAlias = tuple[float, float]

S = typing.TypeVar("S")
M = typing.TypeVar("M", covariant=True)
M_neg = typing.TypeVar("M_neg", bound=SupportsNeg, covariant=True)
M_le = typing.TypeVar("M_le", bound=SupportsLE, covariant=True)
M_min = typing.TypeVar("M_min", bound=SupportsMeet, covariant=True)


class OperatorMixin:
    def and_(self: Formula[S, M_le], other: Formula[S, M_le]) -> And[S, M_le]:
        return And(self, other)


class MetricAttributeError(AttributeError):
    def __init__(self, missing_method: str):
        super().__init__(f"Metric type {type} must implement {missing_method} method")


class Operator(EnsureOutput[S, M], OperatorMixin):
    def __init__(self, subformula: Formula[S, M], required_method: str):
        super().__init__(subformula)
        self.required_method: str = required_method

    @typing.override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        try:
            return super().evaluate(trace)
        except PanicException:
            raise MetricAttributeError(self.required_method)


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


class Always(Operator[S, M_min]):
    def __init__(self, subformula: Formula[S, M_min]):
        if isinstance(subformula, _Always):
            inner = subformula
        else:
            inner = _Always(None, _inner_or_wrap(subformula))

        super().__init__(inner, "__le__")

    @classmethod
    def with_bounds(cls, bounds: Bounds, subformula: Formula[S, M_min]) -> Always[S, M_min]:
        return cls(_Always(bounds, _inner_or_wrap(subformula)))
