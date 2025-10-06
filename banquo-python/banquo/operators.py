from __future__ import annotations

import typing

from typing_extensions import TypeAlias

from ._banquo_impl import And as _And
from ._banquo_impl import Always as _Always
from ._banquo_impl import Not as _Not
from ._banquo_impl import PanicException
from .core import Formula, SupportsNeg, SupportsLE, SupportsMeet, TraceWrapper
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


class Operator(TraceWrapper[S, M], OperatorMixin):
    def __init__(self, subformula: Formula[S, M], required_method: str):
        super().__init__(subformula)
        self.required_method: str = required_method

    @typing.override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        try:
            return super().evaluate(trace)
        except PanicException:
            raise MetricAttributeError(self.required_method)


class Not(Operator[S, M_neg]):
    def __init__(self, formula: Formula[S, M_neg]):
        if isinstance(formula, TraceWrapper):
            formula = formula.inner

        super().__init__(_Not(formula), "__neg__")


class And(Operator[S, M_le]):
    def __init__(self, lhs: Formula[S, M_le], rhs: Formula[S, M_le]):
        if isinstance(lhs, TraceWrapper):
            lhs = lhs.inner

        if isinstance(rhs, TraceWrapper):
            rhs = rhs.inner

        super().__init__(_And(lhs, rhs), "__le__")


class Always(Operator[S, M_min]):
    def __init__(self, subformula: Formula[S, M_min]):
        if isinstance(subformula, _Always):
            inner = subformula
        elif isinstance(subformula, TraceWrapper):
            inner = _Always(None, subformula.inner)
        else:
            inner = _Always(None, subformula)

        super().__init__(inner, "__le__")

    @classmethod
    def with_bounds(cls, bounds: Bounds, subformula: Formula[S, M_min]) -> Always[S, M_min]:
        return cls(_Always(bounds, subformula))
