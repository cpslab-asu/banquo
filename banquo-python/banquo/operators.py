from __future__ import annotations

import typing

from typing_extensions import override

from ._banquo_impl import And as _And
from ._banquo_impl import Always as _Always
from ._banquo_impl import Not as _Not
from ._banquo_impl import Trace
from .core import Formula, SupportsNeg, SupportsLE, SupportsMeet

S = typing.TypeVar("S")
M = typing.TypeVar("M", covariant=True)
M_neg = typing.TypeVar("M_neg", bound=SupportsNeg, covariant=True)
M_le = typing.TypeVar("M_le", bound=SupportsLE, covariant=True)
M_min = typing.TypeVar("M_min", bound=SupportsMeet, covariant=True)


class OperatorMixin:
    def and_(self: Formula[S, M_le], other: Formula[S, M_le]) -> And[S, M_le]:
        return And(self, other)


class Operator(typing.Generic[S, M], Formula[S, M], OperatorMixin):
    def __init__(self, formula: Formula[S, M]):
        self.inner: Formula[S, M] = formula

    @override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        return self.inner.evaluate(trace)


class Not(Operator[S, M_neg]):
    def __init__(self, formula: Formula[S, M_neg]):
        if isinstance(formula, Operator):
            formula = formula.inner

        super().__init__(_Not(formula))


class And(Operator[S, M_le]):
    def __init__(self, lhs: Formula[S, M_le], rhs: Formula[S, M_le]):
        if isinstance(lhs, Operator):
            lhs = lhs.inner

        if isinstance(rhs, Operator):
            rhs = rhs.inner

        super().__init__(_And(lhs, rhs))
