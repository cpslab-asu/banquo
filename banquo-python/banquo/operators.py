from __future__ import annotations

import typing

from ._banquo_impl import M_neg, M_lt
from ._banquo_impl import And as _And
from ._banquo_impl import Not as _Not

if typing.TYPE_CHECKING:
    from .core import Formula

S = typing.TypeVar("S")
M = typing.TypeVar("M")


class OperatorMixin:
    def not_(self: Formula[S, M_neg]) -> Not[S, M_neg]:
        return Not(self)

    def and_(self: Formula[S, M_lt], other: Formula[S, M_lt]) -> And[S, M_lt]:
        return And(self, other)


class Not(_Not[S, M_neg], OperatorMixin):
    ...


class And(_And[S, M_lt], OperatorMixin):
    ...
