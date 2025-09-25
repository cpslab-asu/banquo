from __future__ import annotations

import typing

from . import _banquo_impl as _impl

if typing.TYPE_CHECKING:
    from .core import Formula

S = typing.TypeVar("S")
M = typing.TypeVar("M")


class OperatorMixin:
    def not_(self: Formula[S, _impl.M_neg]) -> Not[S, _impl.M_neg]:
        return Not(self)

    def and_(self: Formula[S, _impl.M_lt], other: Formula[S, _impl.M_lt]) -> And[S, _impl.M_lt]:
        return And(self, other)


class Not(_impl.Not[S, _impl.M_neg], OperatorMixin):
    ...


class And(_impl.And[S, _impl.M_lt], OperatorMixin):
    ...
