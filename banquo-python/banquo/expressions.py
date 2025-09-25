from __future__ import annotations

import functools

from . import _banquo_impl as _impl
from .operators import OperatorMixin


class Polynomial(_impl.Polynomial):
    ...


@functools.singledispatch
def _to_polynomial(value: object) -> Polynomial:
    raise TypeError(f"Cannot convert value of type {type} to polynomial")


@_to_polynomial.register
def _(value: float) -> Polynomial:
    return Polynomial(terms={}, constant=value)


@_to_polynomial.register
def _(value: dict[str, float]) -> Polynomial:
    return Polynomial(terms=value, constant=0.0)


@_to_polynomial.register
def _(value: Polynomial) -> Polynomial:
    return value


class Predicate(_impl.Predicate, OperatorMixin):
    def __init__(
        self,
        lhs: Polynomial | dict[str, float] | float,
        rhs: Polynomial | dict[str, float] | float,
    ):
        super().__init__(_to_polynomial(lhs), _to_polynomial(rhs))
