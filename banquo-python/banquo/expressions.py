from __future__ import annotations

import typing

if typing.TYPE_CHECKING:
    from collections.abc import Mapping

from ._banquo_impl import Predicate as _Predicate
from .operators import OperatorMixin


class Predicate(_Predicate, OperatorMixin):
    def __new__(cls, coefficients: Mapping[str, float], constant: float):
        return super().__new__(cls, dict(coefficients), constant)
