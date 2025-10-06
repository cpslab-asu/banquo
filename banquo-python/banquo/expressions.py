from __future__ import annotations

from collections.abc import Mapping

from ._banquo_impl import Predicate as _Predicate
from .operators import OperatorMixin
from .core import TraceWrapper


class Predicate(TraceWrapper[dict[str, float], float], OperatorMixin):
    def __init__(self, coefficients: Mapping[str, float], constant: float):
        super().__init__(_Predicate(dict(coefficients), constant))
