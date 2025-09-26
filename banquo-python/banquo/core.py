from __future__ import annotations

from typing import Protocol, TypeVar

from ._banquo_impl import Trace

S = TypeVar("S", contravariant=True)
M = TypeVar("M", covariant=True)


class Formula(Protocol[S, M]):
    def evaluate(self, trace: Trace[S]) -> Trace[M]: ...
