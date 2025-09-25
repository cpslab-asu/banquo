from __future__ import annotations

import typing

from ._banquo_impl import Trace

S = typing.TypeVar("S")
M = typing.TypeVar("M")


class Formula(typing.Protocol[S, M]):
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        ...
