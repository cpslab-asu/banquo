from __future__ import annotations

from collections.abc import Iterable, Mapping
from typing import Protocol, TypeVar, cast, overload

from ._banquo_impl import Trace as _Trace

S = TypeVar("S", contravariant=True)
M = TypeVar("M", covariant=True)


class Trace(_Trace[M]):
    @overload
    def __init__(self, elements: Mapping[float, M], /) -> None:
        ...

    @overload
    def __init__(self, *, times: Iterable[float], states: Iterable[M]) -> None:
        ...

    def __init__(
        self,
        times: Mapping[float, M] | Iterable[float],
        states: Iterable[M] | None = None,
    ):
        if states is not None:
            elements = {time: state for time, state in zip(times, states)}
        elif isinstance(times, Mapping):
            elements = dict(cast(Mapping[float, M], times))
        else:
            raise ValueError("Must provide elements as two lists, or a mapping")

        super().__init__(elements)


class Formula(Protocol[S, M]):
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        ...
