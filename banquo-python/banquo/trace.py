from __future__ import annotations

from collections.abc import Iterable, Mapping
from typing import TypeVar

from typing_extensions import override

from ._banquo_impl import Trace as _Trace

T = TypeVar("T", covariant=True)


def _iter_eq(lhs: Iterable[object], rhs: Iterable[object]) -> bool:
    return list(lhs) == list(rhs)


class Trace(_Trace[T]):
    def __new__(cls, elements: Mapping[float, T]):
        return super().__new__(cls, dict(elements))

    def __iter__(self) -> Iterable[tuple[float, T]]:
        return zip(self.times(), self.states())

    @override
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Trace):
            return NotImplemented

        return _iter_eq(self.times(), other.times()) and _iter_eq(self.states(), other.states())

    @classmethod
    def from_timed_states(cls, times: Iterable[float], states: Iterable[T]) -> Trace[T]:
        return cls({time: state for time, state in zip(times, states, strict=True)})
