from __future__ import annotations

from collections.abc import Iterable, Iterator, Mapping
from typing import TypeVar

from typing_extensions import override

from ._banquo_impl import Trace as _Trace

T = TypeVar("T", covariant=True)
U = TypeVar("U", covariant=True)


def _iter_eq(lhs: Iterable[object], rhs: Iterable[object]) -> bool:
    return list(lhs) == list(rhs)


class Trace(_Trace[T], Iterable[tuple[float, T]]):
    def __new__(cls, elements: Mapping[float, T] | _Trace[T]):
        return super().__new__(cls, elements if isinstance(elements, _Trace) else dict(elements))

    @override
    def __iter__(self) -> Iterator[tuple[float, T]]:
        return zip(self.times(), self.states())

    @override
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, _Trace):
            return NotImplemented

        return _iter_eq(self.times(), other.times()) and _iter_eq(self.states(), other.states())

    @staticmethod
    def from_timed_states(times: Iterable[float], states: Iterable[U]) -> Trace[U]:
        return Trace({time: state for time, state in zip(times, states, strict=True)})
