from __future__ import annotations

from typing import Protocol, TypeVar

from typing_extensions import Self, override

from ._banquo_impl import Trace

S = TypeVar("S", contravariant=True)
M = TypeVar("M", covariant=True)


class Formula(Protocol[S, M]):
    def evaluate(self, trace: Trace[S]) -> Trace[M]: ...


class SupportsNeg(Protocol):
    def __neg__(self) -> Self:
        ...


class SupportsLE(Protocol):
    def __le__(self, value: Self, /) -> bool:
        ...


class SupportsMeet(SupportsLE, Protocol):
    def min(self, other: Self) -> Self:
        ...


class SupportsGE(Protocol):
    def __ge__(self, value: Self, /) -> bool:
        ...


class SupportsJoin(SupportsGE, Protocol):
    def max(self, other: Self) -> Self:
        ...


class Metric(SupportsNeg, SupportsMeet, SupportsJoin, Protocol):
    ...


class TraceWrapper(Formula[S, M]):
    """Wrapper to convert traces returned from rust-implemented operators into Trace values."""

    def __init__(self, inner: Formula[S, M]):
        self.inner: Formula[S, M] = inner

    @override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        result: _Trace[M] = self.inner.evaluate(trace)
        return result if isinstance(result, Trace) else Trace(result)
