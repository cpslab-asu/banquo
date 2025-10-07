from __future__ import annotations

from typing import Protocol, TypeVar

from typing_extensions import Self, override

from ._banquo_impl import Trace as _Trace
from .trace import Trace

S = TypeVar("S", contravariant=True)
M = TypeVar("M", covariant=True)


class Formula(Protocol[S, M]):
    def evaluate(self, trace: Trace[S]) -> _Trace[M]: ...


class SupportsNeg(Protocol):
    def __neg__(self) -> Self:
        ...


class SupportsLE(Protocol):
    def __le__(self, value: Self, /) -> bool:
        ...


class SupportsGE(Protocol):
    def __ge__(self, value: Self, /) -> bool:
        ...

class SupportsNegGE(SupportsNeg, SupportsGE, Protocol):
    ...


class EnsureInput(Formula[S, M]):
    def __init__(self, inner: Formula[S, M]):
        self.inner: Formula[S, M] = inner

    @override
    def evaluate(self, trace: _Trace[S]) -> _Trace[M]:
        return self.inner.evaluate(trace if isinstance(trace, Trace) else Trace(trace))


class EnsureOutput(Formula[S, M]):
    """Wrapper to convert traces returned from rust-implemented operators into Trace values."""

    def __init__(self, inner: Formula[S, M]):
        self.inner: Formula[S, M] = inner

    @override
    def evaluate(self, trace: Trace[S]) -> Trace[M]:
        result: _Trace[M] = self.inner.evaluate(trace)
        return result if isinstance(result, Trace) else Trace(result)
