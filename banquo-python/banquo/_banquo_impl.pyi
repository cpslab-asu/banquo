from collections.abc import Iterable, Mapping
from typing import Generic, TypeVar

from typing_extensions import Self, TypeAlias, override

from .core import Formula

T = TypeVar("T", covariant=True)

class Trace(Generic[T]):
    def __new__(cls, elements: Mapping[float, T]) -> Self: ...
    @classmethod
    def from_timed_states(cls, times: Iterable[float], states: Iterable[T]) -> Trace[T]: ...

class Polynomial:
    def __init__(self, *, terms: dict[str, float], constant: float) -> None: ...

class Predicate(Formula[dict[str, float], float]):
    def __new__(cls, coefficients: dict[str, float], constant: float) -> Self: ...
    @override
    def evaluate(self, trace: Trace[dict[str, float]]) -> Trace[float]: ...

S = TypeVar("S")

class SupportsNeg(Protocol):
    def __neg__(self) -> Self: ...

M_neg = TypeVar("M_neg", bound=SupportsNeg, covariant=True)

class Not(Formula[S, M_neg]):
    def __new__(cls, inner: Formula[S, M_neg]) -> Self: ...
    @override
    def evaluate(self, trace: Trace[S]) -> Trace[M_neg]: ...

class SupportsLT(Protocol):
    def __lt__(self, value: Self, /) -> bool: ...

M_lt = TypeVar("M_lt", bound=SupportsLT, covariant=True)

class And(Formula[S, M_lt]):
    def __new__(cls, lhs: Formula[S, M_lt], rhs: Formula[S, M_lt]) -> Self: ...
    @override
    def evaluate(self, trace: Trace[S]) -> Trace[M_lt]: ...
