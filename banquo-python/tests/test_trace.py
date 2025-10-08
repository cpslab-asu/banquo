from __future__ import annotations

import itertools

import pytest

from banquo import Trace
from banquo.trace import MismatchedTimesStates

pytestmark = pytest.mark.unit


def test_trace():
    elements = {
        0.0: {"x": 0.0, "y": 1.0},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    }

    times = elements.keys()
    states = elements.values()

    assert Trace(elements) == Trace.from_timed_states(times, states)  # Ensure constructor parity
    assert Trace(Trace(elements)) == Trace(elements)  # Ensure conversion is lossless

    short_states = itertools.islice(elements.values(), 4)  # Only produce 4 states

    with pytest.raises(MismatchedTimesStates):
        _ = Trace.from_timed_states(elements.keys(), short_states)
