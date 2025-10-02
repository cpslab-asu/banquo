from banquo import Predicate, Trace, operators

def test_predicate_negation():
    elements = {
        0.0: {"x": 0.0, "y": 1.0},
        1.0: {"x": 7.2, "y": 1.1},
        2.0: {"x": 13.3, "y": 1.2},
        3.0: {"x": 21.4, "y": 1.3},
        4.0: {"x": 30.1, "y": 1.2},
        5.0: {"x": 38.2, "y": 1.5},
    }
    input = Trace(elements)
    formula = operators.Not(Predicate({"x": 1.0, "y": 1.0}, 40.0))
    expected = Trace({
        time: -40.0 + (state["x"] + state["y"]) for time, state in elements.items()
    })

    assert formula.evaluate(input) == expected
