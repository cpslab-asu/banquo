from banquo import Polynomial, Predicate


def test_predicate():
    expected = Predicate(
        Polynomial(terms={"x": 4.0, "y": 7.2, "z": -3.1}, constant=0.0),
        Polynomial(terms={}, constant=34.8)
    )

    assert Predicate({"x": 4.0, "y": 7.2, "z": -3.1}, 34.8) == expected
