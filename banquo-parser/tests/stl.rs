//! Integration test: parse strings into Banquo policies (formulas)
//!
//! Run with: `cargo test -p banquo-parser --test parse_string_to_policy`

use banquo::operators::Interval;
use banquo_core::predicate;
use banquo_core::stl::{Formula, Symbol};
use banquo_parser::stl::parse;

#[test]
fn predicate() {
    let p = predicate! { 3.1 * x + 22.4 * y <= 12.0 };
    let expected = Ok(Formula::from([Symbol::Predicate(p)]));

    assert_eq!(parse("3.1*x + 22.4 * y <= 12.0"), expected);
    assert_eq!(parse("x*3.1 + y * 22.4 <= 12.0"), expected);
    assert_eq!(parse("12.0 >= x*3.1 + y * 22.4"), expected);
}

#[test]
fn not() {
    let p = predicate! { 3.0 * x <= 11.0 };
    let expected = Ok(Formula::from([Symbol::Not, Symbol::Predicate(p)]));

    assert_eq!(parse("not 3.0 * x <= 11.0"), expected);
    assert_eq!(parse("!(3.0 * x <= 11.0)"), expected);
}

#[test]
fn next() {
    let p = predicate! { 3.0 * x <= 11.0 };
    let expected = Ok(Formula::from([Symbol::Next, Symbol::Predicate(p)]));

    assert_eq!(parse("X 3.0 * x <= 11.0"), expected);
}

#[test]
fn always() {
    let p = predicate! { 3.1 * x <= 0.5 * y };
    let expected = Ok(Formula::from([Symbol::Always(None), Symbol::Predicate(p.clone())]));

    assert_eq!(parse("always 3.1*x <= 0.5*y"), expected);
    assert_eq!(parse("[](3.1*x <= 0.5*y)"), expected);
    assert_eq!(parse("G 3.1*x <= 0.5*y"), expected);

    let expected = Ok(Formula::from([
        Symbol::Always(Some(Interval::from(0..=4))),
        Symbol::Predicate(p),
    ]));

    assert_eq!(parse("always{0,4} 3.1*x <= 0.5*y"), expected);
    assert_eq!(parse("[]{0,4.0}(3.1*x <= 0.5*y)"), expected);
    assert_eq!(parse("G{0,4} (3.1*x <= 0.5*y)"), expected);
}

#[test]
fn eventually() {
    let p = predicate! { 3.1 * x <= 0.5 * y };
    let expected = Ok(Formula::from([Symbol::Eventually(None), Symbol::Predicate(p.clone())]));

    assert_eq!(parse("eventually 3.1*x <= 0.5*y"), expected);
    assert_eq!(parse("<>(3.1*x <= 0.5*y)"), expected);
    assert_eq!(parse("F 3.1*x <= 0.5*y"), expected);

    let expected = Ok(Formula::from([
        Symbol::Eventually(Some(Interval::from(0..=4))),
        Symbol::Predicate(p),
    ]));

    assert_eq!(parse("eventually{0,4} 3.1*x <= 0.5*y"), expected);
    assert_eq!(parse("<>{0,4.0}(3.1*x <= 0.5*y)"), expected);
    assert_eq!(parse("F{0,4} 3.1*x <= 0.5*y"), expected);
}

#[test]
fn and() {
    let p1 = predicate! { 3.0 * x <= 11.0 };
    let p2 = predicate! { -2.0 * y + z <= 4.0 };
    let expected = Ok(Formula::from([
        Symbol::And,
        Symbol::Predicate(p1.clone()),
        Symbol::Predicate(p2.clone()),
    ]));

    assert_eq!(parse("3.0 * x <= 11.0 && -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse("(3.0 * x <= 11.0) and -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse(r"(3.0 * x <= 11.0) /\ -2.0 * y + z <= 4.0"), expected);

    let p3 = predicate! { a <= 5.0 };
    let expected = Formula::from([
        Symbol::And,
        Symbol::And,
        Symbol::Predicate(p1),
        Symbol::Predicate(p2),
        Symbol::Predicate(p3),
    ]);

    assert_eq!(
        parse("(3.0 * x <= 11.0 && -2.0 * y + z <= 4.0) and a <= 5.0"),
        Ok(expected)
    );
}

#[test]
fn or() {
    let p1 = predicate! { 3.0 * x <= 11.0 };
    let p2 = predicate! { -2.0 * y + z <= 4.0 };
    let expected = Ok(Formula::from([
        Symbol::Or,
        Symbol::Predicate(p1.clone()),
        Symbol::Predicate(p2.clone()),
    ]));

    assert_eq!(parse("3.0 * x <= 11.0 || -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse("(3.0 * x <= 11.0) or -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse(r"(3.0 * x <= 11.0) \/ -2.0 * y + z <= 4.0"), expected);

    let p3 = predicate! { a <= 5.0 };
    let expected = Formula::from([
        Symbol::Or,
        Symbol::Or,
        Symbol::Predicate(p1),
        Symbol::Predicate(p2),
        Symbol::Predicate(p3),
    ]);

    assert_eq!(
        parse("(3.0 * x <= 11.0 || -2.0 * y + z <= 4.0) or a <= 5.0"),
        Ok(expected)
    );
}

#[test]
fn implies() {
    let p1 = predicate! { 3.0 * x <= 11.0 };
    let p2 = predicate! { -2.0 * y + z <= 4.0 };
    let expected = Ok(Formula::from([
        Symbol::Implies,
        Symbol::Predicate(p1.clone()),
        Symbol::Predicate(p2.clone()),
    ]));

    assert_eq!(parse("3.0 * x <= 11.0 -> -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse("(3.0 * x <= 11.0) implies -2.0 * y + z <= 4.0"), expected);

    let p3 = predicate! { a <= 5.0 };
    let expected = Formula::from([
        Symbol::Implies,
        Symbol::Implies,
        Symbol::Predicate(p1),
        Symbol::Predicate(p2),
        Symbol::Predicate(p3),
    ]);

    assert_eq!(
        parse("(3.0 * x <= 11.0 -> -2.0 * y + z <= 4.0) implies a <= 5.0"),
        Ok(expected)
    );
}

#[test]
fn until() {
    let p1 = predicate! { 3.0 * x <= 11.0 };
    let p2 = predicate! { -2.0 * y + z <= 4.0 };
    let expected = Ok(Formula::from([
        Symbol::Until,
        Symbol::Predicate(p1.clone()),
        Symbol::Predicate(p2.clone()),
    ]));

    assert_eq!(parse("3.0 * x <= 11.0 U -2.0 * y + z <= 4.0"), expected);
    assert_eq!(parse("(3.0 * x <= 11.0) until -2.0 * y + z <= 4.0"), expected);
}

#[test]
fn invalid_formula() {
    assert!(parse("not a valid formula !!").is_err());
}
