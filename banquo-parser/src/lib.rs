//! Parse temporal logic formulas from strings into Banquo policies (formulas).
//!
//! # Testing
//!
//! Run the parser tests from the workspace root:
//!
//! ```bash
//! cargo test -p banquo-parser
//! ```
//!
//! # Parsing strings into policies
//!
//! - **Numeric / STL-style formulas** (e.g. `3.1*x + 22.4*y <= 4.8*z`, with temporal operators):
//!   use [`parse_formula`] or [`parse_predicate`].
//! - **Hybrid formulas** (named predicates from a map): use [`parse_hybrid_formula`].
//!
//! See the documentation for each function and the tests in `src/parser/` for supported syntax.

pub mod ltl;
pub mod mtl;
pub mod stl;

use chumsky::Parser;
use chumsky::error::Rich;
use chumsky::extra;

pub type Err<'src> = extra::Err<Rich<'src, char>>;

fn num<'src>() -> impl Parser<'src, &'src str, f64, Err<'src>> + Clone {
    let frac = chumsky::primitive::just(".").then(chumsky::text::int(10));

    chumsky::primitive::just("-")
        .or_not()
        .then(chumsky::text::int(10))
        .then(frac.or_not())
        .to_slice()
        .map(|s: &str| s.parse().unwrap())
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    #[test]
    fn test_num() {
        let parser = super::num();

        assert_eq!(parser.parse("1").unwrap(), 1.0);
        assert_eq!(parser.parse("0.4").unwrap(), 0.4);
        assert_eq!(parser.parse("-3.2").unwrap(), -3.2);
    }
}
