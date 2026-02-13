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

pub mod expressions;
mod parser;

// Re-export core types so the parser can use them.
pub use banquo_core::{Formula, Trace};
pub use banquo_core::operators;

pub use parser::{
    parse_formula, parse_hybrid_formula, parse_predicate, IncompleteParseError, MissingPredicateError,
    ParsedFormulaError, ParsedFormula,
};
pub use parser::{HybridVars, IntoPredicateMap, ParsedHybridFormula};
