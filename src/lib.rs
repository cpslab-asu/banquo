#![deny(clippy::all)]

pub mod expressions;
pub mod formula;
pub mod formulas;
pub mod metric;
pub mod operators;
pub mod trace;

#[cfg(feature = "parser")]
pub mod parser;

pub use formula::{Formula, evaluate};
pub use metric::{Join, Meet};
