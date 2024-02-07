//! Expressions and operators for creating [temporal logic] [`Formula`]s to evaluate timed sequences
//! of system states called [`Trace`]s.
//!
//! Temporal logic formulas are a system for the evaluation of logical propositions over time are
//! are useful for evaluating the behaviors of complex systems over time. Behavioral requirements
//! can be something as simple as _The aircraft altitude should always be greater than zero_ or
//! something more complex like _The vehicle should always brake within 3 seconds if a pedestrian
//! is detected inside of a 60 degree cone of the front of the vehicle_. To determine if a system
//! violates a behavioral requirement, we can provide a set of system states along with the times
//! each state was recorded called a `Trace`, which the formula can evaluate into a metric value.
//! This process is called monitoring, making `Banquo` a _temporal logic monitor_. The computed
//! metric value is useful because it can not only indicate if the requirement was satisfied,
//! it can also provide a measure of distance of the system from a violation. This enables the use 
//! of search-based testing techniques to find examples of violating inputs by trying to minimize
//! the distance from violation. The most common type of metric value for evaluating formulas is
//! called [robustness], which is a real number representing the distance of the system state from
//! the [polytope] representing the set of states that violate the requirement.
//!
//! As an example, consider the automatic transmission system in a car. We might create the
//! following requirement when testing the vehicle:
//!
//! ```text
//! [] ((gear = 3 /\ rpm >= 4000) -> <>[0,3] gear = 4)
//! ```
//!
//! This formula reads _At every time, if the gear is equal to 3 and the rpm is greater than
//! or equal to 4000, then within the next 3 time-steps the gear should be equal to 4_. With a
//! requirement like this, we could attempt to find some combination of system inputs in which the
//! system does not behave properly.
//!
//! [polytope]: https://en.wikipedia.org/wiki/Polytope
//! [robustness]: https://link.springer.com/chapter/10.1007/11940197_12
//! [temporal logic]: https://en.wikipedia.org/wiki/Temporal_logic
//!
//! # Examples
//!
//! A formula is constructed by combining expressions (like a [`Predicate`]), which evaluate states
//! into metric values, and [`operators`], which manipulate metric values. An example formula might
//! look as follows:
//!
//! ```rust
//! use banquo::predicate;
//! use banquo::operators::{Always, Not, Or};
//!
//! let p1 = predicate!{ x <= 10.0 };// Expression
//! let p2 = predicate!{ y <= 3.0 }; // Expression
//!
//! let formula = Always::unbounded(Or::new(p1, Not::new(p2)));  // Operators
//! ```
//!
//! In order to evaluate a formula you must provide a [`Trace`], which is a set of value where each
//! value has an associated time. A trace is constructed like so:
//!
//! ```rust
//! use banquo::Trace;
//!
//! let mut t1 = Trace::new();
//! t1.insert(0.0, 99);
//! t1.insert(1.0, 100);
//! t1.insert(2.0, 107);
//! t1.insert(3.0, 111);
//! t1.insert(4.0, 115);
//!
//! let t2 = Trace::from([
//!     (0.0, 99),
//!     (1.0, 100),
//!     (2.0, 107),
//!     (3.0, 111),
//!     (4.0, 115),
//! ]);
//! ```
//!
//! Given a `Trace`, the formula can be evaluated using the [`Formula::evaluate`] method into a
//! [`Result`] containing either a `Trace` of metric values, or an error.
//! 
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::{Formula, Trace, predicate};
//! use banquo::operators::Always;
//!
//! // A predicate can only evaluate a named variable set, so we define a helper function to create
//! // hash maps for a single variable instead of repeating ourselves.
//! fn state(value: f64) -> HashMap<&'static str, f64> {
//!     HashMap::from([("x", value)])
//! }
//!
//! let trace = Trace::from([
//!     (0.0, state(99.0)),
//!     (1.0, state(100.0)),
//!     (2.0, state(107.0)),
//!     (3.0, state(111.0)),
//!     (4.0, state(115.0)),
//! ]);
//!
//! let phi = Always::unbounded(predicate!{ x <= 110.0 });
//! let result: Result<Trace<f64>, _> = phi.evaluate(&trace);
//! ```
//!
//! Formulas can also be evaluated using the [`evaluate`] function, which returns the first metric
//! in the output trace on successful evaluation.
//!
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::{EvaluationError, Trace, evaluate, predicate};
//! use banquo::operators::Always;
//!
//! // A predicate can only evaluate a named variable set, so we define a helper function to create
//! // hash maps for a single variable instead of repeating ourselves.
//! fn state(value: f64) -> HashMap<&'static str, f64> {
//!     HashMap::from([("x", value)])
//! }
//!
//! let trace = Trace::from([
//!     (0.0, state(99.0)),
//!     (1.0, state(100.0)),
//!     (2.0, state(107.0)),
//!     (3.0, state(111.0)),
//!     (4.0, state(115.0)),
//! ]);
//!
//! let phi = Always::unbounded(predicate!{ x <= 110.0 });
//! let result: Result<f64, EvaluationError> = evaluate(&trace, &phi);
//! ```

extern crate banquo_core as core;

#[doc(inline)]
pub use core::{Formula, EvaluationError, evaluate};

#[doc(inline)]
pub use core::metrics::{Top, Bottom, Meet, Join};

#[doc(inline)]
pub use core::trace;

#[doc(inline)]
pub use core::trace::Trace;

#[doc(inline)]
pub use core::operators;

#[doc(inline)]
pub use core::predicate;

#[doc(inline)]
pub use core::predicate::Predicate;

#[cfg(feature = "hybrid-distance")]
#[doc(inline)]
pub use banquo_hybrid_distance::{
    HybridDistance,
    HybridPredicate,
    HybridState,
    automaton,
    predicate as hybrid_predicate,
};
