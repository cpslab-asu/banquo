//! Combinatorial operators for constructing formulas.
//!
//! In temporal logic formulas, much like arithmetic formulas, operators modify the metric values
//! of their inputs to produce a new metric value. All of the operators defined in this module do
//! not produce values by evaluating states, but delegate that evaluation to their operands and
//! instead evaluate the metric trace(s) produced as output. As a result, a formula cannot consist
//! of only operators but must also contain an expression such as a
//! [`Predicate`](crate::predicate::Predicate).
//!
//! In the following sections, we will briefly cover the different types of operators. More
//! information about each operator can be found on its individual documentation page.
//!
//! # First Order Operators
//!
//! Since temporal logics extend first-order logics, temporal logic formulas inherit all the
//! first-order operators, which are:
//!
//!   - [`Not`]
//!   - [`And`]
//!   - [`Or`]
//!   - [`Implies`]
//!
//! Each of these operators is time-invariant, meaning that it does not depend on any other states
//! than the current one. Operators can be _unary_, meaning they take **one** operand, or _binary_,
//! meaning they take **two** operands.
//!
//! # Temporal Operators
//!
//! Temporal operators are not time-invariant, meaning that they depend on states other than the
//! current one.
//!
//! ## Forward Operators
//!
//! Forward operators evaluate states later in the trace than the current time. For a visual
//! representation of this, consider the following trace:
//!
//! ```text
//! T1 T2 T3 T4 T5 T6
//! M1 M2 M3 M4 M5 M6
//! ```
//!
//! For a forward operator considering the value `M2` at time `T2`, a forward operator will
//! consider the interval containing all the times in the future, like so:
//!
//! ```text
//! T1 T2 T3 T4 T5 T6
//! M1 M2 M3 M4 M5 M6
//!     |-----------|
//! ```
//!
//! Then, at time `T3` the operator would consider the following interval:
//!
//! ```text
//! T1 T2 T3 T4 T5 T6
//! M1 M2 M3 M4 M5 M6
//!        |--------|
//! ```
//!
//! Temporal operators also support optional bounds, which can limit the length of the interval
//! that the operators consider. For the same operator bounded in the interval `[0, 3]`, at time
//! `T2` the evaluation would consider use the following interval:
//!
//! ```text
//! T1 T2 T3 T4 T5 T6
//! M1 M2 M3 M4 M5 M6
//!     |--------|
//! ```
//!
//! The most basic forward operator is [`Next`], which requires its sub-formula to hold at the
//! **next** time. From this operator, we can define two more: [`Always`] and [`Eventually`].
//! `Always` requires that its subformula holds at every time instant, and `Eventually` requires
//! that its sub-formula holds at a given instant or at some instant in the future. Finally, a more
//! complex operator is [`Until`], which is a _binary_ operator that requires that its second
//! sub-formula holds at some point during the trace and that its first sub-formula to hold up to
//! and including the instant that the second sub-formula holds.
//!
//! # Examples
//!
//! Creating a first-order logic operator is straight-forward: 
//! 
//! ```rust
//! use banquo::predicate;
//! use banquo::operators::{And, Not};
//!
//! let p1 = predicate!{ x <= 1.0 };
//! let p2 = predicate!{ -1.0 * y <= -3.0 };
//!
//! let f1 = And::new(p1, p2);
//!
//! let p3 = predicate!{ z <= 10.0 };
//!
//! let f2 = Not::new(p3);
//! ```
//!
//! We indicate if a forward operator contains evaluation bounds by selecting the appropriate
//! constructor:
//!
//! ```rust
//! use banquo::predicate;
//! use banquo::operators::Always;
//!
//! let p1 = predicate!{ x <= 1.0 };
//! let f1 = Always::unbounded(p1);
//!
//! let p2 = predicate!{ -1.0 * y <= -3.0 };
//! let f2 = Always::bounded(0.0..=3.0, p2);
//! ```
//!
//! Operators can be combined like so:
//!
//! ```rust
//! use banquo::predicate;
//! use banquo::operators::{Always, And, Not};
//!
//! let p1 = predicate!{ x <= 1.0 };
//! let p2 = predicate!{ -1.0 * y <= -3.0 };
//!
//! let f = Always::unbounded(And::new(Not::new(p1), p2));
//! ```
//!
//! Finally, traces can be evaluated using either [`Formula::evaluate`](crate::Formula) or
//! [`evaluate`](crate::evaluate).
//!
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::{Formula, Trace, evaluate, predicate};
//! use banquo::operators::{Always, And, Not};
//!
//! let p1 = predicate!{ x <= 1.0 };
//! let p2 = predicate!{ -1.0 * y <= -3.0 };
//!
//! let f = Always::unbounded(And::new(Not::new(p1), p2));
//! let states = Trace::from([
//!     (0.0, HashMap::from([("x", 1.0), ("y", 2.0)])),
//!     (1.0, HashMap::from([("x", 1.0), ("y", 2.0)])),
//! ]);
//!
//! let metrics: Trace<f64> = f.evaluate(&states).unwrap();
//! let metric: f64 = evaluate(&states, &f).unwrap();
//! ```
//!
//! # Custom Operators
//!
//! Users can define their own operators by implementing the [`Formula`](crate::Formula) trait for
//! their own types.

mod first_order;
mod forward;

pub use first_order::{And, Implies, Or, Not, BinaryEvaluationError, BinaryOperatorError};
pub use forward::{Always, Eventually, Next, Until, ForwardOperatorError};

mod test {
    use thiserror::Error;

    use crate::Formula;
    use crate::trace::Trace;

    pub struct Const;

    #[derive(Debug, Error)]
    pub enum ConstError {}

    impl<S> Formula<S> for Const
    where
        S: Clone,
    {
        type Metric = S;
        type Error = ConstError;

        fn evaluate(&self, trace: &Trace<S>) -> Result<Trace<Self::Metric>, Self::Error> {
            Ok(trace.clone())
        }
    }

    pub struct ConstLeft;

    impl<L, R> Formula<(L, R)> for ConstLeft
    where
        L: Clone,
    {
        type Metric = L;
        type Error = ConstError;

        fn evaluate(&self, trace: &Trace<(L, R)>) -> Result<Trace<Self::Metric>, Self::Error> {
            let left_trace = trace
                .iter()
                .map(|(time, (left, _))| (time, left.clone()))
                .collect();

            Ok(left_trace)
        }
    }

    pub struct ConstRight;

    impl<L, R> Formula<(L, R)> for ConstRight
    where
        R: Clone,
    {
        type Metric = R;
        type Error = ConstError;

        fn evaluate(&self, trace: &Trace<(L, R)>) -> Result<Trace<Self::Metric>, Self::Error> {
            let right_trace = trace
                .iter()
                .map(|(time, (_, right))| (time, right.clone()))
                .collect();

            Ok(right_trace)
        }
    }
}
