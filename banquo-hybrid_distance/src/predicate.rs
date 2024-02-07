//! System requirements associated with specific discrete system modes.
//!
//! A [`HybridPredicate`] is a special case a [`Predicate`] where the system under consideration
//! can be represented as a [hybrid automaton], meaning it is composed of a finite number of
//! discrete states where each state may contain continuous-valued variables. In this library we
//! define a **state** as the continuous variables, and a **mode** as the discrete state of the
//! system. The value of a `HybridPredicate` is that it enables marking a system requirement to
//! only be checked when the system is in a specific mode or set of modes, allowing users to
//! check requirements that may not hold across all modes.
//!
//! A `HybridPredicate` is composed of a regular `Predicate` representing the system requirement,
//! as well as a list of modes for which the requirement should be evaluated, and an [`Automaton`]
//! that represents the switching conditions between each mode. When the `HybridPredicate` is
//! evaluated, it results in a value called [`HybridDistance`]. If the system is within one of the
//! specified modes then the the state is evaluated using the inner requirement and the
//! `HybridDistance` is simply the [robustness] value of the requirement. If the system is not in
//! one of the specified modes, then the `Automaton` is used to compute the shortest path from the
//! current mode to any of the specified modes and the length of the path is returned as well as the
//! distance from transitioning to the next mode along the path.
//!
//! [hybrid automaton]: https://en.wikipedia.org/wiki/Hybrid_automaton
//! [robustness]: https://link.springer.com/chapter/10.1007/11940197_12
//!
//! # Examples
//!
//! A `HybridPredicate` can be constructed from an optional inner `Predicate` value, a list of
//! applicable modes, and a reference to the automaton that represents the switching conditions of
//! the system.
//!
//! ```rust
//! use banquo::{HybridPredicate, predicate};
//! use banquo::automaton::Automaton;
//!
//! #[derive(PartialEq, Eq, Hash)]
//! enum Mode {
//!     Heating,
//!     Cooling,
//! }
//!
//! let thermostat = Automaton::from([
//!     // Stop cooling when temp is greater than 15.0c
//!     (Mode::Heating, Mode::Cooling, predicate!{ -1.0 * temp <= -15.0 }),
//!
//!     // Start heating when temp is less than 12.0c
//!     (Mode::Cooling, Mode::Heating, predicate!{ 1.0 * temp <= 12.0 }),
//! ]);
//!
//! // This predicate will be checked for all system states.
//! let all = HybridPredicate::new(predicate!{ temp <= 16.0 }, [], &thermostat);
//!
//! // This predicate will only be checked when the system is in the `Heating` mode.
//! let heating = HybridPredicate::new(predicate!{ 12.0 <= temp }, [Mode::Heating], &thermostat);
//!
//! // This predicate only checks to if the system is in the `Cooling` mode.
//! let cooling = HybridPredicate::new(None, [Mode::Cooling], &thermostat);
//! ```
//!
//! A [`HybridState`] can be evaluated using a `HybridPredicate` into a `HybridDistance` value.
//!
//! ```rust
//! # use banquo::{HybridPredicate, predicate};
//! # use banquo::automaton::Automaton;
//! #
//! # #[derive(PartialEq, Eq, Hash)]
//! # enum Mode {
//! #     Heating,
//! #     Cooling,
//! # }
//! #
//! # let thermostat = Automaton::from([
//! #     // Stop cooling when temp is greater than 15.0c
//! #     (Mode::Heating, Mode::Cooling, predicate!{ -1.0 * temp <= -15.0 }),
//! #
//! #     // Start heating when temp is less than 12.0c
//! #     (Mode::Cooling, Mode::Heating, predicate!{ 1.0 * temp <= 12.0 }),
//! # ]);
//! #
//! # // This predicate will be checked for all system states.
//! # let all = HybridPredicate::new(predicate!{ temp <= 16.0 }, [], &thermostat);
//! #
//! # // This predicate will only be checked when the system is in the `Heating` mode.
//! # let heating = HybridPredicate::new(predicate!{ 12.0 <= temp }, [Mode::Heating], &thermostat);
//! #
//! # // This predicate only checks to if the system is in the `Cooling` mode.
//! # let cooling = HybridPredicate::new(None, [Mode::Cooling], &thermostat);
//! #
//! use std::collections::HashMap;
//!
//! use banquo::HybridState;
//!
//! let state = HybridState {
//!     label: Mode::Cooling,
//!     state: HashMap::from([("temp", 13.4)]),
//! };
//!
//! all.evaluate_state(&state);     // HybridDistance::Robustness(2.6)
//!
//! heating.evaluate_state(&state); // HybridDistance::StateDistance { hops: 1, dist: -1.4 }
//!
//! cooling.evaluate_state(&state);       // HybridDistance::Robustness(f64::INFINITY)
//! ```
//!
//! `HybridPredicate` expressions can be used with [`operators`](banquo_core::operators) to write more
//! complex [`Formula`]s.
//!
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::{HybridPredicate, HybridState, Trace, evaluate, predicate};
//! use banquo::automaton::Automaton;
//! use banquo::operators::Always;
//!
//! #[derive(PartialEq, Eq, Hash)]
//! enum Mode {
//!     Heating,
//!     Cooling,
//! }
//!
//! let thermostat = Automaton::from([
//!     // Stop cooling when temp is greater than 15.0c
//!     (Mode::Heating, Mode::Cooling, predicate!{ -1.0 * temp <= -15.0 }),
//!
//!     // Start heating when temp is less than 12.0c
//!     (Mode::Cooling, Mode::Heating, predicate!{ 1.0 * temp <= 12.0 }),
//! ]);
//!
//! let all = HybridPredicate::new(predicate!{ temp <= 16.0 }, [], &thermostat);
//! let phi = Always::unbounded(all);
//!
//! let trace = Trace::from([
//!     (
//!         1.0,
//!         HybridState {
//!             label: Mode::Cooling,
//!             state: HashMap::from([("temp", 13.4)])
//!         },
//!     ),
//!     (
//!         2.0,
//!         HybridState {
//!             label: Mode::Cooling,
//!             state: HashMap::from([("temp", 12.8)])
//!         },
//!      ),
//!      // ...
//! ]);
//!
//! let result = evaluate(trace, &phi);
//! ```

use banquo_core::{Bottom, Formula, Join, Meet, Top, Trace};
use banquo_core::predicate::{Predicate, VariableSet};
use thiserror::Error;

use crate::automaton::Automaton;

/// A measure of distance over a system represented as a [hybrid automaton].
///
/// _Hybrid distance_ is defined in this [paper] as a generalized metric to support hybrid spaces,
/// that is spaces that have both a discrete and continuous component. This is particularly relevant
/// for hybrid automata, which have several discrete **modes**, each of which may contain a
/// **state** of continuous-valued variables. A `HybridDistance` value contains either a distance
/// metric for a given state, or a quasi-metric representing the "distance" to the target state
/// through the automaton.
///
/// The initial definition of _hybrid distance_ is a 2-tuple with the discrete distance first, and
/// the continuous distance second i.e. `(discrete, continuous)`. However, there are only 3 cases
/// to consider when computing a hybrid distance:
///
///   1. The system is in the desired mode.
///   2. The system is not in the desired mode and a path exists.
///   3. The system is not in the desired mode and a path does not exist.
///
/// The first case is represented in the paper with the discrete distance of zero and the
/// continuous value representing the distance within the mode. This case is represented in this
/// library as the [`HybridDistance::Robustness`] variant which only contains a continuous value
/// since the discrete component is implicitly zero. The second case is represented by a non-zero
/// value for the discrete and any continuous value. This case is represented by the variant
/// [`HybridDistance::StateDistance`], which contains both a discrete and continuous component. The
/// final case is represented in the paper as the tuple `(-inf, -inf)`, which we represent as the
/// variant [`HybridDistance::Unreachable`] which does not contain any values since both are known.
///
/// `HybridDistance` values are produced by evaluating a [`HybridPredicate`] using a
/// [`Trace<HybridState>`]. This confers the following semantics on the discrete and continuous
/// values of the `HybridDistance`:
///
///   1. If the system is in the desired mode, the continuous value is the robustness of the
///      inner [`Predicate`].
///   2. If the system is not in the desired mode and a path exists, the discrete value is the
///      length of the shortest path to reach the desired mode, and the continuous value represents
///      the distance of the current state from transitioning to the next mode.
///
/// [paper]: https://www.public.asu.edu/~gfaineko/pub/emsoft2015.pdf
/// [hybrid automaton]: https://en.wikipedia.org/wiki/Hybrid_automaton
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum HybridDistance {
    /// Robustness of the inner predicate of a [`HybridPredicate`] if the system is in one of the
    /// target modes.
    Robustness(f64),

    /// Distance of the system of switching to a new mode closer to the target mode.
    ///
    /// The `hops` indicates the number transitions in the shortest path to the target mode, while
    /// the `dist` field indicates how far away the system state is from activating the transition
    /// to the next mode in the path.
    ///
    /// The dist value is assumed to negative because a negative value indicates that at least one
    /// of the transition conditions is not satisfied. We assume that the automatons switch ASAP if
    /// the conditions are met, so if the `dist` value were positive it would indicate that all
    /// constraints were satisfied without the system transitioning.
    StateDistance {
        /// Length of the shortest path to the target mode.
        hops: usize,

        /// Distance of the continuous state from transitioning to the next state in the shortest
        /// path to the target label.
        dist: f64,
    },

    /// Infinite distance for when no path from the current mode to the target mode exists.
    Unreachable,
}

impl From<f64> for HybridDistance {
    fn from(value: f64) -> Self {
        HybridDistance::Robustness(value)
    }
}

impl From<(usize, f64)> for HybridDistance {
    fn from((hops, dist): (usize, f64)) -> Self {
        HybridDistance::StateDistance { hops, dist }
    }
}

impl Top for HybridDistance {
    #[inline]
    fn top() -> Self {
        Self::Robustness(f64::INFINITY)
    }
}

impl Bottom for HybridDistance {
    #[inline]
    fn bottom() -> Self {
        Self::Unreachable
    }
}

impl Meet for HybridDistance {
    fn min(&self, other: &Self) -> Self {
        match self {
            // If self is Unreachable, then no matter what the other side is, the min of the two
            // values is Unreachable
            Self::Unreachable => Self::Unreachable,

            Self::StateDistance { hops: lhops, dist: ldist } => match other {
                // If the other is Unreachable then the min of the two is Unreachable
                Self::Unreachable => Self::Unreachable,

                // If the other is a StateDistance, the min is the distance with the greater
                // number of hops, or the more negative guard distance
                Self::StateDistance { hops: rhops, dist: rdist } => {
                    if lhops > rhops {
                        self.clone()
                    } else if lhops < rhops {
                        other.clone()
                    } else {
                        Self::StateDistance { hops: *lhops, dist: f64::min(*ldist, *rdist) }
                    }
                },

                // If the other is a Robustness then the min of the two is a StateDistance
                Self::Robustness(_) => self.clone(),
            },

            // If both values are Robustness then find the min of the two, otherwise, the non-Robustness
            // value is the min
            Self::Robustness(left) => {
                if let Self::Robustness(right) = other {
                    Self::Robustness(f64::min(*left, *right))
                } else {
                    other.clone()
                }
            },
        }
    }
}

impl Join for HybridDistance {
    fn max(&self, other: &Self) -> Self {
        match self {
            // If self is Unreachable, then no matter what the other side is, the max of the two
            // values is the other
            Self::Unreachable => other.clone(),

            Self::StateDistance { hops: lhops, dist: ldist } => match other {
                // If the other is Unreachable then the max of the two is whatever Self is
                Self::Unreachable => self.clone(),

                // If the other is a StateDistance, the max is the distance with the lesser
                // number of hops, or the less negative guard distance
                Self::StateDistance { hops: rhops, dist: rdist } => {
                    if lhops < rhops {
                        self.clone()
                    } else if lhops > rhops {
                        other.clone()
                    } else {
                        Self::StateDistance { hops: *lhops, dist: f64::max(*ldist, *rdist) }
                    }
                },

                // If the other is a Robustness then the max of the two is a Robustness
                Self::Robustness(rho) => Self::Robustness(*rho),
            },

            // If both values are Robustness then find the max of the two, otherwise, the Robustness
            // value is the max
            Self::Robustness(left) => {
                if let Self::Robustness(right) = other {
                    Self::Robustness(f64::max(*left, *right))
                } else {
                    self.clone()
                }
            },
        }
    }
}

/// System requirements associated with specific discrete system modes.
///
/// For more information, see the [module-level](crate::predicate) documentation.
pub struct HybridPredicate<'a, Label> {
    predicate: Option<Predicate>,
    labels: Vec<Label>,
    system: &'a Automaton<Label>,
}

impl<'a, Label> HybridPredicate<'a, Label> {
    /// Create a new hybrid predicate.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::{HybridPredicate, predicate};
    /// use banquo::automaton::{Automaton, Guard};
    ///
    /// #[derive(PartialEq)]
    /// enum State {
    ///     Green,
    ///     Yellow,
    ///     Red,
    /// }
    ///
    /// let automaton = Automaton::from([
    ///     // Green -> Yellow after 15 seconds
    ///     (State::Green, State::Yellow, predicate!{ -1.0 * t <= -15.0 }),
    ///
    ///     // Yellow -> Red after 5 seconds
    ///     (State::Yellow, State::Red, predicate!{ -1.0 * t <= -5.0 }),
    ///
    ///     // Red -> Green after 20 seconds
    ///     (State::Red, State::Green, predicate!{ -1.0 * t <= -20.0 }),
    /// ]);
    ///
    /// // This predicate will be checked for all system states.
    /// let all = HybridPredicate::new(predicate!{ t <= 20.0 }, [], &automaton);
    ///
    /// // This predicate will only be checked when the system is in the Green state.
    /// // When the system is not in the Green state, this evaluates to the system distance from
    /// // the Green state
    /// let green = HybridPredicate::new(predicate!{ t <= 15.0 }, [State::Green], &automaton);
    ///
    /// // This predicate has no requirement, and will only check if the system is in the target state.
    /// let state = HybridPredicate::new(None, [State::Red], &automaton);
    /// ```
    pub fn new<P, L>(predicate: P, labels: L, system: &'a Automaton<Label>) -> Self
    where
        P: Into<Option<Predicate>>,
        L: IntoIterator<Item = Label>,
    {
        Self {
            system,
            predicate: predicate.into(),
            labels: Vec::from_iter(labels),
        }
    }

    /// Returns a reference to the inner predicate if it exists.
    pub fn predicate(&self) -> Option<&Predicate> {
        self.predicate.as_ref()
    }

    /// Returns a reference to the mode labels for which the inner predicate should be evaluated
    pub fn labels(&self) -> &[Label] {
        self.labels.as_slice()
    }

    /// Returns a reference to the automaton representing the system switching dynamics.
    pub fn system(&self) -> &'a Automaton<Label> {
        self.system
    }
}

/// A continuous system state associated with a discrete label.
///
/// When constructing a `HybridState` value, the state `field` should contain the continuous-valued
/// variables of the system, while the `label` value should contain the uniquely identifying value
/// for a discrete location in the [`Automaton`]. 
pub struct HybridState<State, Label> {
    /// The continuous-valued values of the system
    pub state: State,

    /// Value uniquely identifying a discrete location of the system
    pub label: Label,
}

/// Error produced by evaluating a [`HybridState`] using a [`HybridPredicate`].
///
/// The primary source of this error is the [`HybridPredicate::evaluate_state`] method. This error
/// can represent one of three following occurences:
///
///   1. An error occured evaluating the inner predicate
///   2. An [`Automaton`] transition [`Guard`](crate::automaton::Guard) contains no constraints.
///   3. No `Guard` distance is negative.
///
/// Values of this type can be queried for its cause, as well as the variable name causing the
/// error. 
#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("Error evaluating inner predicate: {0}")]
    PredicateError(#[from] banquo_core::predicate::EvaluationError),

    #[error("Transition guard cannot have 0 constraints")]
    EmptyConstraints,

    #[error("No constraint returned a negative value")]
    NoNegativeValues,
}

/// Transparent wrapper around [`ErrorKind`].
///
/// This error type can be created when calling [`HybridPredicate::evaluate_state`] if an error is
/// encountered when evaluating the `HybridState` value.
#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error(transparent)]
pub struct EvaluationError(ErrorKind);

impl From<banquo_core::predicate::EvaluationError> for EvaluationError {
    fn from(error: banquo_core::predicate::EvaluationError) -> Self {
        Self(ErrorKind::from(error))
    }
}

impl From<ErrorKind> for EvaluationError {
    fn from(kind: ErrorKind) -> Self {
        Self(kind)
    }
}

impl EvaluationError {
    /// Returns a reference to the type of error that occured.
    pub fn kind(&self) -> &ErrorKind {
        &self.0
    }
}

impl<'a, Label> HybridPredicate<'a, Label>
where
    Label: PartialEq,
{
    pub fn evaluate_state<State>(&self, hybrid_state: &HybridState<State, Label>) -> Result<HybridDistance, EvaluationError>
    where
        State: VariableSet
    {
        // Compute the robustness for the continuous state if the state label is one of the target
        // labels or if no labels are provided
        if self.labels.contains(&hybrid_state.label) ||self.labels.is_empty() {
            let rho = match &self.predicate {
                Some(predicate) => predicate.evaluate_state(&hybrid_state.state)?,
                None => f64::top(),
            };

            return Ok(HybridDistance::from(rho));
        } 

        let mut min_dist = HybridDistance::bottom();

        for target in &self.labels {
            if let Some(path) = self.system.shortest_path(&hybrid_state.label, target) {
                let label_dists = path.next_guard.distances(&hybrid_state.state)?;

                if label_dists.is_empty() {
                    return Err(EvaluationError::from(ErrorKind::EmptyConstraints));
                }

                // Find the smallest distance to satisfying one of the unsatisfied guard
                // constraints.
                // If a constraint is satisfied, its distance is 0.0, regardless of
                // actual robustness metric. This is consistent with the original implementation of
                // the approach in dp_taliro.
                let max_neg_dist = label_dists
                    .into_iter()
                    .map(|dist| if dist > 0.0 { 0.0 } else { dist })
                    .fold(f64::NEG_INFINITY, |max, dist| f64::max(max, dist));
                
                // Keep the distance to the closest label
                min_dist = min_dist.max(&HybridDistance::StateDistance { hops: path.hops, dist: max_neg_dist });
            }
        }

        Ok(min_dist)
    }
}

/// Error produced during the evaluation of a [`Trace<HybridState>`] using a [`HybridPredicate`].
#[derive(Debug, Clone, Error)]
#[error("At time {time} encountered error evaluation hybrid predicate: {error}")]
pub struct FormulaError {
    time: f64,

    #[source]
    error: EvaluationError,
}

impl FormulaError {
    /// Create a new evaluation error for a given time.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::hybrid_predicate::{ErrorKind, EvaluationError, FormulaError};
    ///
    /// let err = FormulaError::new(1.0, EvaluationError::from(ErrorKind::EmptyConstraints));
    /// ```
    pub fn new(time: f64, error: EvaluationError) -> Self {
        Self { time, error }
    }

    /// Returns the time of the state that produced the [`EvaluationError`].
    pub fn time(&self) -> f64 {
        self.time
    }

    /// Returns a reference to the [`EvaluationError`] that was generated.
    pub fn error(&self) -> &EvaluationError {
        &self.error
    }
}

impl<'a, State, Label> Formula<HybridState<State, Label>> for HybridPredicate<'a, Label>
where
    State: VariableSet,
    Label: PartialEq,
{
    type Metric = HybridDistance;
    type Error = FormulaError;

    fn evaluate(&self, trace: &Trace<HybridState<State, Label>>) -> Result<Trace<Self::Metric>, Self::Error> {
        trace
            .iter()
            .map(|(time, hybrid_state)| {
                self.evaluate_state(hybrid_state)
                    .map(|dist| (time, dist))
                    .map_err(|error| FormulaError { time, error })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use banquo_core::{Join, Meet, predicate};

    use crate::{automaton::Automaton, HybridState};
    use super::{HybridDistance, HybridPredicate};

    #[test]
    fn meet() {
        let r1 = HybridDistance::Robustness(1.0);
        let r2 = HybridDistance::Robustness(2.0);

        let sd1 = HybridDistance::StateDistance { hops: 1, dist: -3.0 };
        let sd2 = HybridDistance::StateDistance { hops: 2, dist: -1.0 };
        let sd3 = HybridDistance::StateDistance { hops: 1, dist: -4.0 };

        let u = HybridDistance::Unreachable;

        assert_eq!(r1.min(&r2), r1);
        assert_eq!(r1.min(&sd1), sd1);
        assert_eq!(r1.min(&u), u);

        assert_eq!(sd1.min(&r1), sd1);
        assert_eq!(sd1.min(&sd2), sd2);
        assert_eq!(sd1.min(&sd3), sd3);
        assert_eq!(sd1.min(&u), u);

        assert_eq!(u.min(&r1), u);
        assert_eq!(u.min(&sd1), u);
        assert_eq!(u.min(&u), u);
    }

    #[test]
    fn join() {
        let r1 = HybridDistance::Robustness(1.0);
        let r2 = HybridDistance::Robustness(2.0);

        let sd1 = HybridDistance::StateDistance { hops: 1, dist: -3.0 };
        let sd2 = HybridDistance::StateDistance { hops: 2, dist: -1.0 };
        let sd3 = HybridDistance::StateDistance { hops: 1, dist: -4.0 };

        let u = HybridDistance::Unreachable;

        assert_eq!(r1.max(&r2), r2);
        assert_eq!(r1.max(&sd1), r1);
        assert_eq!(r1.max(&u), r1);

        assert_eq!(sd1.max(&r1), r1);
        assert_eq!(sd1.max(&sd2), sd1);
        assert_eq!(sd1.max(&sd3), sd1);
        assert_eq!(sd1.max(&u), sd1);

        assert_eq!(u.max(&r1), r1);
        assert_eq!(u.max(&sd1), sd1);
        assert_eq!(u.max(&u), u);
    }

    #[test]
    fn evaluate_state() {
        #[derive(PartialEq, Eq)]
        enum State {
            Green,
            Yellow,
            Red,
        }

        let automaton = Automaton::from([
            // Green -> Yellow after 15 seconds
            (State::Green, State::Yellow, predicate!{ -1.0 * t <= -15.0 }),
        
            // Yellow -> Red after 5 seconds
            (State::Yellow, State::Red, predicate!{ -1.0 * t <= -5.0 }),
        
            // Red -> Green after 20 seconds
            (State::Red, State::Green, predicate!{ -1.0 * t <= -20.0 }),
        ]);

        let g = HybridState {
            label: State::Green,
            state: HashMap::from([("t", 11.0)]),
        };

        let y = HybridState {
            label: State::Yellow,
            state: HashMap::from([("t", 2.0)]),
        };

        let r = HybridState {
            label: State::Red,
            state: HashMap::from([("t", 18.0)]),
        };
        
        // This predicate will be checked for all system states.
        let all = HybridPredicate::new(predicate!{ t <= 10.0 }, [], &automaton);

        assert_eq!(all.evaluate_state(&g).unwrap(), HybridDistance::Robustness(-1.0));
        assert_eq!(all.evaluate_state(&y).unwrap(), HybridDistance::Robustness(8.0));
        assert_eq!(all.evaluate_state(&r).unwrap(), HybridDistance::Robustness(-8.0));
        
        // This predicate will only be checked when the system is in the Green state.
        // When the system is not in the Green state, this evaluates to the system distance from
        // the Green state
        let green = HybridPredicate::new(predicate!{ t <= 15.0 }, [State::Green], &automaton);

        assert_eq!(green.evaluate_state(&g).unwrap(), HybridDistance::Robustness(4.0));
        assert_eq!(green.evaluate_state(&y).unwrap(), HybridDistance::StateDistance { hops: 2, dist: -3.0 });
        assert_eq!(green.evaluate_state(&r).unwrap(), HybridDistance::StateDistance { hops: 1, dist: -2.0 });
        
        // This predicate has no requirement, and will only check if the system is in the target state.
        let state = HybridPredicate::new(None, [State::Red], &automaton);

        assert_eq!(state.evaluate_state(&g).unwrap(), HybridDistance::StateDistance { hops: 2, dist: -4.0 });
        assert_eq!(state.evaluate_state(&y).unwrap(), HybridDistance::StateDistance { hops: 1, dist: -3.0 });
        assert_eq!(state.evaluate_state(&r).unwrap(), HybridDistance::Robustness(f64::INFINITY));

        let bad = HybridState {
            label: State::Yellow,
            state: HashMap::from([("t", 6.0)]),
        };

        assert_eq!(state.evaluate_state(&bad).unwrap(), HybridDistance::StateDistance { hops: 1, dist: 0.0 });
    }
}
