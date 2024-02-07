use banquo_core::{Bottom, Formula, Join, Meet, Top, Trace};
use banquo_core::predicate::{Predicate, PredicateError, VariableSet};
use thiserror::Error;

use crate::automaton::Automaton;

/// A measure of distance over a system represented as a [hybrid automaton].
///
/// [hybrid automaton]: https://en.wikipedia.org/wiki/Hybrid_automaton
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum HybridDistance {
    /// Robustness of the inner predicate of a [`HybridPredicate`] if the system is in one of the
    /// target states of the `HybridPredicate`.
    Robustness(f64),

    /// Distance of the system of switching to a new state closer to the target state.
    ///
    /// The `hops` indicates the number transitions in the shortest path to the target state, while
    /// the `dist` field indicates how far away the system state is from activating the transition
    /// to the next state in the path.
    ///
    /// The dist value is assumed to negative because a negative value indicates that at least one
    /// of the transition conditions is not satisfied. We assume that the automatons switch ASAP if
    /// the conditions are met, so if the `dist` value were positive it would indicate that all
    /// constraints were satisfied without the system transitioning.
    StateDistance {
        /// Length of the shortest path to the target label.
        hops: usize,

        /// Distance of the continuous state from transitioning to the next state in the shortest
        /// path to the target label.
        dist: f64,
    },

    /// Infinite distance for when no path from the current state to any of the [`HybridPredicate`]
    /// target states exists.
    Unreachable,
}

impl From<f64> for HybridDistance {
    fn from(value: f64) -> Self {
        HybridDistance::Robustness(value)
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
}

/// A continuous system state associated with a discrete label.
pub struct HybridState<State, Label> {
    /// The continuous-valued part of the system state
    pub state: State,

    /// The discrete location of the system
    pub label: Label,
}

/// Error produced during the evaluation of a [`HybridState`] using a [`HybridPredicate`].
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum HybridEvaluationError {
    #[error("Error evaluating inner predicate: {0}")]
    PredicateError(#[from] PredicateError),

    #[error("Transition guard cannot have 0 constraints")]
    EmptyConstraints,

    #[error("No constraint returned a negative value")]
    NoNegativeValues,
}

impl<'a, Label> HybridPredicate<'a, Label>
where
    Label: PartialEq,
{
    pub fn evaluate_state<State>(&self, hybrid_state: &HybridState<State, Label>) -> Result<HybridDistance, HybridEvaluationError>
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
                    return Err(HybridEvaluationError::EmptyConstraints);
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

/// Error produced during the evaluation of a [`Trace`] using a [`HybridPredicate`].
#[derive(Debug, Clone, Error)]
#[error("At time {time} encountered error evaluation hybrid predicate: {error}")]
pub struct HybridFormulaError {
    pub time: f64,

    #[source]
    pub error: HybridEvaluationError,
}

impl<'a, State, Label> Formula<HybridState<State, Label>> for HybridPredicate<'a, Label>
where
    State: VariableSet,
    Label: PartialEq,
{
    type Metric = HybridDistance;
    type Error = HybridFormulaError;

    fn evaluate(&self, trace: &Trace<HybridState<State, Label>>) -> Result<Trace<Self::Metric>, Self::Error> {
        trace
            .iter()
            .map(|(time, hybrid_state)| {
                self.evaluate_state(hybrid_state)
                    .map(|dist| (time, dist))
                    .map_err(|error| HybridFormulaError { time, error })
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
