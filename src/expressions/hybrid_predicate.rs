use std::hash::Hash;

use super::predicate::{PolynomialError, Predicate};
use super::{Expression, VariableMap};
use crate::automaton::{ShortestPath, StatePath};
use crate::formula::{self, HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

#[derive(Clone)]
pub struct HybridPredicate<S, L> {
    predicate: Option<Predicate>,
    automaton: S,
    location: L,
}

impl<S, L> HybridPredicate<S, L> {
    pub fn new<P>(predicate: P, location: L, automaton: S) -> Self
    where
        P: Into<Option<Predicate>>,
        S: StatePath<L>,
    {
        Self {
            predicate: predicate.into(),
            automaton,
            location,
        }
    }
}

fn state_distance(predicate: &Option<Predicate>, state: &VariableMap) -> Result<HybridDistance, PolynomialError> {
    let distance = match predicate {
        Some(predicate) => predicate.evaluate_state(state)?,
        None => f64::INFINITY,
    };

    Ok(HybridDistance::Robustness(distance))
}

fn guard_distance(shortest_path: Option<ShortestPath>, state: &VariableMap) -> Result<HybridDistance, PolynomialError> {
    let distance = match shortest_path {
        Some(path) => path
            .next_guard
            .min_distance(state)
            .map(|guard_distance| HybridDistance::PathDistance {
                path_distance: path.length,
                guard_distance,
            })?,
        None => HybridDistance::Infinite,
    };

    Ok(distance)
}

impl<S, L> HybridDistanceFormula<VariableMap, L> for HybridPredicate<S, L>
where
    S: StatePath<L>,
    L: Copy + Ord + Hash,
{
    type Error = PolynomialError;

    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> formula::Result<HybridDistance, Self::Error> {
        let evaluate_time_state = |(time, (state, location)): (f64, &(VariableMap, L))| {
            let distance = if location == &self.location {
                state_distance(&self.predicate, state)?
            } else {
                guard_distance(self.automaton.shortest_path(*location, self.location), state)?
            };

            Ok((time, distance))
        };

        trace.iter().map(evaluate_time_state).collect::<Result<Trace<_>, _>>()
    }
}
