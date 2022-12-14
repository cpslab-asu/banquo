use std::hash::Hash;

use super::predicate::{PolynomialError, Predicate};
use super::{Expression, VariableMap};
use crate::automaton::Automaton;
use crate::formula::{HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

#[derive(Clone)]
pub struct HybridPredicate<L> {
    predicate: Option<Predicate>,
    location: L,
    automaton: Automaton<L>,
}

impl<L> HybridPredicate<L> {
    pub fn new<P>(predicate: P, location: L, automaton: Automaton<L>) -> Self
    where
        P: Into<Option<Predicate>>
    {
        Self {
            predicate: predicate.into(),
            location,
            automaton,
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

fn guard_distance<L>(automaton: &Automaton<L>, current: L, target: L, state: &VariableMap) -> Result<HybridDistance, PolynomialError>
where
    L: Copy + Ord + Hash,
{
    let shortest_path = automaton.shortest_path(current, target);

    let distance = match shortest_path {
        Some(path) => path
            .next_guard
            .min_distance(state)
            .map(|guard_distance| HybridDistance::PathDistance { path_distance: path.length, guard_distance })?,
        None => HybridDistance::Infinite,
    };

    Ok(distance)
}

impl<L> HybridDistanceFormula<VariableMap, L> for HybridPredicate<L>
where
    L: Copy + Ord + Hash,
{
    type Error = PolynomialError;

    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        let evaluate_time_state = |(time, (state, location)): (usize, &(VariableMap, L))| {
            let distance = if location == &self.location {
                state_distance(&self.predicate, state)?
            } else {
                guard_distance(&self.automaton,*location, self.location, state)?
            };

            Ok((time, distance))
        };

        trace.iter().map(evaluate_time_state).collect::<Result<Trace<_>, _>>()
    }
}
