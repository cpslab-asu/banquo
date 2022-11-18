use std::hash::Hash;

use super::predicate::{PolynomialError, Predicate};
use super::{Expression, VariableMap};
use crate::automaton::Automaton;
use crate::formula::{HybridDistance, HybridDistanceFormula};
use crate::trace::Trace;

pub struct HybridPredicate<L> {
    predicate: Predicate,
    location: L,
    automaton: Automaton<L>,
}

impl<L> HybridDistanceFormula<VariableMap, L> for HybridPredicate<L>
where
    L: Copy + Ord + Hash,
{
    type Error = PolynomialError;

    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        let evaluate_time_state = |(time, (state, location)): (usize, &(VariableMap, L))| {
            let distance = if location == &self.location {
                HybridDistance::Robustness(self.predicate.evaluate_state(state)?)
            } else {
                match self.automaton.shortest_path(*location, self.location) {
                    Some(path) => HybridDistance::PathDistance {
                        path_distance: path.length,
                        guard_distance: path.next_guard.min_distance(state)?,
                    },
                    None => HybridDistance::Infinite,
                }
            };

            Ok((time, distance))
        };

        trace.iter().map(evaluate_time_state).collect::<Result<Trace<_>, _>>()
    }
}
