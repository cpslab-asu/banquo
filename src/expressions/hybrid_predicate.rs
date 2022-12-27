use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use super::predicate::{PolynomialError, Predicate};
use super::{Expression, VariableMap};
use crate::automaton::{ShortestPath, StatePath};
use crate::formulas::{HybridDistance, HybridDistanceFormula, PathGuardDistance};
use crate::trace::Trace;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    GuardError,
    PredicateError,
}

#[derive(Debug)]
pub struct HybridPredicateError {
    kind: ErrorKind,
    inner: PolynomialError,
    time: f64,
}

impl Display for HybridPredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_source = match self.kind {
            ErrorKind::GuardError => "guard",
            ErrorKind::PredicateError => "predicate",
        };

        write!(
            f,
            "error {} encountered at time {} while evaluating {}",
            &self.inner, &self.time, error_source
        )
    }
}

impl Error for HybridPredicateError {}

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
    let into_path_distance = |path: ShortestPath| -> Result<HybridDistance, PolynomialError> {
        let guard_distance = path.next_guard.min_distance(state)?;
        let distance = PathGuardDistance {
            path_distance: path.length,
            guard_distance,
        };

        Ok(HybridDistance::PathDistance(distance))
    };

    shortest_path
        .map(into_path_distance)
        .unwrap_or(Ok(HybridDistance::Infinite))
}

impl<S, L> HybridDistanceFormula<VariableMap, L> for HybridPredicate<S, L>
where
    S: StatePath<L>,
    L: Copy + Ord + Hash,
{
    type Error = HybridPredicateError;

    fn hybrid_distance(&self, trace: &Trace<(VariableMap, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        let evaluate_time_state = |(time, (state, location)): (f64, &(VariableMap, L))| {
            let distance = if location == &self.location {
                state_distance(&self.predicate, state).map_err(|inner| HybridPredicateError {
                    inner,
                    time,
                    kind: ErrorKind::PredicateError,
                })
            } else {
                guard_distance(self.automaton.shortest_path(*location, self.location), state).map_err(|inner| {
                    HybridPredicateError {
                        inner,
                        time,
                        kind: ErrorKind::GuardError,
                    }
                })
            };

            Ok((time, distance?))
        };

        trace.iter().map(evaluate_time_state).collect::<Result<Trace<_>, _>>()
    }
}
