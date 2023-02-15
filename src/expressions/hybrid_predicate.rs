use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use super::polynomial::VarMap;
use super::predicate::{Predicate, PredicateError};
use crate::automaton::{Automaton, Path};
use crate::formulas::{Formula, HybridDistance, PathGuardDistance};
use crate::trace::Trace;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    GuardError,
    PredicateError,
}

#[derive(Debug)]
pub struct HybridPredicateError {
    kind: ErrorKind,
    inner: PredicateError,
    time: f64,
}

impl Display for HybridPredicateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let error_source = match self.kind {
            ErrorKind::GuardError => "guard",
            ErrorKind::PredicateError => "predicate",
        };

        write!(f, "error {} at time {} from {}", &self.inner, &self.time, error_source)
    }
}

impl Error for HybridPredicateError {}

#[derive(Clone)]
pub struct HybridPredicate<'a, L> {
    predicate: Option<Predicate>,
    automaton: &'a Automaton<L>,
    location: L,
}

impl<'a, L> HybridPredicate<'a, L> {
    pub fn new<P>(predicate: P, location: L, automaton: &'a Automaton<L>) -> Self
    where
        P: Into<Option<Predicate>>,
    {
        Self {
            predicate: predicate.into(),
            automaton,
            location,
        }
    }
}

type PredicateResult<T> = Result<T, PredicateError>;

fn state_distance<V>(predicate: &Option<Predicate>, state: &V) -> PredicateResult<HybridDistance>
where
    V: VarMap,
{
    let distance = match predicate {
        Some(predicate) => predicate.evaluate_state(state)?,
        None => f64::INFINITY,
    };

    Ok(HybridDistance::Robustness(distance))
}

fn min_guard_dist<'a, P, V>(mut paths: P, init: Path<'a>, state: &V) -> PredicateResult<HybridDistance>
where
    P: Iterator<Item = Path<'a>>,
    V: VarMap,
{
    let mut min_dist = PathGuardDistance {
        path_distance: init.states,
        guard_distance: init.first_guard.min_distance(state)?,
    };

    while let Some(path) = paths.next() {
        if path.states > min_dist.path_distance {
            continue;
        }

        let path_min_guard_dist = path.first_guard.min_distance(state)?;

        if path.states < min_dist.path_distance {
            min_dist.path_distance = path.states;
            min_dist.guard_distance = path_min_guard_dist;
        } else if path.states == min_dist.path_distance {
            min_dist.guard_distance = f64::min(min_dist.guard_distance, path_min_guard_dist);
        }
    }

    Ok(HybridDistance::PathDistance(min_dist))
}

fn guard_distance<'a, P, V>(mut paths: P, state: &V) -> PredicateResult<HybridDistance>
where
    P: Iterator<Item = Path<'a>>,
    V: VarMap,
{
    paths
        .next()
        .map(|path| min_guard_dist(paths, path, state))
        .unwrap_or(Ok(HybridDistance::Infinite))
}

pub struct HybridState<S, L> {
    variables: S,
    location: L,
}

impl<'a, S, L> Formula<HybridState<S, L>> for HybridPredicate<'a, L>
where
    S: VarMap,
    L: Copy + Hash + Ord,
{
    type Metric = HybridDistance;
    type Error = HybridPredicateError;

    fn evaluate_trace(&self, trace: &Trace<HybridState<S, L>>) -> Result<Trace<Self::Metric>, Self::Error> {
        let evaluate_time_state = |(time, state): (f64, &HybridState<S, L>)| {
            let distance = if &state.location == &self.location {
                state_distance(&self.predicate, &state.variables).map_err(|inner| HybridPredicateError {
                    inner,
                    time,
                    kind: ErrorKind::PredicateError,
                })
            } else {
                guard_distance(self.automaton.paths(state.location, self.location), &state.variables).map_err(|inner| {
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
