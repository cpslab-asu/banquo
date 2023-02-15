use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use super::polynomial::Variables;
use super::predicate::{Predicate, PredicateError};
use crate::automaton::{Automaton, Path};
use crate::formulas::Formula;
use crate::metric::{HybridDistance, StateDistance};
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

fn state_distance(predicate: &Option<Predicate>, state: &Variables) -> PredicateResult<HybridDistance> {
    let distance = match predicate {
        Some(predicate) => predicate.evaluate_state(state)?,
        None => f64::INFINITY,
    };

    Ok(HybridDistance::from(distance))
}

fn min_guard_dist<'a, P>(paths: P, init: Path<'a>, state: &Variables) -> PredicateResult<HybridDistance>
where
    P: Iterator<Item = Path<'a>>,
{
    let mut min_dist = StateDistance {
        discrete: init.states,
        continuous: init.first_guard.min_distance(state)?,
    };

    for path in paths {
        if path.states > min_dist.discrete {
            continue;
        }

        let guard_dist = path.first_guard.min_distance(state)?;

        if path.states < min_dist.discrete {
            min_dist.discrete = path.states;
            min_dist.continuous = guard_dist;
        } else {
            min_dist.continuous = f64::min(min_dist.continuous, guard_dist);
        }
    }

    Ok(HybridDistance::from(min_dist))
}

fn guard_distance<'a, P>(mut paths: P, state: &Variables) -> PredicateResult<HybridDistance>
where
    P: Iterator<Item = Path<'a>>,
{
    paths
        .next()
        .map(|path| min_guard_dist(paths, path, state))
        .unwrap_or_else(|| Ok(HybridDistance::unreachable()))
}

pub struct HybridState<L> {
    variables: Variables,
    location: L,
}

impl<'a, L> Formula<HybridState<L>> for HybridPredicate<'a, L>
where
    L: Copy + Hash + Ord,
{
    type Metric = HybridDistance;
    type Error = HybridPredicateError;

    fn evaluate_trace(&self, trace: &Trace<HybridState<L>>) -> Result<Trace<Self::Metric>, Self::Error> {
        let evaluate_time_state = |(time, state): (f64, &HybridState<L>)| {
            let distance = if state.location == self.location {
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
