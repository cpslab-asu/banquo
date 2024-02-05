use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

use nonempty::NonEmpty;
use petgraph::algo::all_simple_paths;
use petgraph::graphmap::DiGraphMap;

use super::polynomial::Variables;
use super::predicate::{Predicate, PredicateError};
use crate::formulas::Formula;
use crate::metric::{HybridDistance, StateDistance};
use crate::trace::Trace;

/// A set of constraints represeting the conditions necessary to switch states
#[derive(Clone, Debug)]
pub struct Guard {
    constraints: NonEmpty<Predicate>,
}

impl Guard {
    pub fn new<I>(values: I) -> Option<Self>
    where
        I: IntoIterator<Item = Predicate>,
    {
        NonEmpty::collect(values).map(|constraints| Self { constraints })
    }

    /// Compute the minimum distance to any guard in the set.
    pub fn min_distance(&self, state: &Variables) -> Result<f64, PredicateError> {
        let distances = self
            .constraints
            .iter()
            .map(|constraint| constraint.evaluate_state(state))
            .collect::<Result<Vec<_>, _>>();

        let distance = distances?
            .into_iter()
            .reduce(f64::min)
            .expect("At least one constraint must be provided to a guard");

        Ok(distance)
    }
}

struct AutomatonInner<L>
where
    L: Copy + Ord + Hash,
{
    graph: DiGraphMap<L, Guard>,
}

impl<L> Default for AutomatonInner<L>
where
    L: Copy + Ord + Hash,
{
    fn default() -> Self {
        Self {
            graph: DiGraphMap::default(),
        }
    }
}

struct Path<'a> {
    pub states: usize,
    pub first_guard: &'a Guard,
}

struct Paths<'a, L> {
    paths: VecDeque<Vec<L>>,
    graph: &'a DiGraphMap<L, Guard>,
}

impl<'a, L> Iterator for Paths<'a, L>
where
    L: Copy + Ord + Hash,
{
    type Item = Path<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let path = self.paths.pop_front()?;
        let length = path.len() - 1; // Simple path includes start node in node list, so decrease the length by 1
        let first_node = path.get(0)?;
        let second_node = path.get(1)?;
        let guard = self.graph.edge_weight(*first_node, *second_node)?;
        let path = Path {
            states: length,
            first_guard: guard,
        };

        Some(path)
    }
}

impl<L> AutomatonInner<L>
where
    L: Copy + Ord + Hash,
{
    pub fn paths(&self, start: L, end: L) -> Paths<'_, L> {
        Paths {
            paths: all_simple_paths(&self.graph, start, end, 0, None).collect(),
            graph: &self.graph,
        }
    }
}

#[derive(Clone)]
pub struct Automaton<L>
where
    L: Copy + Ord + Hash,
{
    inner: Rc<AutomatonInner<L>>,
}

impl<L> Default for Automaton<L>
where
    L: Copy + Ord + Hash,
{
    fn default() -> Self {
        Self {
            inner: Rc::new(AutomatonInner::default()),
        }
    }
}

impl<L> From<HashMap<(L, L), Guard>> for Automaton<L>
where
    L: Copy + Ord + Hash,
{
    fn from(guard_map: HashMap<(L, L), Guard>) -> Self {
        let graph = guard_map
            .into_iter()
            .map(|(edge, guard)| (edge.0, edge.1, guard))
            .collect();

        Automaton {
            inner: Rc::new(AutomatonInner { graph }),
        }
    }
}

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
pub struct HybridPredicate<L>
where
    L: Copy + Ord + Hash,
{
    predicate: Option<Predicate>,
    location: L,
    automaton: Rc<AutomatonInner<L>>,
}

impl<L> HybridPredicate<L>
where
    L: Copy + Ord + Hash,
{
    pub fn new<P>(predicate: P, location: L, automaton: &Automaton<L>) -> Self
    where
        P: Into<Option<Predicate>>,
    {
        Self {
            predicate: predicate.into(),
            location,
            automaton: Rc::clone(&automaton.inner),
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
    pub variables: Variables,
    pub location: L,
}

impl<L> Formula<HybridState<L>> for HybridPredicate<L>
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
