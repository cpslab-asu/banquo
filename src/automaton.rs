use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use petgraph::algo::astar;
use petgraph::graphmap::DiGraphMap;

use crate::expressions::{Predicate, PredicateError};

#[derive(Clone, Debug)]
pub struct Guard {
    constraints: Vec<Predicate>,
}

impl Guard {
    pub fn min_distance<K>(&self, state: &HashMap<K, f64>) -> Result<f64, PredicateError>
    where
        K: Eq + Hash + Borrow<str>,
    {
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

impl FromIterator<Predicate> for Guard {
    fn from_iter<T: IntoIterator<Item = Predicate>>(iter: T) -> Self {
        Self {
            constraints: Vec::from_iter(iter),
        }
    }
}

#[derive(Clone)]
pub struct Automaton<L> {
    state_graph: DiGraphMap<L, Guard>,
}

impl<L> Default for Automaton<L>
where
    L: Copy + Ord + Hash,
{
    fn default() -> Self {
        Self {
            state_graph: DiGraphMap::default(),
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

        Automaton { state_graph: graph }
    }
}

pub struct ShortestPath<'a> {
    pub length: usize,
    pub next_guard: &'a Guard,
}

pub trait StatePath<L> {
    fn shortest_path(&self, start: L, end: L) -> Option<ShortestPath>;
}

impl<L> StatePath<L> for Automaton<L>
where
    L: Copy + Ord + Hash,
{
    fn shortest_path(&self, start: L, end: L) -> Option<ShortestPath> {
        if start == end {
            return None;
        }

        let (length, path_nodes) = astar(&self.state_graph, start, |node| node == end, |_| 1, |_| 1)?;
        let next_guard = self.state_graph.edge_weight(path_nodes[0], path_nodes[1])?;
        let path = ShortestPath { length, next_guard };

        Some(path)
    }
}

impl<L> StatePath<L> for &Automaton<L>
where
    L: Copy + Ord + Hash,
{
    #[inline]
    fn shortest_path(&self, start: L, end: L) -> Option<ShortestPath> {
        (**self).shortest_path(start, end)
    }
}
