use std::collections::HashMap;
use std::hash::Hash;
use std::{borrow::Borrow, collections::VecDeque};

use petgraph::algo::all_simple_paths;
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

pub struct Path<'a> {
    pub states: usize,
    pub first_guard: &'a Guard,
}

pub struct Paths<'a, L> {
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
        let length = path.len();
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

impl<L> Automaton<L>
where
    L: Copy + Ord + Hash,
{
    pub fn paths(&self, start: L, end: L) -> Paths<'_, L> {
        Paths {
            paths: all_simple_paths(&self.state_graph, start, end, 0, None).collect(),
            graph: &self.state_graph,
        }
    }
}
