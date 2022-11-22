use std::collections::HashMap;
use std::hash::Hash;

use petgraph::algo::astar;
use petgraph::graphmap::DiGraphMap;

use crate::expressions::{Expression, PolynomialError, Predicate};

#[derive(Clone, Debug)]
pub struct Guard {
    constraints: Vec<Predicate>,
}

impl Guard {
    pub fn min_distance(&self, state: &HashMap<String, f64>) -> Result<f64, PolynomialError> {
        let distances = self
            .constraints
            .iter()
            .map(|constraint| constraint.evaluate_state(state))
            .collect::<Result<Vec<_>, _>>();

        let distance = distances?
            .into_iter()
            .reduce(|d1, d2| f64::min(d1, d2))
            .expect("At least one constraint must be provided to a guard");

        Ok(distance)
    }
}

#[derive(Clone)]
pub struct Automaton<L> {
    state_graph: DiGraphMap<L, Guard>,
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

impl<L> Automaton<L>
where
    L: Copy + Ord + Hash,
{
    pub fn shortest_path(&self, start: L, end: L) -> Option<ShortestPath> {
        if start == end {
            return None;
        }

        let (length, path_nodes) = astar(&self.state_graph, start, |node| node == end, |_| 1, |_| 1)?;
        let next_guard = self.state_graph.edge_weight(path_nodes[0], path_nodes[1])?;
        let path = ShortestPath { length, next_guard };

        Some(path)
    }
}
