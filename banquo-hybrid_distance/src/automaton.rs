use std::collections::HashMap;

use banquo_core::predicate::Predicate;
use petgraph::algo::astar;
use petgraph::graph::DiGraph;

/// A set of constraints represeting the conditions necessary to switch states
#[derive(Debug, Clone)]
pub struct Guard(Vec<Predicate>);

impl FromIterator<Predicate> for Guard {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Predicate>,
    {
        Self(Vec::from_iter(iter))
    }
}

impl<const N: usize> From<[Predicate; N]> for Guard {
    fn from(constraints: [Predicate; N]) -> Self {
        Self(Vec::from(constraints))
    }
}

impl From<Vec<Predicate>> for Guard {
    fn from(constraints: Vec<Predicate>) -> Self {
        Self(constraints)
    }
}

#[derive(Debug)]
pub struct Constraints<'a>(std::slice::Iter<'a, Predicate>);

impl<'a> Iterator for Constraints<'a> {
    type Item = &'a Predicate;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Guard {
    pub fn constraints(&self) -> Constraints<'_> {
        Constraints(self.0.iter())
    }
}

#[derive(Debug, Clone)]
pub struct Automaton<Label>(DiGraph<Label, Guard>);

impl<Label> From<HashMap<(Label, Label), Guard>> for Automaton<Label>
where
    Label: PartialEq,
{
    fn from(adjmap: HashMap<(Label, Label), Guard>) -> Self {
        let mut graph = DiGraph::default();

        for ((start, end), guard) in adjmap {
            let start_idx = graph
                .node_indices()
                .find(|&idx| &graph[idx] == &start)
                .unwrap_or_else(|| graph.add_node(start));

            let end_idx = graph
                .node_indices()
                .find(|&idx| &graph[idx] == &end)
                .unwrap_or_else(|| graph.add_node(end));

            graph.add_edge(start_idx, end_idx, guard);
        }

        Self(graph)
    }
}

pub struct ShortestPath<'a> {
    pub hops: usize,
    pub next_guard: &'a Guard,
}

impl<Label> Automaton<Label> {
    pub fn shortest_path(&self, start: &Label, end: &Label) -> Option<ShortestPath<'_>>
    where
        Label: PartialEq,
    {
        // Determine node index that contains start label
        let start_idx = self.0
            .node_indices()
            .find(|&idx| &self.0[idx] == start)?;

        // Determine the shortest path from start node to the node that contains the end label
        let (hops, path) = astar(&self.0, start_idx, |idx| &self.0[idx] == end, |_| 1, |_| 0)?;

        // First node in returned path is the start node, so take the second node
        let end_idx = path.get(1).copied()?;
        
        // Get the index for the edge given the node endpoint indices
        let edge_idx = self.0.find_edge(start_idx, end_idx)?;

        // Get the guard for the first edge in the shortest path
        let next_guard = self.0.edge_weight(edge_idx)?;

        Some(ShortestPath { hops, next_guard })
    }
}

/*

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
*/
