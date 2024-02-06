use std::collections::HashMap;

use banquo_core::predicate::Predicate;
use petgraph::algo::FloatMeasure;
use petgraph::graphmap::DiGraphMap;

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
pub struct Automaton<Label>(DiGraphMap<Label, Guard>);

impl<Label> From<HashMap<(Label, Label), Guard>> for Automaton<Label> {
    fn from(value: HashMap<(Label, Label), Guard>) -> Self {
        Self(DiGraphMap::from(value))
    }
}

pub struct ShortestPath<'a> {
    pub hops: usize,
    pub next_guard: &'a Guard,
}

pub struct Paths<'a>(&'a ());

impl<Label> Automaton<Label> {
    pub fn shortest_path<Start, End>(&self, start: Start, end: End) -> ShortestPath<'_>
    where
        Start: ToOwned<Owned = Label>,
        End: ToOwned<Owned = Label>,
    {
        unimplemented!()
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
