//! A finite-state automaton with continuous-valued transition guards.
//!
//! In this library, an [`Automaton`] is a set of discrete locations, with edges between each
//! location representing transitions. Each discrete system location can contain 0 or more
//! continuous variables. This type of mixed automaton is called a _[hybrid automaton]_. Each
//! transition contains a [`Guard`], which is a set of constraints represented as inequalities,
//! that can evaluate the continuous system variables to determine if a transition should be taken.
//! This library assumes that as soon as the guard for a transition is satisfied, the transition
//! will be taken immediately. Note that this library only provides the faculties for determining
//! the transition distances from a given location and is not capable of simulating the system
//! dynamics.
//!
//! This library uses the term **mode** to refer to the discrete locations of the automaton, and
//! the term **state** to refer to the continuous-valued variables of the system. In addition, the
//! type variable `Label` in some type definitions refers to the type of the values used to
//! unqiuely label each mode.
//!
//! [hybrid automaton]: https://en.wikipedia.org/wiki/Hybrid_automaton
//!
//! # Example
//!
//! An [`Automaton`] value is created out of an _adjacency map_, which is a mapping that represents
//! the edges between states, along with the [`Guard`] for each edge. A `Guard` can be constructed
//! out of `1` or more [`Predicate`] values. A `Guard` cannot be constructed with `0` predicates
//! because it would then represent an unconditional transition, which would violate the ASAP
//! transition assumption. The following is an example of an `Automaton` which represents a
//! simple thermostat controller.
//!
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::predicate;
//! use banquo::automaton::Automaton;
//!
//! #[derive(PartialEq, Eq, Hash)]
//! enum Mode {
//!     Heating,
//!     Cooling,
//! }
//!
//! let adjacency_map = HashMap::from([
//!     // Stop cooling when temp is greater than 15.0c
//!     ((Mode::Heating, Mode::Cooling), predicate!{ -1.0 * temp <= -15.0 }),
//!
//!     // Start heating when temp is less than 12.0c
//!     ((Mode::Cooling, Mode::Heating), predicate!{ 1.0 * temp <= 12.0 }),
//! ]);
//!
//! let automaton = Automaton::from(adjacency_map);
//! ```

use std::borrow::Borrow;
use std::collections::HashMap;

use banquo_core::predicate::{EvaluationError, Predicate, VariableSet};
use petgraph::algo::astar;
use petgraph::graph::DiGraph;

/// A set of inequalities representing the conditions necessary to change system states.
///
/// A `Guard` must contain `1` or more [`Predicate`] constraints. Creating a `Guard` with `0`
/// constraints will result in a [`panic`].
///
/// # Examples
///
/// You can create a `Guard` directly from a predicate.
///
/// ```rust
/// use banquo::predicate;
/// use banquo::automaton::Guard;
///
/// let g = Guard::from(predicate!{ x <= 1.0 });
/// ```
///
/// You can also create a guard from a either a [`Vec<Predicate>`] or an [`[Predicate;
/// N]`](prim@array).
///
/// ```rust
/// # use banquo::predicate;
/// # use banquo::automaton::Guard;
/// #
/// let g1 = Guard::from([
///     predicate!{ x <= 1.0 },
///     predicate!{ y <= 3.1 },
/// ]);
///
/// let g2 = Guard::from(
///     vec![predicate!{ x <= 1.0 }, predicate! { y <= 3.1 }]
/// );
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Guard(Vec<Predicate>);

impl FromIterator<Predicate> for Guard {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Predicate>,
    {
        Self(Vec::from_iter(iter))
    }
}

impl From<Predicate> for Guard {
    fn from(constraint: Predicate) -> Self {
        Self(vec![constraint])
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

/// Iterator over references to the constraints of a [`Guard`].
///
/// This struct is created by the [`Guard::iter`] function.
#[derive(Debug)]
pub struct Iter<'a>(std::slice::Iter<'a, Predicate>);

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Predicate;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> IntoIterator for &'a Guard {
    type IntoIter = Iter<'a>;
    type Item = &'a Predicate;

    fn into_iter(self) -> Self::IntoIter {
        Iter(self.0.iter())
    }
}

impl Guard {
    /// Consume the `Guard` returning a list of [`Predicate`] constraints.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::{Predicate, predicate};
    /// use banquo::automaton::Guard;
    ///
    /// let guard = Guard::from(predicate!{ x <= 1.0 });
    /// let constraints: Vec<Predicate> = guard.into_inner();
    /// ```
    pub fn into_inner(self) -> Vec<Predicate> {
        self.0
    }

    /// Return an iterator over the constraints of the `Guard`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::collections::HashMap;
    ///
    /// use banquo::predicate;
    /// use banquo::automaton::Guard;
    ///
    /// let guard = Guard::from([predicate!{ x <= 1.0 }, predicate!{ y <= -3.5 }]);
    /// let constraints = guard.iter();
    /// ```
    pub fn iter(&self) -> Iter<'_> {
        Iter(self.0.iter())
    }

    /// Compute the distance of a state from satisfying each constraint in the `Guard`.
    ///
    /// This function returns an error if any of the constraints generates an error while
    /// evaluating the state.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::collections::HashMap;
    ///
    /// use banquo::predicate;
    /// use banquo::automaton::Guard;
    ///
    /// let guard = Guard::from([predicate!{ x <= 1.0 }, predicate!{ y <= 3.0 }]);
    /// let state = HashMap::from([("x", 0.5), ("y", 4.2)]);
    ///
    /// guard.distances(&state); // vec![0.5, -1.2]
    /// ```
    pub fn distances<S>(&self, state: &S) -> Result<Vec<f64>, EvaluationError>
    where
        S: VariableSet,
    {
        self.0
            .iter()
            .map(|constraint| constraint.evaluate_state(state))
            .collect()
    }
}

/// A finite-state automaton with continuous-valued state variables and continuous-valued transition
/// guards.
///
/// An [`Automaton`] value is created out of an _adjacency map_, which is a mapping that represents
/// the edges between states, along with the [`Guard`] for each edge.
///
/// See the [module-level](self) documentation for mode information.
///
/// # Examples
///
/// You can construct an `Automaton` from a [`HashMap`] like so:
///
/// ```rust
/// use std::collections::HashMap;
///
/// use banquo::predicate;
/// use banquo::automaton::Automaton;
///
/// #[derive(PartialEq, Eq, Hash)]
/// enum Mode {
///     Heating,
///     Cooling,
/// }
///
/// let adjacency_map = HashMap::from([
///     // Stop cooling when temp is greater than 15.0c
///     ((Mode::Heating, Mode::Cooling), predicate!{ -1.0 * temp <= -15.0 }),
///
///     // Start heating when temp is less than 12.0c
///     ((Mode::Cooling, Mode::Heating), predicate!{ 1.0 * temp <= 12.0 }),
/// ]);
///
/// let automaton = Automaton::from(adjacency_map);
/// ```
///
/// You can also use an array of 3-tuples, like so:
///
/// ```rust
/// # use banquo::predicate;
/// # use banquo::automaton::Automaton;
/// #
/// # #[derive(PartialEq, Eq, Hash)]
/// # enum Mode {
/// #     Heating,
/// #     Cooling,
/// # }
/// #
/// let automaton = Automaton::from([
///     // Stop cooling when temp is greater than 15.0c
///     (Mode::Heating, Mode::Cooling, predicate!{ -1.0 * temp <= -15.0 }),
///
///     // Start heating when temp is less than 12.0c
///     (Mode::Cooling, Mode::Heating, predicate!{ 1.0 * temp <= 12.0 }),
/// ]);
/// ```
#[derive(Debug, Clone)]
pub struct Automaton<Label>(DiGraph<Label, Guard>);

impl<Label, G> FromIterator<(Label, Label, G)> for Automaton<Label>
where
    Label: PartialEq,
    G: Into<Guard>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Label, Label, G)>,
    {
        let mut graph = DiGraph::default();

        for (start, end, guard) in iter {
            // If the start label already exists in the graph retrieve its index, otherwise create
            // a new node
            let start_idx = graph
                .node_indices()
                .find(|&idx| graph[idx] == start)
                .unwrap_or_else(|| graph.add_node(start));

            // If the end label already exists in the graph retrieve its index, otherwise create
            // a new node
            let end_idx = graph
                .node_indices()
                .find(|&idx| graph[idx] == end)
                .unwrap_or_else(|| graph.add_node(end));

            // Add the edge between start and end nodes containing the transition guards
            graph.add_edge(start_idx, end_idx, guard.into());
        }

        Self(graph)
    }
}

impl<Label, G> From<HashMap<(Label, Label), G>> for Automaton<Label>
where
    Label: PartialEq,
    G: Into<Guard>,
{
    fn from(adjmap: HashMap<(Label, Label), G>) -> Self {
        let iter = adjmap.into_iter().map(|(edge, guard)| (edge.0, edge.1, guard));

        Self::from_iter(iter)
    }
}

impl<Label, G, const N: usize> From<[(Label, Label, G); N]> for Automaton<Label>
where
    Label: PartialEq,
    G: Into<Guard>,
{
    fn from(edges: [(Label, Label, G); N]) -> Self {
        Self::from_iter(edges)
    }
}

/// A struct containing the length of the shortest path from one state to another, along with the
/// next guard in the path.
///
/// This struct is created using [`Automaton::shortest_path`].
pub struct ShortestPath<'a> {
    /// Length of the shortest path from the start state to the end state.
    pub hops: usize,

    /// The set of constraints restricting the transition between the start state and the next state
    /// in the shortest path.
    pub next_guard: &'a Guard,
}

impl<Label> Automaton<Label>
where
    Label: PartialEq,
{
    /// Compute the shortest path from the `start` state to the `end` state.
    ///
    /// This function accepts any value that can be borrowed as a `&Label`. This method returns the
    /// length of the shortest path, along with the edge [`Guard`] restricting the transition from
    /// the start state to the next state in the shortest path. If no path exists, then this
    /// function returns [`None`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate;
    /// use banquo::automaton::{Automaton, Guard};
    ///
    /// #[derive(PartialEq, Eq, Hash)]
    /// enum Mode {
    ///     Heating,
    ///     Cooling,
    /// }
    ///
    /// let thermostat = Automaton::from([
    ///     // Stop cooling when temp is greater than 15.0c
    ///     (Mode::Heating, Mode::Cooling, predicate!{ -1.0 * temp <= -15.0 }),
    ///
    ///     // Start heating when temp is less than 12.0c
    ///     (Mode::Cooling, Mode::Heating, predicate!{ 1.0 * temp <= 12.0 }),
    /// ]);
    ///
    /// let path = thermostat.shortest_path(Mode::Heating, Mode::Cooling).unwrap();
    ///
    /// assert_eq!(path.hops, 1);
    /// assert_eq!(path.next_guard, &Guard::from(predicate!{ -1.0 * temp <= -15.0 }));
    /// ```
    pub fn shortest_path<Start, End>(&self, start: Start, end: End) -> Option<ShortestPath<'_>>
    where
        Start: Borrow<Label>,
        End: Borrow<Label>,
    {
        let start_label: &Label = start.borrow();
        let end_label: &Label = end.borrow();

        // Determine node index that contains start label
        let start_idx = self.0.node_indices().find(|&idx| &self.0[idx] == start_label)?;

        // Determine the shortest path from start node to the node that contains the end label
        // Using 0 as the heuristic makes this equivalent to Dijkstra's algorithm
        let (hops, path) = astar(&self.0, start_idx, |idx| &self.0[idx] == end_label, |_| 1, |_| 0)?;

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
