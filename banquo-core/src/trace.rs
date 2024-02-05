//! A set of values where each value is associated with a time.
//!
//! A [`Trace`] is an associative map that represents a sequence of values, where the key for
//! each value is the time when the value was generated. In the context of this library, this data
//! structure is used to represent the state of a system, or its satisfaction metric, over time.
//!
//! # Safety
//!
//! `f64` values are used to represent the time of each state, which do not inherently support
//! `Ord` and `Hash` due to the presence of NaN values, thus making them unsuitable for use
//! as map keys. To work around this issue we ensure that no time value is NaN, which enables the
//! implementation of the missing traits. As a result, using a NaN value as a time value in any
//! method will result in a panic.
//!
//! # Examples
//!
//! An empty `Trace` can be constructed by using the [`Trace::new`] method.
//!
//! ```rust
//! use banquo::Trace;
//! let trace: Trace<f64> = Trace::new();
//! ```
//!
//! Elements can be inserted into the `Trace` by using the [`Trace::insert`] method.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let mut trace: Trace<f64> = Trace::new();
//! trace.insert(0.0, 100.0);
//! trace.insert(1.0, 105.3);
//! trace.insert(2.0, 107.1);
//! ```
//!
//! A `Trace` can also be constructed from an array of known size.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let trace = Trace::from([
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ]);
//! ```
//!
//! Individual states can be accessed either checked or unchecked using [`Trace::at_time`] or [`Index`].
//!
//! ```rust
//! use banquo::Trace;
//!
//! let trace = Trace::from([
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ]);
//!
//! trace.at_time(0.0);  // Some(100.0)
//! trace.at_time(3.0);  // None
//!
//! trace[0.0];      // 100.0
//! // trace[3.0];   // Panic
//! ```
//!
//! A `Trace` can be iterated over using for loops. You can also use the [`IntoIterator`]
//! implementation for `Trace<T>`, `&Trace<T>`, and `&mut Trace<T>` to manually create iterators.
//! Finally, you can iterate over either the times or states in the trace using the [`Trace::times`]
//! and [`Trace::states`] methods.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let mut trace = Trace::from([
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ]);
//!
//! let times = trace.times();
//! let states = trace.states();
//!
//! let iter = trace.iter();
//! let iter = IntoIterator::into_iter(&trace);
//!
//! for (time, state) in &trace {  // (f64, &f64)
//!     // ...
//! }
//!
//! let iter = trace.iter_mut();
//! let iter = IntoIterator::into_iter(&mut trace);
//!
//! for (time, state) in &mut trace {  // (f64, &mut f64)
//!     // ... 
//! }
//!
//! // let iter = trace.into_iter();
//!
//! for (time, state) in trace {  // (f64, f64)
//!     // ... 
//! }
//! ```
//!
//! Traces can be collected from [`Iterator`]s that yield tuple values where the first element can
//! be converted into a `f64`.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let elements = vec![
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ];
//!
//! let trace: Trace<_> = elements
//!     .into_iter()
//!     .map(|(time, state): (f64, f64)| (time, state / 10.0))
//!     .collect();
//! ```
//!
//! Trace iterators support mapping over only the states of a trace while keeping the times the
//! same by using the `map_states` method.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let trace = Trace::from([
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ]);
//!
//! let mapped: Trace<_> = trace
//!     .into_iter()
//!     .map_states(|state: f64| state / 2.0)
//!     .collect();
//! ```
//!
//! Traces also support iterating over sub-intervals using the [`Trace::range`] and
//! [`Trace::range_mut`] methods.
//!
//! ```rust
//! use banquo::Trace;
//!
//! let mut trace = Trace::from([
//!     (0.0, 100.0),
//!     (1.0, 105.3),
//!     (2.0, 107.1),
//! ]);
//!
//! let r1 = trace.range(0.0..2.0);  // Contains elements from times 0.0 <= t < 2.0
//! let r2 = trace.range_mut(0.0..=2.0); // Contains elements from times 0.0 <= t <= 2.0
//! ```
use std::collections::BTreeMap;
use std::ops::{Bound, Index, RangeBounds};

use ordered_float::NotNan;

/// A set of values where each value is associated with a time.
///
/// See the [`trace`](trace) module for more information about the semantics of this type.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Trace<T>(BTreeMap<NotNan<f64>, T>);

impl<T> Default for Trace<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A, T> FromIterator<(A, T)> for Trace<T>
where
    A: Into<f64>,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (A, T)>,
    {
        let elements = iter
            .into_iter()
            .map(|(time, state)| (NotNan::new(time.into()).unwrap(), state))
            .collect();

        Self(elements)
    }
}

impl<A, T, const N: usize> From<[(A, T); N]> for Trace<T>
where
    A: Into<f64>,
{
    #[inline]
    fn from(values: [(A, T); N]) -> Self {
        Self::from_iter(values)
    }
}

/// Create a `Trace` from an array of elements of known length
impl<A, T> From<Vec<(A, T)>> for Trace<T>
where
    A: Into<f64>,
{
    #[inline]
    fn from(values: Vec<(A, T)>) -> Self {
        Self::from_iter(values)
    }
}

impl<T> Index<f64> for Trace<T> {
    type Output = T;

    fn index(&self, index: f64) -> &Self::Output {
        let index = NotNan::new(index).unwrap();
        self.0.index(&index)
    }
}

impl<T> Trace<T> {
    /// Create a new empty trace. Equivalent to [`Trace::default()`]
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    /// Number of elements in the trace
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Determine if the trace contains any elements
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the state for a given time. Returns None if the time is not present in the trace.
    ///
    /// # Safety
    ///
    /// This method will panic if the provided time is NaN
    pub fn at_time(&self, time: f64) -> Option<&T> {
        let key = NotNan::new(time).unwrap();
        self.0.get(&key)
    }

    /// Insert a state for a given time into the trace. Returns the prior state if it exists.
    ///
    /// # Safety
    ///
    /// This method will panic if the provided time is NaN
    pub fn insert(&mut self, time: f64, state: T) -> Option<T> {
        let key = NotNan::new(time).unwrap();
        self.0.insert(key, state)
    }
}

/// Iterator over the times in a trace. The times are yielded in chronological order
/// (lower times -> higher times).
///
/// This iterator can be construced by calling the `times()` method on either a `Trace` or `Range`
/// value.
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let times = trace.times();
///
/// let range = trace.range(0.0..=3.0);
/// let times = range.times();
/// ```
pub struct Times<I>(I);

impl<I, T> Iterator for Times<I>
where
    I: Iterator<Item = (f64, T)>,
{
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|p| p.0)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<I, T> ExactSizeIterator for Times<I>
where
    I: ExactSizeIterator<Item = (f64, T)>,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<I, T> DoubleEndedIterator for Times<I>
where
    I: DoubleEndedIterator<Item = (f64, T)>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|p| p.0)
    }
}

/// Iterator over the states in a trace. The states are yielded in chronological order
/// (lower times -> higher times).
/// This iterator can be construced by calling the `states()` method on either a `Trace` or `Range`
/// value.
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let states = trace.states();
///
/// let range = trace.range(0.0..=3.0);
/// let states = range.states();
/// ```
pub struct States<I>(I);

impl<I, T> Iterator for States<I>
where
    I: Iterator<Item = (f64, T)>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|p| p.1)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<I, T> ExactSizeIterator for States<I>
where
    I: ExactSizeIterator<Item = (f64, T)>,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<I, T> DoubleEndedIterator for States<I>
where
    I: DoubleEndedIterator<Item = (f64, T)>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|p| p.1)
    }
}

/// Iterator that calls a given function for every state in a trace, keeping the times the same.
///
/// This iterator can be construced by calling the `map_states()` method on an `IntoIter`, `Iter`,
/// 'IterMut', or `Range` value.
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let states = trace.states();
///
/// let range = trace.range(0.0..=3.0);
/// let states = range.states();
/// ```
pub struct MapStates<I, F> {
    iter: I,
    f: F,
}

impl<I, F> MapStates<I, F> {
    fn map_element<T, U>(&mut self, (time, state): (f64, T)) -> (f64, U)
    where
        F: FnMut(T) -> U,
    {
        (time, (self.f)(state))
    }
}

impl<I, F, T, U> Iterator for MapStates<I, F>
where
    I: Iterator<Item = (f64, T)>,
    F: FnMut(T) -> U,
{
    type Item = (f64, U);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|e| self.map_element(e))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<I, F, T, U> DoubleEndedIterator for MapStates<I, F>
where
    I: DoubleEndedIterator<Item = (f64, T)>,
    F: FnMut(T) -> U,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|e| self.map_element(e))
    }
}

impl<I, F, T, U> ExactSizeIterator for MapStates<I, F>
where
    I: ExactSizeIterator<Item = (f64, T)>,
    F: FnMut(T) -> U,
{
    fn len(&self) -> usize {
        self.iter.len()
    }
}

/// Borrowing iterator over the (time, state) pairs in a trace. The values are yielded in
/// chronological order (lower times -> higher times).
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (2.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let iter = trace.iter();
///
/// for (time, state) in &trace {
///     // ...
/// }
/// ```
pub struct Iter<'a, T>(std::collections::btree_map::Iter<'a, NotNan<f64>, T>);

impl<'a, T> Iter<'a, T> {
    fn map_element((&time, state): (&'a NotNan<f64>, &'a T)) -> (f64, &'a T) {
        (time.into_inner(), state)
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (f64, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Self::map_element)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Self::map_element)
    }
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, T> Iter<'a, T> {
    /// Create an iterator over the states of the trace, ignoring the times.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let trace = Trace::from([
    ///     (0.0, 'a'),
    ///     (1.0, 'b'),
    ///     (2.0, 'c'),
    ///     (3.0, 'd'),
    ///     (4.0, 'e'),
    ///     (5.0, 'f'),
    /// ]);
    ///
    /// let iter: Vec<&char> = trace.iter().states().collect();
    /// ```
    pub fn states(self) -> States<Self> {
        States(self)
    }

    /// Create an iterator that applies the function `f` to each state of the trace, while keeping
    /// the times the same.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let trace = Trace::from([
    ///     (0.0, "s1".to_string()),
    ///     (1.0, "s2".to_string()),
    ///     (2.0, "s3".to_string()),
    ///     (3.0, "s4".to_string()),
    ///     (4.0, "s5".to_string()),
    ///     (5.0, "s6".to_string()),
    /// ]);
    ///
    /// let iter: Trace<usize> = trace
    ///     .iter()
    ///     .map_states(|state: &String| state.len())
    ///     .collect();
    /// ```
    pub fn map_states<F, U>(self, f: F) -> MapStates<Self, F>
    where
        F: FnMut(&'a T) -> U,
    {
        MapStates { f, iter: self }
    }
}

/// Owning iterator over the (time, state) pairs in a trace. The values are yielded in
/// chronological order (lower times -> higher times).
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (2.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// // let iter = trace.into_iter();
///
/// for (time, state) in trace {
///     // ...
/// }
/// ```
pub struct IntoIter<T>(std::collections::btree_map::IntoIter<NotNan<f64>, T>);

impl<T> IntoIter<T> {
    fn map_element((time, state): (NotNan<f64>, T)) -> (f64, T) {
        (time.into_inner(), state)
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = (f64, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Self::map_element)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Self::map_element)
    }
}

impl<T> ExactSizeIterator for IntoIter<T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> IntoIter<T> {
    /// Create an iterator over the states of the trace, ignoring the times.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let trace = Trace::from([
    ///     (0.0, 'a'),
    ///     (1.0, 'b'),
    ///     (2.0, 'c'),
    ///     (3.0, 'd'),
    ///     (4.0, 'e'),
    ///     (5.0, 'f'),
    /// ]);
    ///
    /// let iter: Vec<char> = trace
    ///     .into_iter()
    ///     .states()
    ///     .collect();
    /// ```
    pub fn states(self) -> States<Self> {
        States(self)
    }

    /// Create an iterator that applies a function `f` to each state of the trace, while keeping the
    /// times the same.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let trace = Trace::from([
    ///     (0.0, 'a'),
    ///     (1.0, 'b'),
    ///     (2.0, 'c'),
    ///     (3.0, 'd'),
    ///     (4.0, 'e'),
    ///     (5.0, 'f'),
    /// ]);
    ///
    /// let iter: Trace<u8> = trace
    ///     .into_iter()
    ///     .map_states(|state: char| state as u8)
    ///     .collect();
    /// ```
    pub fn map_states<F, U>(self, f: F) -> MapStates<Self, F>
    where
        F: FnMut(T) -> U,
    {
        MapStates{ f, iter: self }
    }
}

/// Mutably borrowing iterator over the (time, state) pairs in a trace.
///
/// The values are yielded in chronological order (lower times -> higher times).
///
/// ```rust
/// use banquo::Trace;
///
/// let mut trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (2.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let iter = trace.iter_mut();
///
/// for (time, state) in &mut trace {
///     // ...
/// }
/// ```
pub struct IterMut<'a, T>(std::collections::btree_map::IterMut<'a, NotNan<f64>, T>);

impl<'a, T> IterMut<'a, T> {
    fn map_element((&time, state): (&'a NotNan<f64>, &'a mut T)) -> (f64, &'a mut T) {
        (time.into_inner(), state)
    }
}

impl<'a, T> IterMut<'a, T> {
    /// Create an iterator over the states of the trace, ignoring the times.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let mut trace = Trace::from([
    ///     (0.0, 10),
    ///     (1.0, 20),
    ///     (2.0, 30),
    ///     (3.0, 40),
    ///     (4.0, 50),
    ///     (5.0, 60),
    /// ]);
    ///
    /// trace
    ///     .iter_mut()
    ///     .states()
    ///     .for_each(|state: &mut i32| *state += 5);
    /// ```
    pub fn states(self) -> States<Self> {
        States(self)
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = (f64, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Self::map_element)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for IterMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Self::map_element)
    }
}

impl<'a, T> ExactSizeIterator for IterMut<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

/// Iterator over the (time, &state) pairs of a sub-interval of trace. The values are yielded in
/// chronological order (lower times -> higher times).
///
/// This value can be constructed by calling the `range()` method on a `Trace` value;
///
/// ```rust
/// use banquo::Trace;
///
/// let trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (2.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let range = trace.range(0.0..4.0);
/// let range = trace.range(1.0..=3.0);
/// ```
pub struct Range<'a, T>(std::collections::btree_map::Range<'a, NotNan<f64>, T>);

impl<'a, T> Range<'a, T> {
    fn map_element((&time, state): (&'a NotNan<f64>, &'a T)) -> (f64, &'a T) {
        (time.into_inner(), state)
    }
}

impl<'a, T> Iterator for Range<'a, T> {
    type Item = (f64, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Self::map_element)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for Range<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Self::map_element)
    }
}

impl<'a, T> Range<'a, T> {
    /// Create an iterator over the times of the trace, ignoring the states
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let mut trace = Trace::from([
    ///     (0.0, 'a'),
    ///     (1.0, 'b'),
    ///     (2.0, 'c'),
    ///     (3.0, 'd'),
    ///     (4.0, 'e'),
    ///     (5.0, 'f'),
    /// ]);
    ///
    /// let iter: Vec<f64> = trace
    ///     .range(0.5..=4.5)
    ///     .times()
    ///     .collect();
    /// ```
    pub fn times(self) -> Times<Self> {
        Times(self)
    }

    /// Create an iterator over states of the trace, ignoring the times
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Trace;
    ///
    /// let mut trace = Trace::from([
    ///     (0.0, 'a'),
    ///     (1.0, 'b'),
    ///     (2.0, 'c'),
    ///     (3.0, 'd'),
    ///     (4.0, 'e'),
    ///     (5.0, 'f'),
    /// ]);
    ///
    /// let iter: Vec<&char> = trace
    ///     .range(1.0..=4.0)
    ///     .states()
    ///     .collect();
    /// ```
    pub fn states(self) -> States<Self> {
        States(self)
    }

    pub fn map_states<F, U>(self, f: F) -> MapStates<Self, F>
    where
        F: FnMut(&'a T) -> U,
    {
        MapStates { f, iter: self }
    }
}

/// Iterator over the (time, &mut state) pairs of a sub-interval of trace. The values are yielded in
/// chronological order (lower times -> higher times).
///
/// This value can be constructed by calling the `range_mut()` method on a `Trace` value;
///
/// ```rust
/// use banquo::Trace;
///
/// let mut trace = Trace::from([
///     (0.0, ()),
///     (1.0, ()),
///     (2.0, ()),
///     (3.0, ()),
///     (4.0, ()),
///     (5.0, ()),
/// ]);
///
/// let range = trace.range_mut(0.0..4.0);
/// let range = trace.range_mut(1.0..=3.0);
/// ```
pub struct RangeMut<'a, T>(std::collections::btree_map::RangeMut<'a, NotNan<f64>, T>);

impl<'a, T> RangeMut<'a, T> {
    fn map_element((&time, state): (&'a NotNan<f64>, &'a mut T)) -> (f64, &'a mut T) {
        (time.into_inner(), state)
    }
}

impl<'a, T> Iterator for RangeMut<'a, T> {
    type Item = (f64, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Self::map_element)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for RangeMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Self::map_element)
    }
}

impl<'a, T> RangeMut<'a, T> {
    pub fn times(self) -> Times<Self> {
        Times(self)
    }

    pub fn states(self) -> States<Self> {
        States(self)
    }

    pub fn map_states<F, U>(self, f: F) -> MapStates<Self, F>
    where
        F: FnMut(&'a T) -> U,
    {
        MapStates { f, iter: self }
    }
}

fn convert_bound(bound: Bound<&f64>) -> Bound<NotNan<f64>> {
    match bound {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(&val) => Bound::Included(NotNan::new(val).unwrap()),
        Bound::Excluded(&val) => Bound::Excluded(NotNan::new(val).unwrap()),
    }
}

impl<T> Trace<T> {
    /// Create an iterator yielding (time, &state) values from the trace in chronological order.
    pub fn iter(&self) -> Iter<T> {
        self.into_iter()
    }

    /// Create an iterator yielding (time, &mut state) values from the trace in chronological order.
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut(self.0.iter_mut())
    }

    /// Create an iterator yielding time values from the trace in chronological order.
    pub fn times(&self) -> Times<Iter<T>> {
        Times(self.iter())
    }

    /// Create an iterator yielding &state values from the trace in chronological order.
    pub fn states(&self) -> States<Iter<T>> {
        States(self.iter())
    }

    /// Create an iterator yielding &mut state values from the trace in chronological order.
    pub fn states_mut(&mut self) -> States<IterMut<T>> {
        States(self.iter_mut())
    }

    /// Create an iterator over the (time, &state) values from a sub-interval of the trace. Values
    /// are yielded in chronological order.
    ///
    /// # Safety
    ///
    /// This function panics if either range bound is NaN.
    pub fn range<R>(&self, bounds: R) -> Range<T>
    where
        R: RangeBounds<f64>,
    {
        let start = convert_bound(bounds.start_bound());
        let end = convert_bound(bounds.end_bound());

        Range(self.0.range((start, end)))
    }

    /// Create an iterator over the (time, &mut state) values from a sub-interval of the trace.
    /// Values are yielded in chronological order.
    ///
    /// # Safety
    ///
    /// This function panics if either range bound is NaN.
    pub fn range_mut<R>(&mut self, bounds: R) -> RangeMut<T>
    where
        R: RangeBounds<f64>,
    {
        let start = convert_bound(bounds.start_bound());
        let end = convert_bound(bounds.end_bound());

        RangeMut(self.0.range_mut((start, end)))
    }
}

impl<T> IntoIterator for Trace<T> {
    type Item = (f64, T);
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter())
    }
}

impl<'a, T> IntoIterator for &'a Trace<T> {
    type Item = (f64, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter(self.0.iter())
    }
}

impl<'a, T> IntoIterator for &'a mut Trace<T> {
    type Item = (f64, &'a mut T);
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut(self.0.iter_mut())
    }
}

#[cfg(test)]
mod tests {
    use super::Trace;

    #[test]
    fn get_element() {
        let times = 0..10;
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let trace = Trace::from_iter(times.zip(values));

        assert_eq!(trace.at_time(3.0), Some(&4.0))
    }

    #[test]
    fn times() {
        let trace = Trace::from_iter([
            (1.0, ()),
            (2.0, ()),
            (3.0, ()),
            (4.0, ()),
        ]);

        let mut times = trace.times();

        assert_eq!(times.next(), Some(1.0));
        assert_eq!(times.next(), Some(2.0));
        assert_eq!(times.next(), Some(3.0));
        assert_eq!(times.next(), Some(4.0));
        assert_eq!(times.next(), None);
    }

    #[test]
    fn states() {
        let trace = Trace::from_iter([
            (1.0, 1.0),
            (2.0, 2.0),
            (3.0, 3.0),
            (4.0, 4.0),
        ]);

        let mut states = trace.states();

        assert_eq!(states.next(), Some(&1.0));
        assert_eq!(states.next(), Some(&2.0));
        assert_eq!(states.next(), Some(&3.0));
        assert_eq!(states.next(), Some(&4.0));
        assert_eq!(states.next(), None);
    }

    #[test]
    fn select_range() {
        let times = 0..10;
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let trace = Trace::from_iter(times.zip(values));

        let subtrace_times: Vec<f64> = trace.range(0f64..4.0).times().collect::<Vec<_>>();
        let subtrace_states: Vec<f64> = trace.range(0f64..4.0).states().map(|state| *state).collect::<Vec<f64>>();

        assert_eq!(subtrace_times, vec![0.0, 1.0, 2.0, 3.0]);
        assert_eq!(subtrace_states, vec![1.0, 2.0, 3.0, 4.0]);
    }
}
