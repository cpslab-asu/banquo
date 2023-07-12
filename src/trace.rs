use std::collections::BTreeMap;
use std::ops::{Bound, Index, RangeBounds};
use std::rc::Rc;

use ordered_float::NotNan;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Trace<T> {
    elements: BTreeMap<NotNan<f64>, T>,
}

impl<T> Trace<T> {
    pub fn new() -> Self {
        Self {
            elements: BTreeMap::new(),
        }
    }

    pub fn zip<U>(self, mut trace: Trace<U>) -> Trace<(T, U)> {
        let elements = self
            .elements
            .into_iter()
            .filter_map(|(time, s1)| trace.elements.remove(&time).map(|s2| (time, (s1, s2))))
            .collect();

        Trace { elements }
    }

    pub fn get(&self, time: &f64) -> Option<&T> {
        let key = NotNan::new(*time).unwrap();
        self.elements.get(&key)
    }

    pub fn insert(&mut self, time: f64, state: T) -> Option<T> {
        let key = NotNan::new(time).unwrap();
        self.elements.insert(key, state)
    }

    pub fn into_shared(self) -> Trace<Rc<T>> {
        let elements = self
            .elements
            .into_iter()
            .map(|(time, state)| (time, Rc::new(state)))
            .collect();

        Trace { elements }
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
    pub fn range<R>(&self, bounds: R) -> Trace<&T>
    where
        R: RangeBounds<f64>,
    {
        let start = convert_bound(bounds.start_bound());
        let end = convert_bound(bounds.end_bound());

        let elements = self
            .elements
            .range((start, end))
            .map(|(time, state)| (*time, state))
            .collect();

        Trace { elements }
    }
}

pub struct Times<'a, T> {
    times: std::collections::btree_map::Keys<'a, NotNan<f64>, T>,
}

impl<'a, T> Iterator for Times<'a, T> {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        self.times.next().cloned().map(NotNan::into_inner)
    }
}

impl<T> Trace<T> {
    pub fn times(&self) -> Times<T> {
        Times {
            times: self.elements.keys(),
        }
    }
}

pub struct States<'a, T> {
    states: std::collections::btree_map::Values<'a, NotNan<f64>, T>,
}

impl<'a, T> Iterator for States<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.states.next()
    }
}

impl<T> Trace<T> {
    pub fn states(&self) -> States<T> {
        States {
            states: self.elements.values(),
        }
    }
}

impl<T> Index<&f64> for Trace<T> {
    type Output = T;

    fn index(&self, index: &f64) -> &Self::Output {
        let index = NotNan::new(*index).unwrap();
        self.elements.index(&index)
    }
}

impl<T> Index<f64> for Trace<T> {
    type Output = T;

    fn index(&self, index: f64) -> &Self::Output {
        let index = NotNan::new(index).unwrap();
        self.elements.index(&index)
    }
}

pub struct MapStates<I, F> {
    inner: I,
    func: F,
}

impl<I, K, T1, F, T2> Iterator for MapStates<I, F>
where
    I: Iterator<Item = (K, T1)>,
    F: Fn(T1) -> T2,
{
    type Item = (K, T2);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(key, value)| (key, (self.func)(value)))
    }
}

pub struct Iter<'a, T> {
    values: std::collections::btree_map::Iter<'a, NotNan<f64>, T>,
}

impl<'a, T> Iter<'a, T> {
    pub fn map_states<F, U>(self, func: F) -> MapStates<Self, F>
    where
        F: Fn(T) -> U,
    {
        MapStates { inner: self, func }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (f64, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|(time, state)| (time.into_inner(), state))
    }
}

impl<'a, T> IntoIterator for &'a Trace<T> {
    type Item = (f64, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            values: self.elements.iter(),
        }
    }
}

impl<T> Trace<T> {
    pub fn iter(&self) -> Iter<T> {
        self.into_iter()
    }
}

pub struct IntoIter<T> {
    values: std::collections::btree_map::IntoIter<NotNan<f64>, T>,
}

impl<T> IntoIter<T> {
    pub fn map_states<F, U>(self, func: F) -> MapStates<Self, F>
    where
        F: Fn(T) -> U,
    {
        MapStates { inner: self, func }
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = (f64, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|(time, state)| (time.into_inner(), state))
    }
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.values.next_back().map(|(time, state)| (time.into_inner(), state))
    }
}

impl<T> IntoIterator for Trace<T> {
    type Item = (f64, T);
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            values: self.elements.into_iter(),
        }
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

        Self { elements }
    }
}

impl<T> Default for Trace<T> {
    fn default() -> Self {
        Self {
            elements: BTreeMap::default(),
        }
    }
}

impl<T> AsRef<Trace<T>> for Trace<T> {
    fn as_ref(&self) -> &Trace<T> {
        self
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

        assert_eq!(trace.get(&3.0), Some(&4.0))
    }

    #[test]
    fn select_range() {
        let times = 0..10;
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let trace = Trace::from_iter(times.zip(values));
        let subtrace = trace.range(0f64..4.0);

        let subtrace_times: Vec<f64> = subtrace.times().collect::<Vec<_>>();
        let subtrace_states: Vec<f64> = subtrace.states().map(|state| **state).collect::<Vec<f64>>();

        assert_eq!(subtrace_times, vec![0.0, 1.0, 2.0, 3.0]);
        assert_eq!(subtrace_states, vec![1.0, 2.0, 3.0, 4.0]);
    }
}
