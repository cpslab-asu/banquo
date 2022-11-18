use std::collections::BTreeMap;
use std::ops::{Index, RangeBounds};
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Trace<T> {
    elements: BTreeMap<usize, T>,
}

impl<T> Trace<T> {
    pub fn new(elements: BTreeMap<usize, T>) -> Self {
        Self { elements }
    }

    pub fn zip<U>(self, mut trace: Trace<U>) -> Trace<(T, U)> {
        let elements = self
            .elements
            .into_iter()
            .filter_map(|(time, s1)| trace.elements.remove(&time).map(|s2| (time, (s1, s2))))
            .collect();

        Trace { elements }
    }

    pub fn get(&self, time: &usize) -> Option<&T> {
        self.elements.get(time)
    }

    pub fn insert_state(&mut self, time: usize, state: T) -> Option<T> {
        self.elements.insert(time, state)
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

impl<T> Trace<T> {
    pub fn range<R>(&self, bounds: R) -> Trace<&T>
    where
        R: RangeBounds<usize>,
    {
        let elements = self
            .elements
            .range(bounds)
            .map(|(time, state)| (*time, state))
            .collect();

        Trace { elements }
    }
}

pub struct Times<'a, T> {
    times: std::collections::btree_map::Keys<'a, usize, T>,
}

impl<'a, T> Iterator for Times<'a, T> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        self.times.next().cloned()
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
    states: std::collections::btree_map::Values<'a, usize, T>,
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

impl<T> Index<&usize> for Trace<T> {
    type Output = T;

    fn index(&self, index: &usize) -> &Self::Output {
        self.elements.index(index)
    }
}

impl<T> Index<usize> for Trace<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.elements.index(&index)
    }
}

pub struct Iter<'a, T> {
    values: std::collections::btree_map::Iter<'a, usize, T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (usize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next().map(|(time, state)| (*time, state))
    }
}

impl<'a, T> IntoIterator for &'a Trace<T> {
    type Item = (usize, &'a T);
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
    values: std::collections::btree_map::IntoIter<usize, T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = (usize, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.values.next()
    }
}

impl<T> IntoIterator for Trace<T> {
    type Item = (usize, T);
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            values: self.elements.into_iter(),
        }
    }
}

impl<T> FromIterator<(usize, T)> for Trace<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (usize, T)>,
    {
        Self {
            elements: BTreeMap::from_iter(iter),
        }
    }
}

impl<'a, T> FromIterator<(&'a usize, T)> for Trace<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (&'a usize, T)>,
    {
        let elements = iter.into_iter().map(|(time, state)| (*time, state)).collect();

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

#[cfg(test)]
mod tests {
    use super::Trace;

    #[test]
    fn get_element() {
        let times = 0..10;
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let trace = Trace::from_iter(times.zip(values));

        assert_eq!(trace.get(&3), Some(&4.0))
    }

    #[test]
    fn select_range() {
        let times = 0..10;
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
        let trace = Trace::from_iter(times.zip(values));
        let subtrace = trace.range(0..4);

        let subtrace_times: Vec<usize> = subtrace.times().collect::<Vec<_>>();
        let subtrace_states: Vec<f64> = subtrace.states().map(|state| **state).collect::<Vec<_>>();

        assert_eq!(subtrace_times, vec![0, 1, 2, 3]);
        assert_eq!(subtrace_states, vec![1.0, 2.0, 3.0, 4.0]);
    }
}
