use std::cmp::Ordering;
use std::error::Error;
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;

use crate::trace::Trace;

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct DebugRobustness<T> {
    pub robustness: f64,
    pub previous: T,
}

impl<T> Neg for DebugRobustness<T> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        DebugRobustness {
            robustness: -self.robustness,
            previous: self.previous,
        }
    }
}

impl<T> PartialOrd for DebugRobustness<T>
where
    T: PartialEq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        f64::partial_cmp(&self.robustness, &other.robustness)
    }
}

impl<T> DebugRobustness<T>
where
    T: PartialEq,
{
    fn is_nan(&self) -> bool {
        self.robustness.is_nan()
    }

    pub fn min(self, other: Self) -> Self {
        match self.partial_cmp(&other) {
            None => {
                if self.is_nan() {
                    other
                } else {
                    self
                }
            }
            Some(ordering) => match ordering {
                Ordering::Less | Ordering::Equal => self,
                Ordering::Greater => other,
            },
        }
    }

    pub fn max(self, other: Self) -> Self {
        match self.partial_cmp(&other) {
            None => {
                if self.is_nan() {
                    other
                } else {
                    self
                }
            }
            Some(ordering) => match ordering {
                Ordering::Greater | Ordering::Equal => self,
                Ordering::Less => other,
            },
        }
    }
}

pub trait DebugRobustnessFormula<T> {
    type Error: Error;
    type Prev;

    fn debug_robustness(&self, trace: &Trace<T>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error>;
}

impl<S, T> DebugRobustnessFormula<S> for &T
where
    T: DebugRobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;
    type Prev = T::Prev;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        (**self).debug_robustness(trace)
    }
}

impl<S, T> DebugRobustnessFormula<S> for Box<T>
where
    T: DebugRobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;
    type Prev = T::Prev;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        (**self).debug_robustness(trace)
    }
}

impl<S, T> DebugRobustnessFormula<S> for Rc<T>
where
    T: DebugRobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;
    type Prev = T::Prev;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        (**self).debug_robustness(trace)
    }
}

impl<S, T> DebugRobustnessFormula<S> for Arc<T>
where
    T: DebugRobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;
    type Prev = T::Prev;

    fn debug_robustness(&self, trace: &Trace<S>) -> Result<Trace<DebugRobustness<Self::Prev>>, Self::Error> {
        (**self).debug_robustness(trace)
    }
}
