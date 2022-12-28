use std::error::Error;
use std::rc::Rc;
use std::sync::Arc;

use crate::trace::Trace;

pub trait RobustnessFormula<T> {
    type Error: Error;
    fn robustness(&self, trace: &Trace<T>) -> Result<Trace<f64>, Self::Error>;
}

impl<T, S> RobustnessFormula<S> for &T
where
    T: RobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> RobustnessFormula<S> for Box<T>
where
    T: RobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> RobustnessFormula<S> for Rc<T>
where
    T: RobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> RobustnessFormula<S> for Arc<T>
where
    T: RobustnessFormula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<Trace<f64>, Self::Error> {
        (**self).robustness(trace)
    }
}
