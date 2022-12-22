use std::cmp::Ordering;
use std::error::Error;
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;

use crate::trace::Trace;

pub type Result<T, E> = std::result::Result<Trace<T>, E>;

pub trait Formula<T> {
    type Error: Error;
    fn robustness(&self, trace: &Trace<T>) -> Result<f64, Self::Error>;
}

impl<T, S> Formula<S> for &T
where
    T: Formula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> Formula<S> for Box<T>
where
    T: Formula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> Formula<S> for Rc<T>
where
    T: Formula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        (**self).robustness(trace)
    }
}

impl<T, S> Formula<S> for Arc<T>
where
    T: Formula<S> + ?Sized,
{
    type Error = T::Error;

    fn robustness(&self, trace: &Trace<S>) -> Result<f64, Self::Error> {
        (**self).robustness(trace)
    }
}

pub fn evaluate_robustness<T, F: Formula<T>>(formula: &F, trace: &Trace<T>) -> std::result::Result<f64, F::Error> {
    let robustness = formula.robustness(trace)?;
    let first = robustness.iter().next().unwrap();

    Ok(*first.1)
}

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

pub trait DebugFormula<T> {
    type Error: Error;
    type Prev;
    fn debug_robustness(&self, trace: &Trace<T>) -> Result<DebugRobustness<Self::Prev>, Self::Error>;
}

pub fn evaluate_debug_robustness<F, S>(
    formula: F,
    trace: &Trace<S>,
) -> std::result::Result<DebugRobustness<F::Prev>, F::Error>
where
    F: DebugFormula<S>,
{
    let robustness_trace = formula.debug_robustness(trace)?;
    let (_, debug_robustness) = robustness_trace.into_iter().next().unwrap();

    Ok(debug_robustness)
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub struct PathGuardDistance {
    pub path_distance: usize,
    pub guard_distance: f64,
}

impl PathGuardDistance {
    fn min(self, other: Self) -> Self {
        if self.path_distance < other.path_distance {
            self
        } else if other.path_distance < self.path_distance {
            other
        } else {
            PathGuardDistance {
                path_distance: self.path_distance,
                guard_distance: f64::max(self.guard_distance, other.guard_distance),
            }
        }
    }

    fn max(self, other: Self) -> Self {
        if self.path_distance > other.path_distance {
            self
        } else if other.path_distance > self.path_distance {
            other
        } else {
            PathGuardDistance {
                path_distance: self.path_distance,
                guard_distance: f64::min(self.guard_distance, other.guard_distance),
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum HybridDistance {
    Infinite,
    PathDistance(PathGuardDistance),
    Robustness(f64),
}

impl Neg for HybridDistance {
    type Output = Self;

    fn neg(self) -> Self::Output {
        if let Self::Robustness(r) = self {
            Self::Robustness(-r)
        } else {
            self
        }
    }
}

impl HybridDistance {
    pub fn min(self, other: Self) -> Self {
        match (self, other) {
            (HybridDistance::Infinite, _) => HybridDistance::Infinite,
            (HybridDistance::PathDistance(..), HybridDistance::Infinite) => HybridDistance::Infinite,
            (HybridDistance::PathDistance(d1), HybridDistance::PathDistance(d2)) => {
                HybridDistance::PathDistance(PathGuardDistance::max(d1, d2))
            }
            (HybridDistance::PathDistance(d), HybridDistance::Robustness(..)) => HybridDistance::PathDistance(d),
            (HybridDistance::Robustness(r1), HybridDistance::Robustness(r2)) => {
                HybridDistance::Robustness(f64::min(r1, r2))
            }
            (HybridDistance::Robustness(..), other) => other,
        }
    }

    pub fn max(self, other: Self) -> Self {
        match (self, other) {
            (HybridDistance::Robustness(r1), HybridDistance::Robustness(r2)) => {
                HybridDistance::Robustness(f64::max(r1, r2))
            }
            (HybridDistance::Robustness(r), _) => HybridDistance::Robustness(r),
            (HybridDistance::PathDistance(..), HybridDistance::Robustness(r)) => HybridDistance::Robustness(r),
            (HybridDistance::PathDistance(d1), HybridDistance::PathDistance(d2)) => {
                HybridDistance::PathDistance(PathGuardDistance::min(d1, d2))
            }
            (HybridDistance::PathDistance(d), HybridDistance::Infinite) => HybridDistance::PathDistance(d),
            (HybridDistance::Infinite, HybridDistance::Infinite) => HybridDistance::Infinite,
            (HybridDistance::Infinite, other) => other,
        }
    }
}

pub trait HybridDistanceFormula<S, L> {
    type Error: Error;
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error>;
}

impl<S, L, T> HybridDistanceFormula<S, L> for &T
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Box<T>
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Rc<T>
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

pub fn evaluate_hybrid_distance<F, S, L>(
    formula: F,
    trace: &Trace<(S, L)>,
) -> std::result::Result<HybridDistance, F::Error>
where
    F: HybridDistanceFormula<S, L>,
{
    let hybrid_distance_trace = formula.hybrid_distance(trace)?;
    let (_, hybrid_distance) = hybrid_distance_trace.into_iter().next().unwrap();

    Ok(hybrid_distance)
}
