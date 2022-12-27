use std::error::Error;
use std::ops::Neg;
use std::rc::Rc;
use std::sync::Arc;

use crate::trace::Trace;

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

    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error>;
}

impl<S, L, T> HybridDistanceFormula<S, L> for &T
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Box<T>
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Rc<T>
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Arc<T>
where
    T: HybridDistanceFormula<S, L> + ?Sized,
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<Trace<HybridDistance>, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}
