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

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum HybridDistance {
    Infinite,
    PathDistance { path_distance: usize, guard_distance: f64 },
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

impl PartialOrd for HybridDistance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (HybridDistance::Robustness(r1), HybridDistance::Robustness(r2)) => f64::partial_cmp(r1, r2),
            (HybridDistance::Robustness(..), _) => Some(Ordering::Less),
            (HybridDistance::PathDistance { .. }, HybridDistance::Robustness(..)) => Some(Ordering::Greater),
            (
                HybridDistance::PathDistance {
                    path_distance: p1,
                    guard_distance: g1,
                },
                HybridDistance::PathDistance {
                    path_distance: p2,
                    guard_distance: g2,
                },
            ) => {
                let location_ordering = usize::cmp(p1, p2);

                if location_ordering == Ordering::Equal {
                    f64::partial_cmp(g1, g2)
                } else {
                    Some(location_ordering)
                }
            }
            (HybridDistance::PathDistance { .. }, HybridDistance::Infinite) => Some(Ordering::Less),
            (HybridDistance::Infinite, HybridDistance::Infinite) => Some(Ordering::Equal),
            (HybridDistance::Infinite, _) => Some(Ordering::Greater),
        }
    }
}

impl HybridDistance {
    fn is_nan(&self) -> bool {
        match self {
            Self::Infinite => false,
            Self::PathDistance { guard_distance, .. } => guard_distance.is_nan(),
            Self::Robustness(r) => r.is_nan(),
        }
    }

    fn concat<F: Fn(Ordering) -> Self>(self, other: Self, f: F) -> Self {
        match self.partial_cmp(&other) {
            None => {
                if self.is_nan() {
                    other
                } else {
                    self
                }
            }
            Some(ordering) => f(ordering),
        }
    }

    pub fn min(self, other: Self) -> Self {
        let f = |ordering: Ordering| match ordering {
            Ordering::Less | Ordering::Equal => self,
            Ordering::Greater => other,
        };

        self.concat(other, f)
    }

    pub fn max(self, other: Self) -> Self {
        let f = |ordering: Ordering| match ordering {
            Ordering::Greater | Ordering::Equal => self,
            Ordering::Less => other,
        };

        self.concat(other, f)
    }
}

pub trait HybridDistanceFormula<S, L> {
    type Error: Error;
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error>;
}

impl<S, L, T> HybridDistanceFormula<S, L> for &T
where
    T: HybridDistanceFormula<S, L> + ?Sized
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Box<T> 
where
    T: HybridDistanceFormula<S, L> + ?Sized
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}

impl<S, L, T> HybridDistanceFormula<S, L> for Rc<T> 
where
    T: HybridDistanceFormula<S, L> + ?Sized
{
    type Error = T::Error;

    #[inline]
    fn hybrid_distance(&self, trace: &Trace<(S, L)>) -> Result<HybridDistance, Self::Error> {
        (**self).hybrid_distance(trace)
    }
}