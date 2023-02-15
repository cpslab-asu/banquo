use std::{cmp::Ordering, ops::Neg};

/// Robustness generalization to support hybrid automata.
///
/// Hybrid distance is composed of two distance components. The discrete distance represents how
/// far away the system is from a target state in terms of edges. The continuous distance has
/// different meanings depending on the value of the discrete distance. For a discrete distance of
/// zero, meaning the system is in the target state, the continuous value represents the robustness
/// as defined for non-hybrid formulas. For non-zero discrete distances, the continuous distance
/// represents how far the system is from one of the transition [Guard]s.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct StateDistance {
    pub discrete: usize,
    pub continuous: f64,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct HybridDistance(Option<StateDistance>);

impl HybridDistance {
    pub fn unreachable() -> Self {
        Self(None)
    }
}

impl From<f64> for HybridDistance {
    fn from(value: f64) -> Self {
        let distance = StateDistance {
            discrete: 0,
            continuous: value,
        };

        Self(Some(distance))
    }
}

impl From<(usize, f64)> for HybridDistance {
    fn from(value: (usize, f64)) -> Self {
        let distance = StateDistance {
            discrete: value.0,
            continuous: value.1,
        };

        Self(Some(distance))
    }
}

impl From<StateDistance> for HybridDistance {
    fn from(value: StateDistance) -> Self {
        Self(Some(value))
    }
}

impl From<Option<StateDistance>> for HybridDistance {
    fn from(value: Option<StateDistance>) -> Self {
        Self(value)
    }
}

fn neg_state_dist(state_dist: StateDistance) -> StateDistance {
    if state_dist.continuous == 0.0 {
        return state_dist;
    }

    StateDistance {
        discrete: state_dist.discrete,
        continuous: -state_dist.continuous,
    }
}

/// Negation operator for HybridDistance
///
/// Since the discrete component of the hybrid distance is represented using an unsigned integer
/// value, this implementation only negates the continuous component, and it is only negated when
/// the discrete distance is zero (when the continuous component represents robustness).
impl Neg for HybridDistance {
    type Output = Self;

    fn neg(self) -> Self::Output {
        HybridDistance(self.0.map(neg_state_dist))
    }
}

/// Trait representing the binary operator that computes the greatest lower bound of two values.
///
/// In general, this equates to the miniumum of the two values, but this behavior is not guaranteed.
/// When provided along with a partial ordering, the type forms a Meet-Semilattice.
pub trait Meet<Other = Self> {
    /// Compute the greatest lower bound of self and other
    fn meet(self, other: Other) -> Self;
}

/// Meet trait implementation for f64 by value
///
/// This implementation tries to propogate NaN values as much as possible so that users can be
/// alerted to any issues during computation. If either of the values being compared are NaN values
/// then the result of the Meet operator will also be NaN.
impl Meet for f64 {
    fn meet(self, other: Self) -> Self {
        if self.is_nan() || other.is_nan() {
            f64::NAN
        } else {
            f64::min(self, other)
        }
    }
}

impl Meet<&Self> for f64 {
    fn meet(self, other: &Self) -> Self {
        self.meet(*other)
    }
}

/// [Meet] trait implementation for StateDistance by value
///
/// This implementation considers larger values of the discrete distance to be lesser. This
/// behavior originates from the initial description of Hybrid Distance where the discrete distance
/// is negative to support minimization. Since we use usize to represent the discrete distance,
/// which cannot be negative, we must take the max instead of the min to replicate the original
/// behavior. To compare the continuous components, we delegate to the [Meet] implementation for
/// f64 to compute the minimum.
impl Meet for StateDistance {
    fn meet(self, other: Self) -> Self {
        match self.discrete.cmp(&other.discrete) {
            Ordering::Greater => self,
            Ordering::Less => other,
            Ordering::Equal => StateDistance {
                discrete: self.discrete,
                continuous: self.continuous.meet(other.continuous),
            },
        }
    }
}

/// [Meet] trait implementation for HybridDistance by values
///
/// In the original description of the HybridDistance metric an unreachable state is represented
/// by the tuple (-inf, -inf) which we emulate using None. Since None represents the [Bottom] value
/// for HybridDistance, both values must be Some in order to compute a distance that is not None.
impl Meet for HybridDistance {
    fn meet(self, other: Self) -> Self {
        HybridDistance(self.0.zip(other.0).map(|(d1, d2)| d1.meet(d2)))
    }
}

impl<'a> Meet<&'a Self> for HybridDistance {
    fn meet(self, other: &'a Self) -> Self {
        self.meet(*other)
    }
}

/// Trait representing the binary operator that computes the least upper bound of two values.
///
/// In general, this equates to the maximum of the two values, but this behavior is not guaranteed.
/// When provided along with a partial ordering, the type forms a Join-Semilattice.
pub trait Join<Other = Self> {
    /// Compute the least upper bound of self and other
    fn join(self, other: Other) -> Self;
}

/// Join trait implementation for f64 by value
///
/// This implementation tries to propogate NaN values as much as possible so that users can be
/// alerted to any issues during computation. If either of the values being compared are NaN values
/// then the result of the Meet operator will also be NaN.
impl Join for f64 {
    fn join(self, other: Self) -> Self {
        if self.is_nan() || other.is_nan() {
            f64::NAN
        } else {
            f64::max(self, other)
        }
    }
}

impl Join<&Self> for f64 {
    fn join(self, other: &Self) -> Self {
        self.meet(*other)
    }
}

/// [Join] trait implementation for StateDistance by value
///
/// This implementation considers smaller values of the discrete distance to be greater. This
/// behavior originates from the initial description of Hybrid Distance where the discrete distance
/// is negative to support minimization. Since we use usize to represent the discrete distance,
/// which cannot be negative, we must take the max instead of the min to replicate the original
/// behavior. To compare the continuous components, we delegate to the [Join] implementation for
/// f64 to compute the maximum.
impl Join for StateDistance {
    fn join(self, other: Self) -> Self {
        match self.discrete.cmp(&other.discrete) {
            Ordering::Less => self,
            Ordering::Greater => other,
            Ordering::Equal => StateDistance {
                discrete: self.discrete,
                continuous: self.continuous.join(other.continuous),
            },
        }
    }
}

/// [Join] trait implementation for HybridDistance by values
///
/// In the original description of the HybridDistance metric an unreachable state is represented
/// by the tuple (-inf, -inf) which we emulate using None. Since None represents the [Bottom] value
/// for HybridDistance, a None value is only returned if both of the values are None.
impl Join for HybridDistance {
    fn join(self, other: Self) -> Self {
        let inner = match (self.0, other.0) {
            (Some(d1), Some(d2)) => Some(d1.join(d2)),
            (None, Some(right)) => Some(right),
            (Some(left), None) => Some(left),
            (None, None) => None,
        };

        HybridDistance(inner)
    }
}

impl<'a> Join<&'a Self> for HybridDistance {
    fn join(self, other: &'a Self) -> Self {
        self.join(*other)
    }
}

/// Trait representing a type with a least value
///
/// When combined with the [Join] trait, this element should serve as the identity for the operator.
/// In other words, for any value x: join(x, bottom) = x. This value is sometimes referred to as
/// the zero element.
pub trait Bottom {
    fn bottom() -> Self;
}

impl Bottom for f64 {
    fn bottom() -> Self {
        f64::NEG_INFINITY
    }
}

impl Bottom for HybridDistance {
    fn bottom() -> Self {
        HybridDistance(None)
    }
}

/// Trait representing a type with a greatest value
///
/// When combined with the [Meet] trait, this element should serve as the identity for the
/// operator. In other words, for any value x: meet(x, top) = x. This value is sometimes referred
/// to as the one element.
pub trait Top {
    fn top() -> Self;
}

impl Top for f64 {
    fn top() -> Self {
        f64::INFINITY
    }
}

impl Top for HybridDistance {
    fn top() -> Self {
        let distance = StateDistance {
            discrete: 0,
            continuous: f64::INFINITY,
        };

        HybridDistance(Some(distance))
    }
}
