/// Distance from one state to another state
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct StateDistance {
    pub path_length: usize,
    pub guard_distance: f64,
}

/// Metric for analyzing hybrid automata
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum HybridDistance {
    Unreachable,
    Robustness(f64),
    StateDistance(StateDistance),
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

impl Meet for HybridDistance {
    fn meet(self, other: Self) -> Self {
        todo!()
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
