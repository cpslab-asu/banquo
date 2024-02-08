/// Trait representing a type with a global maximum.
///
/// The value returned by the `top` method should be greater than all other values in the type.
/// That is there should be no other value `v` such that `v > top`.
///
/// This trait is implemented using an associated function and not an associated constant because
/// it may be valuable for some implementers to use non-const functions to compute the global
/// maximum. In some instances this may be more inefficient than just defining a constant since it
/// requires the creation of a new stack frame for the function call.
pub trait Top {
    /// Compute the global maximum for the type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Top;
    /// let top = f64::top();  // f64::INFINITY
    /// ```
    fn top() -> Self;
}

/// Trait representing a type with a global minimum.
///
/// The value returned by the `bottom` method should be less than all other values in the type.
/// That is there should be no other value `v` such that `v < bottom`.
///
/// This trait is implemented using an associated function and not an associated constant because
/// it may be valuable for some implementers to use non-const functions to compute the global
/// maximum. In some instances this may be more inefficient than just defining a constant since it
/// requires the creation of a new stack frame for the function call.
pub trait Bottom {
    /// Compute the global minimum value for the type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Bottom;
    /// let bottom = f64::bottom();  // f64::NEG_INFINITY
    /// ```
    fn bottom() -> Self;
}

/// Trait representing a type that can compute the [infimum] of two values.
///
/// For a [`PartialOrd`] type the output [`Meet::min`] for values `a` and `b` should return a value
/// `v*` of the same type such that the `v* <= a`, `v* <= b`, and `v*` is greater than or equal to
/// all other members of the type which are also less than or equal to `a` and `b`. This trait is
/// sometimes referred to as the _Greatest Lower Bound_. In the case of types with a total ordering,
/// like `usize`, this is just the smallest value in the set. However, in general the infimum is not
/// required to be a member of the input set. Types that implement this trait form a [meet]
/// semi-lattice, hence the name.
///
/// [`Meet::min`] is takes its parameters as references rather than by value to reduce the amount
/// of copying necessary for a function that is not required to return one of its arguments. For
/// small types like [`f64`] there is no efficiency gain, but for arbitrarily large types it can be
/// more efficient.
///
/// [infimum]: https://en.wikipedia.org/wiki/Infimum_and_supremum
/// [meet]: https://en.wikipedia.org/wiki/Join_and_meet
pub trait Meet: PartialOrd {
    /// This method returns the infimum of two values.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Meet;
    /// let inf = Meet::min(&1.0, &2.0);  // 1.0
    /// ```
    fn min(&self, other: &Self) -> Self;
}

/// Trait representing a type that can compute the [supremum] of two values.
///
/// For a [`PartialOrd`] type the output [`Join::max`] for values `a` and `b` should return a value
/// `v*` of the same type such that the `v* >= a`, `v* >= b`, and `v*` is less than or equal to
/// all other members of the type which are also greater than or equal to `a` and `b`. This trait is
/// sometimes referred to as the _Smallest Upper Bound_. In the case of types with a total ordering,
/// like `usize`, this is just the largest value in the set. However, in general the supremum is not
/// required to be a member of the input set. Types that implement this trait form a [join]
/// semi-lattice, hence the name.
///
/// [`Join::max`] is takes its parameters as references rather than by value to reduce the amount
/// of copying necessary for a function that is not required to return one of its arguments. For
/// small types like [`f64`] there is no efficiency gain, but for arbitrarily large types it can be
/// more efficient.
///
/// [supremum]: https://en.wikipedia.org/wiki/Infimum_and_supremum
/// [join]: https://en.wikipedia.org/wiki/Join_and_meet
pub trait Join: PartialOrd {
    /// This method returns the supremum of two values.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::Join;
    /// let sup = Join::max(&1.0, &2.0);  // 2.0
    /// ```
    fn max(&self, other: &Self) -> Self;
}

impl Top for f64 {
    fn top() -> Self {
        f64::INFINITY
    }
}

impl Bottom for f64 {
    fn bottom() -> Self {
        f64::NEG_INFINITY
    }
}

impl Meet for f64 {
    fn min(&self, other: &Self) -> Self {
        f64::min(*self, *other)
    }
}

impl Join for f64 {
    fn max(&self, other: &Self) -> Self {
        f64::max(*self, *other)
    }
}
