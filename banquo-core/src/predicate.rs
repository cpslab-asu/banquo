//! System requirements expressed as the inequality **`ax`**`≤ b`.
//!
//! In this library a [`Predicate`] is used to represent some constraint on a system state. As an
//! example, consider a requirement on a automotive system such as "The RPM of transmission should
//! always be greater than or equal to 10,000". In this situation, we would use a `Predicate` to
//! represent the `rpm <= 10_000` part of the requirement.
//!
//! In this inequality, both **a** and **x** are both maps of names to [`f64`] variables, while b
//! is a constant value. The **a** vector contains the variable coefficients and is created while
//! constructing the predicate. Conversely, the **x** vector contains the variable values and is
//! provided by the system trace. The [robustness] value of the inequality is computed using the
//! equation `b - `**`a`**`·`**`x`**.
//!
//! [robustness]: https://link.springer.com/chapter/10.1007/11940197_12
//!
//! # Examples
//!
//! You can explicitly create a new empty `Predicate` using the [`Predicate::new`] function:
//!
//! ```rust
//! use banquo::Predicate;
//! let p = Predicate::new();
//! ```
//!
//! You can add new terms to a predicate using the [`AddAssign`](std::ops::AddAssign) and
//! [`SubAssign`](std::ops::SubAssign) operators like so:
//!
//! ```rust
//! use banquo::Predicate;
//!
//! let mut p = Predicate::new();
//!
//! p += ("x", 1.5);  // Add a term x to the _a_ vector with coefficient 1.0
//! p += (2.0, "y");  // Add a term y to the _a_ vector with coefficient 2.0
//! p += "z";         // Add a term z to the _a_ vector with coefficient 1.0
//! p += 1.3;         // Add constant to _b_ value;
//!
//! p -= ("x", 0.6);  // Subtract a term x from _a_ vector with coefficient 0.5
//! p -= (2.0, "y");  // Subtract a term y from _a_ vector with coefficient 2.0
//! p -= 0.6;         // Subtract constant from _b_ value;
//! ```
//!
//! Any value that can be converted into a [`Term`] is supported as an `AddAssign` operand. If a
//! variable already contains a coefficient then the two values will be added together. Therefore,
//! the final result of the example above would be the predicate `0.9 * x + 1.0 * z ≤ 0.7`.
//!
//! For a predicate of known length, you can create a `Predicate` from an array:
//!
//! ```rust
//! use banquo::predicate::{Predicate, Term};
//!
//! let p = Predicate::from([
//!     Term::from(("x", 1.0)),
//!     Term::from((1.0, "y")),
//!     Term::from("z"),
//!     Term::from(1.0),
//! ]);
//! ```
//!
//! You can also use the [`predicate!`](crate::predicate!) macro for short-hand initialization:
//!
//! ```rust
//! use banquo::predicate;
//! let p = predicate!{ x + y * 1.1 + 1.2 * z + 1.3 <= 2.0 * a + 2.1 };
//! ```
//!
//! Once a `Predicate` has been constructed, you can use it to evaluate a system state using the
//! [`Predicate::evaluate_state`] method. Any type that implements the [`VariableSet`] trait can be
//! evaluated.
//!
//! ```rust
//! use std::collections::{HashMap, BTreeMap};
//!
//! use banquo::predicate;
//!
//! let p = predicate!{ x + y * 1.1 + 1.2 * z + 1.3 <= 2.0 * a + 2.1 };
//! let s1 = HashMap::from([
//!     ("x", 1.0),
//!     ("y", 2.0),
//!     ("z", 3.0),
//!     ("a", 0.5),
//! ]);
//!
//! let s2 = BTreeMap::from([
//!     ("x", 1.5),
//!     ("y", 0.8),
//!     ("z", 4.9),
//!     ("a", 0.1),
//! ]);
//!
//! let _ = p.evaluate_state(&s1);
//! let _ = p.evaluate_state(&s2);
//! ```
//!
//! A `Predicate` also implements the [`Formula`] trait which allows evaluating a set of timed
//! system states.
//!
//! ```rust
//! use std::collections::HashMap;
//!
//! use banquo::predicate;
//! use banquo::{Trace, Formula};
//!
//! let p = predicate!{ x + y * 1.1 + 1.2 * z + 1.3 <= 2.0 * a + 2.1 };
//! let trace = Trace::from([
//!     (1.0, HashMap::from([
//!         ("x", 1.0),
//!         ("y", 2.0),
//!         ("z", 3.0),
//!         ("a", 0.5),
//!     ])),
//!     (2.0, HashMap::from([
//!         ("x", 1.5),
//!         ("y", 0.8),
//!         ("z", 4.9),
//!         ("a", 0.1),
//!     ])),
//! ]);
//!
//! let _ = p.evaluate(&trace);
//! ```

use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::ops::{AddAssign, Index, Neg, SubAssign};

use thiserror::Error;

use crate::trace::Trace;
use crate::Formula;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Comparison {
    #[default]
    LTE,
    EQ,
}

/// System requirements expressed as the inequality **`ax`**`≤ b`.
///
/// See the [`predicate`](predicate) module for more information on the semantics of this data type.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Predicate {
    coefficients: HashMap<String, f64>,
    pub comparison: Comparison,
    constant: f64,
}

impl Predicate {
    /// Create a new empty predicate equivalent to `0 ≤ 0`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::Predicate;
    /// let mut p = Predicate::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Return a variable coefficient from the left-hand side of the inequality if it has been set,
    /// otherwise return [`None`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::Predicate;
    ///
    /// let p = Predicate::from([
    ///     ("x", 1.0), ("y", 3.0), ("z", 2.0),
    /// ]);
    ///
    /// p.get("x");  // Some(3.0)
    /// p.get("a");  // None
    /// ```
    pub fn get(&self, name: &str) -> Option<f64> {
        self.coefficients.get(name).copied()
    }

    /// Return the constant from the right-hand side of the inequality.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use banquo::Predicate;
    ///
    /// let p = Predicate::from([
    ///     (None, 5.0), (Some("x"), 1.0), (Some("y"), 2.0), (None, 10.0),
    /// ]);
    ///
    /// p.constant();  // 15
    /// ```
    pub fn constant(&self) -> f64 {
        self.constant
    }
}

/// Iterator over the coefficient terms of a predicate.
///
/// The coefficient terms are the terms with a variable name associated with them. This iterator
/// does not guarantee any order of the terms.
#[derive(Debug)]
pub struct Coefficients<'a>(std::collections::hash_map::Iter<'a, String, f64>);

impl<'a> Iterator for Coefficients<'a> {
    type Item = (&'a str, f64);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(name, value)| (name.as_str(), *value))
    }
}

impl Predicate {
    /// Create an iterator over the coefficient terms of the `Predicate`.
    ///
    /// A coefficient term is a term that has a variable. This function does not make any guarantees
    /// about the order of iteration of the terms.
    ///
    /// # Example
    ///
    /// ```rust
    /// let p = banquo::predicate!{ x + 2.0 * y + 3.3 * z <= 4.2 };
    ///
    /// for (name, coefficient) in p.coefficients() {
    ///     // ...
    /// }
    /// ````
    pub fn coefficients(&self) -> Coefficients<'_> {
        Coefficients(self.coefficients.iter())
    }
}

impl<T> FromIterator<T> for Predicate
where
    T: Into<Term>,
{
    fn from_iter<I>(terms: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut p = Predicate::new();

        for term in terms {
            p += term.into();
        }

        p
    }
}

impl<T, const N: usize> From<[T; N]> for Predicate
where
    T: Into<Term>,
{
    fn from(terms: [T; N]) -> Self {
        Predicate::from_iter(terms)
    }
}

impl Index<&str> for Predicate {
    type Output = f64;

    /// Returns a reference to a coefficient in the predicate
    ///
    /// # Panics
    ///
    /// Panics if the variable does not have a coefficient set
    fn index(&self, index: &str) -> &Self::Output {
        &self.coefficients[index]
    }
}

impl Neg for Predicate {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            constant: -self.constant,
            comparison: self.comparison,
            coefficients: self
                .coefficients
                .into_iter()
                .map(|(name, coeff)| (name, -coeff))
                .collect(),
        }
    }
}

/// A variable or constant term in a predicate.
///
/// A `Variable` term contains a variable name and the associated coefficient, while a `Constant`
/// term contains only a constant value. The purpose of this type is to provide a conversion target
/// that supports multiple different types as [`AddAssign`] operands for a [`Predicate`].
///
/// # Examples
///
/// ```rust
/// use banquo::predicate::Term;
///
/// let terms = [
///     Term::from(1.0),
///     Term::from("x"),
///     Term::from(("x", 1.0)),
///     Term::from((1.0, "x")),
///     Term::from((Some("x"), 1.0)),
///     Term::from((1.0, Some("x"))),
/// ];
///
/// ```
pub enum Term {
    Variable(String, f64),
    Constant(f64),
}

impl Neg for Term {
    type Output = Term;

    fn neg(self) -> Self::Output {
        match self {
            Self::Variable(name, value) => Self::Variable(name, -value),
            Self::Constant(value) => Self::Constant(-value),
        }
    }
}

impl From<&str> for Term {
    fn from(name: &str) -> Self {
        Term::Variable(name.into(), 1.0)
    }
}

impl From<String> for Term {
    fn from(name: String) -> Self {
        Term::Variable(name, 1.0)
    }
}

impl From<(&str, f64)> for Term {
    fn from((name, value): (&str, f64)) -> Self {
        Term::Variable(name.into(), value)
    }
}

impl From<(String, f64)> for Term {
    fn from((name, value): (String, f64)) -> Self {
        Term::Variable(name, value)
    }
}

impl From<(f64, &str)> for Term {
    fn from((value, name): (f64, &str)) -> Self {
        Term::Variable(name.into(), value)
    }
}

impl From<(f64, String)> for Term {
    fn from((value, name): (f64, String)) -> Self {
        Term::Variable(name, value)
    }
}

impl From<f64> for Term {
    fn from(value: f64) -> Self {
        Term::Constant(value)
    }
}

impl From<(Option<String>, f64)> for Term {
    fn from((name, value): (Option<String>, f64)) -> Self {
        match name {
            Some(n) => Term::from((n, value)),
            None => Term::from(value),
        }
    }
}

impl From<(Option<&str>, f64)> for Term {
    fn from((name, value): (Option<&str>, f64)) -> Self {
        match name {
            Some(n) => Term::from((n, value)),
            None => Term::from(value),
        }
    }
}

impl From<(f64, Option<String>)> for Term {
    fn from((value, name): (f64, Option<String>)) -> Self {
        match name {
            Some(n) => Term::from((n, value)),
            None => Term::from(value),
        }
    }
}

impl From<(f64, Option<&str>)> for Term {
    fn from((value, name): (f64, Option<&str>)) -> Self {
        match name {
            Some(n) => Term::from((n, value)),
            None => Term::from(value),
        }
    }
}

impl<T> AddAssign<T> for Predicate
where
    T: Into<Term>,
{
    fn add_assign(&mut self, rhs: T) {
        match rhs.into() {
            Term::Variable(name, value) => {
                let coeff = self.coefficients.entry(name).or_insert(0.0);
                *coeff += value;
            }
            Term::Constant(value) => {
                self.constant += value;
            }
        }
    }
}

impl<T> SubAssign<T> for Predicate
where
    T: Into<Term>,
{
    fn sub_assign(&mut self, rhs: T) {
        *self += -rhs.into();
    }
}

/// Trait representing a set of named variables.
pub trait VariableSet {
    /// Return the value for a name in the set if it exists.
    fn value_for(&self, name: &str) -> Option<f64>;
}

impl<K> VariableSet for HashMap<K, f64>
where
    K: Borrow<str> + Eq + Hash,
{
    fn value_for(&self, name: &str) -> Option<f64> {
        self.get(name).copied()
    }
}

impl<K> VariableSet for BTreeMap<K, f64>
where
    K: Borrow<str> + Ord + Hash,
{
    fn value_for(&self, name: &str) -> Option<f64> {
        self.get(name).copied()
    }
}

/// Error categories that can be produced from [`Predicate::evaluate_state`]
///
/// This enum is marked as `non_exhaustive` so it is best practice to match against the `ErrorKind`
/// variants you are expecting, and use `_` for all the rest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("Missing variable")]
    Missing,

    #[error("NaN value for variable")]
    NanValue,

    #[error("NaN coefficient for variable")]
    NanCoefficient,
}

/// The error type for evaluating of a state using a predicate.
///
/// The primary source of this error is the [`Predicate::evaluate_state`] method. This error can
/// represent one of three following occurences:
///   1. A variable has a coefficient but not a value in the variable set
///   2. A variable has a value in the variable set that is NaN
///   3. A variable has a coefficient that is NaN
///
/// Values of this type can be queried for its cause, as well as the variable name causing the
/// error.
#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Error evaluating predicate: {kind} \"{name}\"")]
pub struct EvaluationError {
    kind: ErrorKind,
    name: String,
}

impl EvaluationError {
    /// Create an error for a missing variable
    ///
    /// This method creates a clone of the name argument.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use banquo::predicate::EvaluationError;
    /// let err = EvaluationError::missing("foo");
    /// ```
    pub fn missing(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            kind: ErrorKind::Missing,
        }
    }

    /// Create an error for a variable with a NaN value
    ///
    /// This method creates a clone of the name argument.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use banquo::predicate::EvaluationError;
    /// let err = EvaluationError::nan_value("foo");
    /// ```
    pub fn nan_value(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            kind: ErrorKind::NanValue,
        }
    }

    /// Create an error for a variable with a NaN coefficient
    ///
    /// This method creates a clone of the name argument.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use banquo::predicate::EvaluationError;
    /// let err = EvaluationError::nan_coefficient("foo");
    /// ```
    pub fn nan_coefficient(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            kind: ErrorKind::NanCoefficient,
        }
    }

    /// Return the name of the variable that produced the error
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return the [`ErrorKind`] for this error
    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl Predicate {
    /// Evaluate a system state into a robustness value.
    ///
    /// The successful output of this function is a `f64` value representing the distance of the
    /// state from making the inequality represented by the `Predicate` false. A positive output
    /// value indicates that the inequality was not violated, while a negative value indicates the
    /// inequality was violated. The unsuccessful output of this function is a [`PredicateError`]
    /// which indicates the nature of the evaluation error.
    ///
    /// Any value that implements the [`VariableSet`] trait can be evaluated with this method.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::collections::HashMap;
    ///
    /// use banquo::predicate::{Predicate, Term};
    ///
    /// let state = HashMap::from([
    ///      ("x", 1.0), ("y", 3.0), ("z", f64::NAN),
    /// ]);
    ///
    /// // BTreeMap also implements the VariableSet trait
    /// // let state = BTreeMap::from([
    /// //     ("x", 1.0), ("y", 3.0), ("z", f64::NAN),
    /// // ]);
    ///
    /// let p = Predicate::from([
    ///     Term::from(("x", 1.0)), Term::from(("y", 2.0)), Term::from(10.0)
    /// ]);
    ///
    /// p.evaluate_state(&state);  // Ok -> 3.0
    ///
    /// let p = Predicate::from([
    ///     Term::from(("x", 1.0)), Term::from(("a", 2.0)), Term::from(10.0)
    /// ]);
    ///
    /// p.evaluate_state(&state);  // Error -> Missing variable "a"
    ///
    /// let p = Predicate::from([
    ///     Term::from(("x", 1.0)), Term::from(("z", 2.0)), Term::from(10.0)
    /// ]);
    ///
    /// p.evaluate_state(&state);  // Error -> Variable "z" has NaN value
    ///
    /// let p = Predicate::from([
    ///     Term::from(("x", 1.0)), Term::from(("y", f64::NAN)), Term::from(10.0)
    /// ]);
    ///
    /// p.evaluate_state(&state);  // Error -> Variable "y" has NaN coefficient
    /// ```
    pub fn evaluate_state<State>(&self, state: &State) -> Result<f64, EvaluationError>
    where
        State: VariableSet,
    {
        let mut sum = 0.0;

        for (name, coeff) in &self.coefficients {
            if coeff.is_nan() {
                return Err(EvaluationError::nan_coefficient(name));
            }

            let value = state
                .value_for(name.as_str())
                .ok_or_else(|| EvaluationError::missing(name))?;

            if value.is_nan() {
                return Err(EvaluationError::nan_value(name));
            }

            sum += coeff * value;
        }

        let metric = match &self.comparison {
            Comparison::LTE => self.constant - sum,
            Comparison::EQ => {
                if sum == self.constant {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
        };

        Ok(metric)
    }
}

/// The error type when evaluating a state trace using a [`Predicate`].
#[derive(Debug, Clone, Error)]
#[error("At time {time} encountered error: {error}")]
pub struct FormulaError {
    time: f64,

    #[source]
    error: EvaluationError,
}

impl FormulaError {
    /// Create a new evaluation error for a given time.
    ///
    /// # Example
    ///
    /// ```rust
    /// use banquo::predicate::{EvaluationError, FormulaError};
    ///
    /// let err = FormulaError::new(1.0, EvaluationError::missing("x"));
    /// ```
    pub fn new(time: f64, error: EvaluationError) -> Self {
        Self { time, error }
    }

    /// Returns the time of the state that produced the [`EvaluationError`].
    pub fn time(&self) -> f64 {
        self.time
    }

    /// Returns a reference to the [`EvaluationError`] that was generated.
    pub fn error(&self) -> &EvaluationError {
        &self.error
    }
}

/// Evaluate a [`Trace`] of state values by evaluating each state individually.
impl<State> Formula<State> for Predicate
where
    State: VariableSet,
{
    type Metric = f64;
    type Error = FormulaError;

    fn evaluate(&self, trace: &Trace<State>) -> Result<Trace<Self::Metric>, Self::Error> {
        trace
            .iter()
            .map(|(time, state)| {
                self.evaluate_state(state)
                    .map(|rho| (time, rho))
                    .map_err(|error| FormulaError { time, error })
            })
            .collect()
    }
}

/// Create a [`Predicate`] from a given algebraic expression.
///
/// This macro parses an algebraic expression into a predicate from right to left. Terms can be
/// specified as either a `constant`, `variable`, `coefficient * variable`, or
/// `variable * coefficient`. Malformed expressions will result in a compile-time error. An empty
/// expression will also result in compile-time error.
///
/// # Example
///
/// ```rust
/// banquo::predicate!{ 1.0 * x + y + -2.0 * z <= 10.0 + a };
/// ```
#[macro_export]
macro_rules! predicate {
    (@rhs $(+)? $coeff:literal * $name:ident $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!(@rhs $($rest)*);
            pred += (stringify!($name), $coeff);
            pred
        }
    };
    (@rhs $(+)? $name:ident * $coeff:literal $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!(@rhs $($rest)*);
            pred += (stringify!($name), $coeff);
            pred
        }
    };
    (@rhs $(+)? $name:ident $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!(@rhs $($rest)*);
            pred += (stringify!($name), 1.0);
            pred
        }
    };
    (@rhs $(+)? $const:literal $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!(@rhs $($rest)*);
            pred -= $const;
            pred
        }
    };
    (@rhs) => {
        $crate::predicate::Predicate::new()
    };
    ($(+)? $coeff:literal * $name:ident $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!($($rest)*);
            pred += (stringify!($name), $coeff);
            pred
        }
    };
    ($(+)? $name:ident * $coeff:literal $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!($($rest)*);
            pred += (stringify!($name), $coeff);
            pred
        }
    };
    ($(+)? $name:ident $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!($($rest)*);
            pred += (stringify!($name), 1.0);
            pred
        }
    };
    ($(+)? $const:literal $($rest:tt)*) => {
        {
            let mut pred = $crate::predicate!($($rest)*);
            pred -= $const;
            pred
        }
    };
    (<= $($rest:tt)+) => {
        {
            let pred = $crate::predicate!(@rhs $($rest)*);
            std::ops::Neg::neg(pred)
        }
    };
    (= $($rest:tt)+) => {
        {
            let mut pred = $crate::predicate!(@rhs $($rest)*);
            pred.comparison = $crate::predicate::Comparison::EQ;
            std::ops::Neg::neg(pred)
        }
    };
    (<=) => {
        compile_error!("Missing right hand side of predicate")
    };
    () => {
        compile_error!("Missing inequality (<=) sign in predicate")
    };
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use super::{Comparison, EvaluationError, Predicate};
    use crate::predicate;

    #[test]
    fn macro_parsing() {
        // Equivalent to 3.0 * x - 4.1 * y - 1.0 * z <= 2.2
        let p = predicate! { x * 3.0 + 1.0 <= y * 4.1 + 3.2 + z };

        assert_eq!(p.get("x"), Some(3.0));
        assert_eq!(p.get("y"), Some(-4.1));
        assert_eq!(p.get("z"), Some(-1.0));
        assert_eq!(p.constant(), 2.2);
    }

    #[test]
    fn robustness() {
        // 1.0 * x + 2.0 * y <= 10
        let mut p = Predicate::new();
        p += ("x", 1.0);
        p += ("y", 2.0);
        p += 10.0;

        let hash_map = HashMap::from([("x", 2.0), ("y", 1.0)]);
        let btree = BTreeMap::from([("x", 5.0), ("y", 5.0)]);
        let missing = HashMap::from([("y", 2.0)]);
        let nan_value = HashMap::from([("x", 2.0), ("y", f64::NAN)]);

        assert_eq!(p.evaluate_state(&hash_map), Ok(6.0));
        assert_eq!(p.evaluate_state(&btree), Ok(-5.0));
        assert_eq!(p.evaluate_state(&missing), Err(EvaluationError::missing("x")));
        assert_eq!(p.evaluate_state(&nan_value), Err(EvaluationError::nan_value("y")));

        let true_input = HashMap::from([("x", 2.0), ("y", 4.0)]);
        p.comparison = Comparison::EQ;

        assert_eq!(p.evaluate_state(&true_input), Ok(f64::INFINITY));
        assert_eq!(p.evaluate_state(&hash_map), Ok(f64::NEG_INFINITY));

        p.comparison = Comparison::LTE;
        p += ("z", f64::NAN);

        assert_eq!(p.evaluate_state(&hash_map), Err(EvaluationError::nan_coefficient("z")));
    }
}
