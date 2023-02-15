mod backwards;
mod binary;
mod forward;
mod unary;

mod testing {
    use std::error::Error;
    use std::fmt::{Display, Formatter};

    use crate::trace::Trace;
    use crate::Formula;

    pub struct Const<T>(Trace<T>);

    impl<T> From<Trace<T>> for Const<T> {
        fn from(value: Trace<T>) -> Self {
            Self(value)
        }
    }

    #[derive(Debug)]
    pub struct ConstError;

    impl Display for ConstError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            unreachable!("Const cannot return an error")
        }
    }

    impl Error for ConstError {}

    impl<T> Formula<T> for Const<T>
    where
        T: Clone,
    {
        type State = ();
        type Error = ConstError;

        fn evaluate_states(&self, _: &Trace<Self::State>) -> Result<Trace<T>, Self::Error> {
            Ok(self.0.clone())
        }
    }
}

pub use backwards::Until;
pub use binary::{And, Implies, Or};
pub use forward::{Always, BoundedAlways, BoundedEventually, Eventually, Next};
pub use unary::Not;
