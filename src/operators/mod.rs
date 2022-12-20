use std::error::Error;
use std::fmt::{Display, Formatter};

use crate::formula::Formula;
use crate::trace::Trace;

struct Const(Trace<f64>);

#[derive(Debug)]
struct ConstError;

impl Display for ConstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConstError")
    }
}

impl Error for ConstError {}

impl Formula<()> for Const {
    type Error = ConstError;

    fn robustness(&self, _: &Trace<()>) -> Result<Trace<f64>, Self::Error> {
        Ok(self.0.clone())
    }
}

mod always;
mod and;
mod binary;
mod eventually;
mod implies;
mod next;
mod not;
mod or;
mod temporal;
mod until;

pub use always::Always;
pub use and::And;
pub use eventually::Eventually;
pub use implies::Implies;
pub use next::Next;
pub use not::Not;
pub use or::Or;
