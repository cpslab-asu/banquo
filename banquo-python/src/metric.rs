use std::cmp::Ordering;
use std::ops::Neg;

use pyo3::types::{PyAny, PyAnyMethods};
use pyo3::{Bound, IntoPyObject, IntoPyObjectExt, Py, PyResult, Python, pyclass, pymethods};

use banquo_core::{Bottom, Join, Meet, Top};

// This class is a special value that represents the maximum of ALL python values.
// Thus, this the greater-than implementation for this class will always return true.
#[pyclass(name = "Top")]
pub struct PyTop;

#[pymethods]
impl PyTop {
    fn __gt__(&self, other: &Bound<'_, PyAny>) -> bool {
        true
    }
}

// This class is a special value that represents the minimum of ALL python values.
// Thus, this the less-than implementation for this class will always return true.
#[pyclass(name = "Bottom")]
pub struct PyBottom;

#[pymethods]
impl PyBottom {
    fn __lt__(&self, other: &Bound<'_, PyAny>) -> bool {
        true
    }
}

#[derive(IntoPyObject)]
pub struct PyMetric(Py<PyAny>);

impl PyMetric {
    pub fn try_from_f64(value: f64, py: Python<'_>) -> PyResult<Self> {
        value.into_py_any(py).map(Self)
    }
}

impl From<Py<PyAny>> for PyMetric {
    fn from(value: Py<PyAny>) -> Self {
        Self(value)
    }
}

impl From<Bound<'_, PyAny>> for PyMetric {
    fn from(value: Bound<'_, PyAny>) -> Self {
        Self(value.unbind())
    }
}

impl Into<Py<PyAny>> for PyMetric {
    fn into(self) -> Py<PyAny> {
        self.0
    }
}

// Required for PartialOrd implementation
impl PartialEq for PyMetric {
    fn eq(&self, other: &Self) -> bool {
        // If equality is not defined, then the two objects are never equal
        Python::attach(|py| self.0.bind(py).eq(&other.0).unwrap())
    }
}

impl Neg for PyMetric {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Python::attach(|py| self.0.bind(py).neg().map(Self::from).unwrap())
    }
}

// Required for Meet implementation
impl PartialOrd for PyMetric {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Python::attach(|py| Some(self.0.bind(py).compare(other).unwrap()))
    }
}

fn pymeet<'py>(lhs: &Bound<'py, PyAny>, rhs: &Bound<'py, PyAny>) -> Bound<'py, PyAny> {
    if lhs.le(rhs).unwrap() { lhs.clone() } else { rhs.clone() }
}

// Required for operator implementations (And, Iff, etc.)
impl Meet for PyMetric {
    fn min(&self, other: &Self) -> Self {
        Python::attach(|py| pymeet(self.0.bind(py), other.0.bind(py)).into())
    }
}

fn pyjoin<'py>(lhs: &Bound<'py, PyAny>, rhs: &Bound<'py, PyAny>) -> Bound<'py, PyAny> {
    if lhs.ge(rhs).unwrap() { lhs.clone() } else { rhs.clone() }
}

// Required for operator implementations (Or, Implies, etc.)
impl Join for PyMetric {
    fn max(&self, other: &Self) -> Self {
        Python::attach(|py| pyjoin(self.0.bind(py), other.0.bind(py)).into())
    }
}

impl Top for PyMetric {
    fn top() -> Self {
        Python::attach(|py| PyMetric(PyBottom.into_py_any(py).unwrap()))
    }
}

impl Bottom for PyMetric {
    fn bottom() -> Self {
        Python::attach(|py| PyMetric(PyBottom.into_py_any(py).unwrap()))
    }
}
