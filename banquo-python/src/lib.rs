mod metric;

#[pyo3::pymodule]
mod _banquo_impl {
    use std::collections::HashMap;

    use pyo3::exceptions::PyRuntimeError;
    use pyo3::prelude::*;
    use pyo3::types::{PyDict, PyType};
    use pyo3::{FromPyObject, IntoPyObject, IntoPyObjectExt};

    use super::metric::PyMetric;
    use banquo_core::operators::{And, Not};
    use banquo_core::predicate::Predicate;
    use banquo_core::{Formula, Trace};

    #[pyclass(name = "Trace")]
    struct PyTrace(Trace<Py<PyAny>>);

    impl<'py> FromPyObject<'py> for PyTrace {
        fn extract_bound(obj: &Bound<'py, PyAny>) -> PyResult<Self> {
            Self::new(obj.cast::<PyDict>()?)
        }
    }

    impl PyTrace {
        fn from_trace<'py, T>(py: Python<'py>, trace: Trace<T>) -> PyResult<Self>
        where
            T: IntoPyObject<'py>,
        {
            trace
                .into_iter()
                .map(|(time, state)| state.into_py_any(py).map(|obj| (time, obj)))
                .collect::<PyResult<Trace<Py<PyAny>>>>()
                .map(|t| Self(t))
        }
    }

    #[pymethods]
    impl PyTrace {
        #[new]
        fn new(elements: &Bound<'_, PyDict>) -> PyResult<Self> {
            elements
                .iter()
                .map(|(key, value)| key.extract::<f64>().map(|time| (time, value.unbind())))
                .collect::<PyResult<Trace<_>>>()
                .map(|trace| Self(trace))
        }

        #[classmethod]
        fn from_timed_states(_: &Bound<'_, PyType>, times: Vec<f64>, states: Vec<Py<PyAny>>) -> Self {
            Self(times.into_iter().zip(states.into_iter()).collect())
        }

        fn __eq__(&self, other: &Bound<'_, Self>) -> PyResult<bool> {
            let py = other.py();
            let other = &other.borrow().0;

            if self.0.len() != other.len() {
                return Ok(false); // Traces cannot be equal with different number of elements
            }

            for (time, state) in &self.0 {
                let states_equal = other
                    .at_time(time) // Try to retrieve state from other trace at the given time
                    .map(|other_state| state.bind(py).eq(other_state)) // Compare states if they exist using python __eq__ method
                    .transpose()? // Raise python error early if __eq__ method throws an error
                    .unwrap_or(false); // Default to false if state does not exist for current time

                if states_equal == false {
                    return Ok(false);
                }
            }

            Ok(true)
        }
    }

    #[pyclass(name = "Predicate", subclass)]
    struct PyPredicate(Predicate);

    struct PyMetricTrace(Trace<PyMetric>);

    impl PyMetricTrace {
        fn into_inner(self) -> Trace<PyMetric> {
            self.0
        }
    }

    impl From<PyTrace> for PyMetricTrace {
        fn from(value: PyTrace) -> Self {
            Self(value.0.into_iter().map_states(PyMetric::from).collect())
        }
    }

    impl<'py> IntoPyObject<'py> for PyMetricTrace {
        type Target = PyTrace;
        type Output = Bound<'py, Self::Target>;
        type Error = PyErr;

        fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
            let trace = self
                .0
                .into_iter()
                .map_states(|state| state.into())
                .collect::<Trace<Py<PyAny>>>();

            Bound::new(py, PyTrace(trace))
        }
    }

    impl PyPredicate {
        fn evaluate_inner(&self, py: Python<'_>, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            let converted = trace
                .iter()
                .map(|(time, state)| state.extract::<HashMap<String, f64>>(py).map(|s| (time, s)))
                .collect::<PyResult<Trace<_>>>()?;

            let evaluated = self
                .0
                .evaluate(&converted)
                .map_err(|err| PyRuntimeError::new_err(err.to_string()))?;

            evaluated
                .into_iter()
                .map(|(time, rho)| PyMetric::try_from_f64(rho, py).map(|m| (time, m)))
                .collect()
        }
    }

    #[pymethods]
    impl PyPredicate {
        #[new]
        pub fn new(coefficients: HashMap<String, f64>, constant: f64) -> Self {
            Self(Predicate::new(coefficients, constant))
        }

        pub fn __eq__(&self, other: &Bound<'_, Self>) -> bool {
            self.0 == other.borrow().0
        }

        pub fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(trace.py(), &trace.borrow().0).map(PyMetricTrace)
        }
    }

    struct PyFormula(Py<PyAny>);

    fn evaluate(obj: &Bound<'_, PyAny>, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
        if let Ok(pred) = obj.cast::<PyPredicate>() {
            return pred.borrow().evaluate_inner(obj.py(), trace);
        }

        obj.call_method1("evaluate", ())
            .and_then(|result| result.extract::<PyTrace>())
            .map(|result| PyMetricTrace::from(result).into_inner())
    }

    impl Formula<Py<PyAny>> for PyFormula {
        type Metric = PyMetric;
        type Error = PyErr;

        fn evaluate(&self, trace: &Trace<Py<PyAny>>) -> Result<Trace<Self::Metric>, Self::Error> {
            Python::attach(|py| evaluate(self.0.bind(py), trace))
        }
    }

    #[pyclass(name = "Not")]
    struct PyNot(Not<PyFormula>);

    #[pymethods]
    impl PyNot {
        #[new]
        fn new(subformula: Py<PyAny>) -> Self {
            Self(Not::new(PyFormula(subformula)))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.0.evaluate(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "And")]
    struct PyAnd(And<PyFormula, PyFormula>);

    #[pymethods]
    impl PyAnd {}

    #[pyclass]
    struct Always;
}
