mod metric;

#[pyo3::pymodule]
mod _banquo_impl {
    use std::collections::HashMap;

    use pyo3::exceptions::PyRuntimeError;
    use pyo3::prelude::*;
    use pyo3::types::PyDict;
    use pyo3::{FromPyObject, IntoPyObject};

    use super::metric::PyMetric;
    use banquo_core::operators::{And, BinaryOperatorError, Not};
    use banquo_core::predicate::Predicate;
    use banquo_core::{Formula, Trace};

    #[pyclass(name = "Trace")]
    struct PyTrace(Trace<Py<PyAny>>);

    impl<'py> FromPyObject<'py> for PyTrace {
        fn extract_bound(obj: &Bound<'py, PyAny>) -> PyResult<Self> {
            Self::new(obj.cast::<PyDict>()?)
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

        fn times(&self) -> Vec<f64> {
            self.0.times().collect()
        }

        fn states(&self, py: Python<'_>) -> Vec<Py<PyAny>> {
            self.0.states().map(|state| state.clone_ref(py)).collect()
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

        if let Ok(not) = obj.cast::<PyNot>() {
            return not.borrow().evaluate_inner(trace);
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

    impl PyNot {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace)
        }
    }

    #[pymethods]
    impl PyNot {
        #[new]
        fn new(subformula: Py<PyAny>) -> Self {
            Self(Not::new(PyFormula(subformula)))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "And")]
    struct PyAnd(And<PyFormula, PyFormula>);

    impl PyAnd {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace).map_err(|err| match err {
                BinaryOperatorError::LeftError(left) => left,
                BinaryOperatorError::RightError(right) => right,
                BinaryOperatorError::EvaluationError(err) => PyRuntimeError::new_err(err.to_string()),
            })
        }
    }

    #[pymethods]
    impl PyAnd {
        #[new]
        fn new(lhs: Py<PyAny>, rhs: Py<PyAny>) -> Self {
            Self(And::new(PyFormula(lhs), PyFormula(rhs)))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass]
    struct Always;
}
