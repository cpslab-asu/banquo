#[pyo3::pymodule]
mod _banquo_impl {
    use std::collections::HashMap;

    use pyo3::PyErr;
    use pyo3::exceptions::PyRuntimeError;
    use pyo3::prelude::*;
    use pyo3::types::PyDict;

    use banquo_core::operators::{And, Not};
    use banquo_core::predicate::Predicate;
    use banquo_core::{Formula, Trace};

    struct PyFormula {
        inner: Py<PyAny>,
    }

    fn evaluate_py<'py>(py: Python<'py>, obj: &Py<PyAny>, trace: &Trace<HashMap<String, f64>>) {
        if let Ok(pred) = obj.cast_bound::<PyPredicate>(py) {
            pred.borrow().inner.evaluate(trace);
        }
    }

    impl Formula<HashMap<String, f64>> for PyFormula {
        type Metric = f64;
        type Error = PyErr;

        fn evaluate(&self, trace: &Trace<HashMap<String, f64>>) -> Result<Trace<Self::Metric>, Self::Error> {
            Python::attach(|py| evaluate_py(py, &self.inner, trace));
            todo!()
        }
    }

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
    }

    #[pyclass(name = "Predicate")]
    struct PyPredicate {
        inner: Predicate,
    }

    #[pymethods]
    impl PyPredicate {
        pub fn evaluate<'py>(&self, pytrace: &Bound<'py, PyDict>) -> PyResult<Bound<'py, PyDict>> {
            let trace = convert_pytrace(pytrace);
            let evaluated = self
                .inner
                .evaluate(&trace)
                .map_err(|err| PyRuntimeError::new_err(err.to_string()));

            let python = pytrace.py();
            let converted = convert_trace(python, evaluated?);

            Ok(converted)
        }
    }

    #[pyclass]
    struct PyNot(Not<PyFormula>);

    #[pymethods]
    impl PyNot {
        pub fn evaluate<'py>(&self, pytrace: &Bound<'py, PyDict>) -> PyResult<Bound<'py, PyDict>> {
            todo!()
        }
    }

    #[pyclass(name = "And")]
    struct PyAnd {
        inner: And<PyFormula, PyFormula>,
    }

    #[pymethods]
    impl PyAnd {
        pub fn evaluate(&self) {}
    }
}
