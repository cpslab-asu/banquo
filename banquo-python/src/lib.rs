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

    fn convert_pytrace(pytrace: &Bound<'_, PyDict>) -> Trace<HashMap<String, f64>> {
        todo!()
    }

    fn convert_trace<'py>(py: Python<'py>, trace: Trace<f64>) -> Bound<'py, PyDict> {
        todo!()
    }

    #[pyclass(name = "Trace")]
    struct PyTrace(Trace<Py<PyAny>>);

    #[pymethods]
    impl PyTrace {
        #[new]
        fn new() -> Self {
            Self(Trace::default())
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
