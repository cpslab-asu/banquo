mod metric;

#[pyo3::pymodule]
mod _banquo_impl {
    use std::collections::HashMap;

    use pyo3::exceptions::{PyKeyError, PyRuntimeError, PyValueError};
    use pyo3::prelude::*;
    use pyo3::types::PyDict;
    use pyo3::{FromPyObject, IntoPyObject};

    use super::metric::PyMetric;
    use banquo_core::operators::{
        Always, And, BinaryOperatorError, Eventually, ForwardOperatorError, Implies, Next, Not, Or,
    };
    use banquo_core::predicate::Predicate;
    use banquo_core::{Formula, Trace};

    #[pyclass(name = "Trace", subclass, generic)]
    struct PyTrace(Trace<Py<PyAny>>);

    impl<'py> FromPyObject<'py> for PyTrace {
        fn extract_bound(obj: &Bound<'py, PyAny>) -> PyResult<Self> {
            Self::new(obj)
        }
    }

    #[pymethods]
    impl PyTrace {
        #[new]
        fn new(elements: &Bound<'_, PyAny>) -> PyResult<Self> {
            // If we construct a pytrace from a pytrace, we can copy without converting to python objects
            if let Ok(pytrace) = elements.cast::<PyTrace>() {
                let py = elements.py();
                let copied = pytrace.borrow().0.iter().map_states(|obj| obj.clone_ref(py)).collect();
                return Ok(PyTrace(copied));
            }

            elements
                .cast::<PyDict>()?
                .iter()
                .map(|(key, value)| key.extract::<f64>().map(|time| (time, value.unbind())))
                .collect::<PyResult<Trace<_>>>()
                .map(|trace| Self(trace))
        }

        fn __getitem__(&self, py: Python<'_>, time: f64) -> PyResult<Py<PyAny>> {
            self.at_time(py, time)
                .ok_or_else(|| PyKeyError::new_err(format!("Time {} is not present in trace", time)))
        }

        fn times(&self) -> Vec<f64> {
            self.0.times().collect()
        }

        fn states(&self, py: Python<'_>) -> Vec<Py<PyAny>> {
            self.0.states().map(|state| state.clone_ref(py)).collect()
        }

        fn at_time(&self, py: Python<'_>, time: f64) -> Option<Py<PyAny>> {
            self.0.at_time(time).map(|state| state.clone_ref(py))
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
                .collect::<PyResult<Trace<_>>>()
                .map_err(|_| PyValueError::new_err("Predicate only supports dict values as trace states."))?;

            let evaluated = self
                .0
                .evaluate(&converted)
                .map_err(|err| PyValueError::new_err(err.to_string()))?;

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
            let mut p = Predicate::from_iter(coefficients);
            p += constant;

            Self(p)
        }

        pub fn __eq__(&self, other: &Bound<'_, Self>) -> bool {
            self.0 == other.borrow().0
        }

        pub fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(trace.py(), &trace.borrow().0).map(PyMetricTrace)
        }
    }

    struct PyFormula(Py<PyAny>);

    impl<'py> FromPyObject<'py> for PyFormula {
        fn extract_bound(obj: &Bound<'py, PyAny>) -> PyResult<Self> {
            Ok(Self(obj.clone().unbind()))
        }
    }

    fn evaluate(obj: &Bound<'_, PyAny>, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
        if let Ok(pred) = obj.cast::<PyPredicate>() {
            return pred.borrow().evaluate_inner(obj.py(), trace);
        }

        if let Ok(not) = obj.cast::<PyNot>() {
            return not.borrow().evaluate_inner(trace);
        }

        if let Ok(and) = obj.cast::<PyAnd>() {
            return and.borrow().evaluate_inner(trace);
        }

        if let Ok(or) = obj.cast::<PyOr>() {
            return or.borrow().evaluate_inner(trace);
        }

        if let Ok(implies) = obj.cast::<PyImplies>() {
            return implies.borrow().evaluate_inner(trace);
        }

        if let Ok(always) = obj.cast::<PyAlways>() {
            return always.borrow().evaluate_inner(trace);
        }

        if let Ok(eventually) = obj.cast::<PyEventually>() {
            return eventually.borrow().evaluate_inner(trace);
        }

        let py = obj.py();
        let new_trace = trace.iter().map_states(|obj| obj.clone_ref(py)).collect();

        obj.call_method1("evaluate", (PyTrace(new_trace),))
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
        fn new(subformula: PyFormula) -> Self {
            Self(Not::new(subformula))
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
        fn new(lhs: PyFormula, rhs: PyFormula) -> Self {
            Self(And::new(lhs, rhs))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "Or")]
    struct PyOr(Or<PyFormula, PyFormula>);

    impl PyOr {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace).map_err(|err| match err {
                BinaryOperatorError::LeftError(left) => left,
                BinaryOperatorError::RightError(right) => right,
                BinaryOperatorError::EvaluationError(err) => PyRuntimeError::new_err(err.to_string()),
            })
        }
    }

    #[pymethods]
    impl PyOr {
        #[new]
        fn new(lhs: PyFormula, rhs: PyFormula) -> Self {
            Self(Or::new(lhs, rhs))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "Implies")]
    struct PyImplies(Implies<PyFormula, PyFormula>);

    impl PyImplies {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace).map_err(|err| match err {
                BinaryOperatorError::LeftError(left) => left,
                BinaryOperatorError::RightError(right) => right,
                BinaryOperatorError::EvaluationError(err) => PyRuntimeError::new_err(err.to_string()),
            })
        }
    }

    #[pymethods]
    impl PyImplies {
        #[new]
        fn new(lhs: PyFormula, rhs: PyFormula) -> Self {
            Self(Implies::new(lhs, rhs))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "Next")]
    struct PyNext(Next<PyFormula>);

    impl PyNext {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace)
        }
    }

    #[pymethods]
    impl PyNext {
        #[new]
        fn new(subformula: PyFormula) -> Self {
            Self(Next::new(subformula))
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "Always")]
    struct PyAlways(Always<PyFormula>);

    impl PyAlways {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace).map_err(|err| match err {
                ForwardOperatorError::EmptyInterval => PyValueError::new_err("Bounds interval must not be empty."),
                ForwardOperatorError::EmptySubtraceEvaluation(t) => {
                    PyRuntimeError::new_err(format!("Subtrace at time {} is empty.", t))
                }
                ForwardOperatorError::FormulaError(err) => err,
            })
        }
    }

    #[pymethods]
    impl PyAlways {
        #[new]
        fn new(bounds: Option<(f64, f64)>, subformula: PyFormula) -> Self {
            let inner = if let Some((lo, hi)) = bounds {
                Always::bounded(lo..=hi, subformula)
            } else {
                Always::unbounded(subformula)
            };

            Self(inner)
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pyclass(name = "Eventually")]
    struct PyEventually(Eventually<PyFormula>);

    impl PyEventually {
        fn evaluate_inner(&self, trace: &Trace<Py<PyAny>>) -> PyResult<Trace<PyMetric>> {
            self.0.evaluate(trace).map_err(|err| match err {
                ForwardOperatorError::EmptyInterval => PyValueError::new_err("Bounds interval must not be empty."),
                ForwardOperatorError::EmptySubtraceEvaluation(t) => {
                    PyRuntimeError::new_err(format!("Subtrace at time {} is empty.", t))
                }
                ForwardOperatorError::FormulaError(err) => err,
            })
        }
    }

    #[pymethods]
    impl PyEventually {
        #[new]
        fn new(bounds: Option<(f64, f64)>, subformula: PyFormula) -> Self {
            let inner = if let Some((lo, hi)) = bounds {
                Eventually::bounded(lo..=hi, subformula)
            } else {
                Eventually::unbounded(subformula)
            };

            Self(inner)
        }

        fn evaluate(&self, trace: &Bound<'_, PyTrace>) -> PyResult<PyMetricTrace> {
            self.evaluate_inner(&trace.borrow().0).map(PyMetricTrace)
        }
    }

    #[pymodule_export]
    use pyo3::panic::PanicException;

    #[pymodule_export]
    use crate::metric::PyTop;

    #[pymodule_export]
    use crate::metric::PyBottom;
}
