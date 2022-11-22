use crate::trace::Trace;

#[derive(Clone, Debug)]
pub struct TemporalOperator<F> {
    pub subformula: F,
    pub t_bounds: Option<(usize, usize)>,
}

impl<F> TemporalOperator<F> {
    pub fn apply_subtrace<S, T, E, F1, U, F2>(
        &self,
        trace: &Trace<S>,
        eval_subformula: F1,
        fold: F2,
    ) -> Result<Trace<U>, E>
    where
        F1: Fn(&F, &Trace<S>) -> Result<Trace<T>, E>,
        F2: Fn(Trace<&T>) -> U,
    {
        let subformula_trace = eval_subformula(&self.subformula, trace)?;
        let robustness_for_time = |time| -> (usize, U) {
            let subtrace = match self.t_bounds {
                Some((t_start, t_end)) => subformula_trace.range((time + t_start)..=(time + t_end)),
                None => subformula_trace.range(time..),
            };

            let robustness = fold(subtrace);

            (time, robustness)
        };

        let robustness = subformula_trace.times().map(robustness_for_time).collect();

        Ok(robustness)
    }

    pub fn apply<S, T, E, F1, F2>(
        &self,
        trace: &Trace<S>,
        eval_subformula: F1,
        initial: T,
        combine: F2,
    ) -> Result<Trace<T>, E>
    where
        T: Clone,
        F1: Fn(&F, &Trace<S>) -> Result<Trace<T>, E>,
        F2: Fn(T, &T) -> T,
    {
        let fold = |subtrace: Trace<&T>| subtrace.states().fold(initial.clone(), |r1, &r2| combine(r1, r2));

        self.apply_subtrace(trace, eval_subformula, fold)
    }
}
