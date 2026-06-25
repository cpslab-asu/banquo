use banquo_core::operators::Interval;
use chumsky::prelude::*;

use crate::ltl;
use crate::num;

pub fn bounds<'src>() -> impl Parser<'src, &'src str, Interval> + Clone {
    num()
        .then_ignore(just(","))
        .then(num())
        .delimited_by(just("{"), just("}"))
        .map(|(start, end)| Interval::from(start..=end))
}

pub fn always<'src>() -> impl Parser<'src, &'src str, Option<Interval>> + Clone {
    ltl::always().ignore_then(bounds().or_not())
}

pub fn eventually<'src>() -> impl Parser<'src, &'src str, Option<Interval>> + Clone {
    ltl::eventually().ignore_then(bounds().or_not())
}
