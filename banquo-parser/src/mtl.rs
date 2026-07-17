use banquo_core::operators::Interval;
use chumsky::prelude::*;

use crate::ltl;
use crate::{num, Err};

pub fn bounds<'src>() -> impl Parser<'src, &'src str, Interval, Err<'src>> + Clone {
    num()
        .then_ignore(just(","))
        .then(num())
        .delimited_by(just("{"), just("}"))
        .map(|(start, end)| Interval::from(start..=end))
}

pub fn always<'src>() -> impl Parser<'src, &'src str, Option<Interval>, Err<'src>> + Clone {
    ltl::always().ignore_then(bounds().or_not()).padded()
}

pub fn eventually<'src>() -> impl Parser<'src, &'src str, Option<Interval>, Err<'src>> + Clone {
    ltl::eventually().ignore_then(bounds().or_not()).padded()
}
