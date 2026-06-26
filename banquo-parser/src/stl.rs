mod predicate;

use banquo_core::stl;
use chumsky::prelude::*;

use crate::ltl;
use crate::mtl;
use predicate::predicate;

fn formula<'src>() -> impl Parser<'src, &'src str, stl::Formula> {
    recursive(|expr| {
        let predicate = predicate()
            .padded()
            .map(|p| stl::Formula::from(stl::Symbol::Predicate(p)));
        let subformula = expr.delimited_by(just("(").padded(), just(")").padded());
        let atom = predicate.or(subformula);

        let unary_op = choice((
            ltl::neg().to(stl::Symbol::Not),
            ltl::next().to(stl::Symbol::Next),
            mtl::always().map(|bounds| stl::Symbol::Always(bounds)),
            mtl::eventually().map(|bounds| stl::Symbol::Eventually(bounds)),
        ));
        let unary = unary_op.repeated().foldr(atom, |op, formula| {
            stl::Formula::from_iter(std::iter::once(op).chain(formula))
        });

        let binary_op = choice((
            ltl::and().to(stl::Symbol::And),
            ltl::or().to(stl::Symbol::Or),
            ltl::implies().to(stl::Symbol::Implies),
            ltl::until().to(stl::Symbol::Until),
        ));
        let binary = unary.clone().foldl(binary_op.then(unary).repeated(), |lhs, (op, rhs)| {
            stl::Formula::from_iter(std::iter::once(op).chain(lhs).chain(rhs))
        });

        binary
    })
}

#[derive(Debug, PartialEq)]
enum ParseErrorKind {
    Unknown,
}

impl Default for ParseErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct ParseError {
    kind: ParseErrorKind,
}

pub fn parse(phi: &str) -> Result<stl::Formula, ParseError> {
    formula().parse(phi).into_result().map_err(|_| ParseError::default())
}
