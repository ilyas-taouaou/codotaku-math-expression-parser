#![allow(dead_code, unused)]
#![forbid(unused_must_use)]

pub use chumsky;
pub use num_traits;

use chumsky::prelude::*;
use std::{borrow::Cow, ops::*};

use num_traits::Pow;
type BinaryOperator<'src> = fn(usize, usize) -> Expression<'src>;

#[derive(Default, Debug)]
pub struct State<'src> {
    pub expressions: Vec<Expression<'src>>,
    pub cleanup: Vec<usize>,
}

type ParserError<'src> = extra::Full<Simple<'src, char>, State<'src>, ()>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression<'src> {
    // Atom
    Number(f64),
    Identifier(&'src str),

    // Unary
    Negate(usize),

    // Binary
    Add(usize, usize),
    Substract(usize, usize),
    Multiply(usize, usize),
    Divide(usize, usize),
    Power(usize, usize),

    // Tuple
    Call((&'src str, Vec<Expression<'src>>)),
}

impl<'src> Expression<'src> {
    pub fn boxed(self, state: &mut State<'src>) -> usize {
        let index = state.expressions.len();
        state.expressions.push(self);
        index
    }
}

// impl<'src> Add for Expression<'src> {
//     type Output = Expression<'src>;

//     fn add(self, rhs: Self) -> Self::Output {
//         Self::Add(self.boxed(), rhs.boxed())
//     }
// }

// impl<'src> Sub for Expression<'src> {
//     type Output = Expression<'src>;

//     fn sub(self, rhs: Self) -> Self::Output {
//         Self::Substract(self.boxed(), rhs.boxed())
//     }
// }

// impl<'src> Mul for Expression<'src> {
//     type Output = Expression<'src>;

//     fn mul(self, rhs: Self) -> Self::Output {
//         Self::Multiply(self.boxed(), rhs.boxed())
//     }
// }

// impl<'src> Div for Expression<'src> {
//     type Output = Expression<'src>;

//     fn div(self, rhs: Self) -> Self::Output {
//         Self::Divide(self.boxed(), rhs.boxed())
//     }
// }

// impl<'src> Neg for Expression<'src> {
//     type Output = Expression<'src>;

//     fn neg(self) -> Self::Output {
//         Self::Negate(self.boxed())
//     }
// }

// impl<'src> Pow<Self> for Expression<'src> {
//     type Output = Expression<'src>;

//     fn pow(self, rhs: Self) -> Self::Output {
//         Self::Power(self.boxed(), rhs.boxed())
//     }
// }

fn binary_parser2<'src>(
    previous_parser: impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone,
    operator1: (char, BinaryOperator<'src>),
    operator2: (char, BinaryOperator<'src>),
) -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone {
    let binary_operator = choice((
        just(operator1.0).to(operator1.1),
        just(operator2.0).to(operator2.1),
    ));
    previous_parser.clone().foldl_with(
        binary_operator.then(previous_parser).repeated(),
        |a, (op, b), e| op(a.boxed(&mut e.state()), b.boxed(&mut e.state())),
    )
}

fn binary_parser<'src>(
    previous_parser: impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone,
    operator1: (char, BinaryOperator<'src>),
) -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone {
    let binary_operator = just(operator1.0).to(operator1.1);
    previous_parser.clone().foldl_with(
        binary_operator.then(previous_parser).repeated(),
        |a, (op, b), e| op(a.boxed(&mut e.state()), b.boxed(&mut e.state())),
    )
}

fn number_parser<'src>() -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone
{
    text::int(10)
        .then(just('.').ignore_then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Expression::Number)
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> {
    use Expression::*;
    let number = number_parser();
    let identifier = text::ident().map(Expression::Identifier).padded();
    recursive(
        |expression: Recursive<
            dyn Parser<&str, Expression<'src>, extra::Full<Simple<'_, char>, State<'_>, ()>>,
        >| {
            let value = choice((
                text::ident()
                    .padded()
                    .then(
                        expression
                            .clone()
                            .separated_by(just(','))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .delimited_by(just('('), just(')')),
                    )
                    .map(Call),
                number
                    .clone()
                    .then(identifier)
                    .map_with(|(number, identifier), e| {
                        Multiply(
                            number.boxed(&mut e.state()),
                            identifier.boxed(&mut e.state()),
                        )
                    }),
                number
                    .clone()
                    .then(expression.clone().delimited_by(just('('), just(')')))
                    .map_with(|(number, expression): (_, Expression<'src>), e| {
                        Multiply(
                            number.boxed(&mut e.state()),
                            expression.boxed(&mut e.state()),
                        )
                    }),
                identifier,
                number,
                expression.delimited_by(just('('), just(')')),
            ))
            .padded();
            let unary_operator = choice((just('-').to(true), just('+').to(false)))
                .repeated()
                .foldr_with(value, |negative, b, e| {
                    if negative {
                        Expression::Negate(b.boxed(&mut e.state()))
                    } else {
                        b
                    }
                });
            let exponent = binary_parser(unary_operator, ('^', Power));
            let product = binary_parser2(exponent, ('*', Multiply), ('/', Divide));
            binary_parser2(product, ('+', Add), ('-', Substract))
        },
    )
}

use std::collections::HashMap;

use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
pub enum EvalError<'src> {
    #[error("undefined variable `{0}`")]
    UndefinedVariable(&'src str),
    #[error("undefined function `{0}`")]
    UndefinedFunction(&'src str),
}

pub fn eval<'src>(
    expression: &Expression<'src>,
    variables: &HashMap<&str, f64>,
    functions: &HashMap<&str, fn(Vec<f64>) -> f64>,
    state: &State<'src>,
) -> Result<f64, EvalError<'src>> {
    use Expression::*;
    Ok(match expression {
        Number(n) => *n,
        Identifier(id) => variables
            .get(id)
            .copied()
            .ok_or(EvalError::UndefinedVariable(id))?,
        Negate(a) => eval(&state.expressions[*a], variables, functions, state).map(|x| -x)?,
        Add(a, b) => {
            eval(&state.expressions[*a], variables, functions, state)?
                + eval(&state.expressions[*b], variables, functions, state)?
        }
        Substract(a, b) => {
            eval(&state.expressions[*a], variables, functions, state)?
                - eval(&state.expressions[*b], variables, functions, state)?
        }
        Multiply(a, b) => {
            eval(&state.expressions[*a], variables, functions, state)?
                * eval(&state.expressions[*b], variables, functions, state)?
        }
        Divide(a, b) => {
            eval(&state.expressions[*a], variables, functions, state)?
                / eval(&state.expressions[*b], variables, functions, state)?
        }
        Power(a, b) => eval(&state.expressions[*a], variables, functions, state)?.powf(eval(
            &state.expressions[*b],
            variables,
            functions,
            state,
        )?),
        Call((name, args)) => functions
            .get(name)
            .map(|f| {
                args.into_iter()
                    .map(|arg| eval(arg, variables, functions, state))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|args| f(args))
            })
            .ok_or(EvalError::UndefinedFunction(name))??,
    })
}

// NOTE: some optimizations here may produce incorrect results if the expression contains side effects
// and if the one of the floating point numbers is NaN or Infinity
pub fn constant_fold<'src>(
    expression: usize,
    variables: &HashMap<&str, f64>,
    state: &mut State<'src>,
) {
    use Expression::*;
    match state.expressions[expression] {
        Number(_) => {}
        Identifier(name) => {
            if let Some(value) = variables.get(name) {
                state.expressions[expression] = Number(*value);
            }
        }
        Negate(a) => {
            constant_fold(a, variables, state);
            if let Number(value) = state.expressions[a] {
                state.cleanup.push(a);
                state.expressions[expression] = Number(-value);
            }
        }
        Add(a, b) => {
            constant_fold(a, variables, state);
            constant_fold(b, variables, state);

            match (&state.expressions[a], &state.expressions[b]) {
                (Number(lhs), Number(rhs)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(lhs + rhs);
                }
                (expr, Number(0.0)) | (Number(0.0), expr) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = expr.clone();
                }
                _ => {}
            }
        }
        Multiply(a, b) => {
            constant_fold(a, variables, state);
            constant_fold(b, variables, state);

            match (&state.expressions[a], &state.expressions[b]) {
                (Number(lhs), Number(rhs)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(lhs * rhs);
                }
                (expr, Number(1.0)) | (Number(1.0), expr) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = expr.clone();
                }
                _ => {}
            }
        }
        Substract(a, b) => {
            constant_fold(a, variables, state);
            constant_fold(b, variables, state);

            match (&state.expressions[a], &state.expressions[b]) {
                (Number(lhs), Number(rhs)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(lhs - rhs);
                }
                (expr, Number(0.0)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = expr.clone();
                }
                _ => {}
            }
        }
        Divide(a, b) => {
            constant_fold(a, variables, state);
            constant_fold(b, variables, state);

            match (&state.expressions[a], &state.expressions[b]) {
                (Number(lhs), Number(rhs)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(lhs / rhs);
                }
                (expr, Number(1.0)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = expr.clone();
                }
                _ => {}
            }
        }
        Power(a, b) => {
            constant_fold(a, variables, state);
            constant_fold(b, variables, state);

            match (&state.expressions[a], &state.expressions[b]) {
                (Number(lhs), Number(rhs)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(lhs.powf(*rhs));
                }
                (_, Number(1.0)) => {
                    state.cleanup.push(b);
                    state.expressions[expression] = state.expressions[a].clone();
                }
                (_, Number(0.0)) => {
                    state.cleanup.push(a);
                    state.cleanup.push(b);
                    state.expressions[expression] = Number(1.0);
                }
                _ => {}
            }
        }
        // Borrow checker problems implementing this
        Call((name, ref args)) => {}
    }
}

impl<'src> State<'src> {
    fn cleanup(&mut self) {}

    pub fn format(&self, expression: &Expression<'src>) -> Cow<'src, str> {
        use Cow::*;
        match expression {
            Expression::Number(value) => Owned(value.to_string()),
            Expression::Identifier(name) => Borrowed(name),
            Expression::Negate(a) => Owned(format!("-{}", self.format(&self.expressions[*a]))),
            Expression::Add(a, b) => Owned(format!(
                "{} + {}",
                self.format(&self.expressions[*a]),
                self.format(&self.expressions[*b])
            )),
            Expression::Substract(a, b) => Owned(format!(
                "{} - {}",
                self.format(&self.expressions[*a]),
                self.format(&self.expressions[*b])
            )),
            Expression::Multiply(a, b) => Owned(format!(
                "{} * {}",
                self.format(&self.expressions[*a]),
                self.format(&self.expressions[*b])
            )),
            Expression::Divide(a, b) => Owned(format!(
                "{} / {}",
                self.format(&self.expressions[*a]),
                self.format(&self.expressions[*b])
            )),
            Expression::Power(a, b) => Owned(format!(
                "{} ^ {}",
                self.format(&self.expressions[*a]),
                self.format(&self.expressions[*b])
            )),
            Expression::Call((name, arguments)) => Owned(format!(
                "{}{:?}",
                name,
                arguments
                    .into_iter()
                    .map(|argument| self.format(argument))
                    .collect::<Vec<_>>()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        use Expression::*;
        let parser = parser();

        let mut state = State::default();

        // assert_eq!(
        //     parser
        //         .parse_with_state("-123.0456", &mut state)
        //         .into_result(),
        //     Ok(-Number(123.0456))
        // );
        // assert_eq!(
        //     parser
        //         .parse_with_state("+123.0456", &mut state)
        //         .into_result(),
        //     Ok(Number(123.0456).into())
        // );
        // assert_eq!(
        //     parser
        //         .parse_with_state("--123.0456", &mut state)
        //         .into_result(),
        //     Ok(--Number(123.0456))
        // );
        // assert_eq!(
        //     parser.parse_with_state("hello", &mut state).into_result(),
        //     Ok(Identifier("hello"))
        // );
        // assert_eq!(
        //     parser.parse_with_state("1+2*3/4", &mut state).into_result(),
        //     Ok(Number(1.0) + Number(2.0) * Number(3.0) / Number(4.0))
        // );
        // assert_eq!(
        //     parser
        //         .parse_with_state("(1+2)*3/4", &mut state)
        //         .into_result(),
        //     Ok((Number(1.0) + Number(2.0)) * Number(3.0) / Number(4.0))
        // );
        // assert_eq!(
        //     parser
        //         .parse_with_state("2(3x+4y^3)", &mut state)
        //         .into_result(),
        //     Ok(Number(2.0)
        //         * (Number(3.0) * Identifier("x")
        //             + (Number(4.0) * Identifier("y")).pow(Number(3.0))))
        // );
        // assert_eq!(
        //     parser
        //         .parse_with_state("2(3x+4y^3)+f(2x+pi,y^3,)", &mut state)
        //         .into_result(),
        //     Ok(Number(2.0)
        //         * (Number(3.0) * Identifier("x")
        //             + (Number(4.0) * Identifier("y")).pow(Number(3.0)))
        //         + Call((
        //             "f",
        //             vec![
        //                 Number(2.0) * Identifier("x") + Identifier("pi"),
        //                 Identifier("y").pow(Number(3.0)),
        //             ]
        //         )))
        // );
        // assert_eq!(
        //     eval(
        //         &parser.parse_with_state("1+2", &mut state).unwrap(),
        //         &Default::default(),
        //         &Default::default(),
        //     ),
        //     Ok(3.0)
        // );
        // assert_eq!(
        //     eval(
        //         &parser.parse_with_state("(1+x)*3/4", &mut state).unwrap(),
        //         &HashMap::from([("x", 2.0)]),
        //         &Default::default(),
        //     ),
        //     Ok(2.25)
        // );
        // // sin(x) + cos(y)
        // let sin = |args: Vec<f64>| args[0].sin();
        // let cos = |args: Vec<f64>| args[0].cos();
        // let x = 4.0;
        // let y = 2.0;
        // type F = fn(Vec<f64>) -> f64;
        // assert_eq!(
        //     eval(
        //         &parser
        //             .parse_with_state("sin(  x  ) +  cos (  y   )", &mut state)
        //             .unwrap(),
        //         &HashMap::from([("x", x), ("y", y)]),
        //         &HashMap::from([("sin", sin as F), ("cos", cos as F)]),
        //     ),
        //     Ok(x.sin() + y.cos())
        // );
    }
}
