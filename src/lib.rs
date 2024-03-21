// #![allow(dead_code, unused)]
#![forbid(unused_must_use)]

pub use chumsky;
pub use num_traits;

use chumsky::prelude::*;
use std::ops::*;

use num_traits::Pow;
type BinaryOperator<'src> = fn(Box<Expression<'src>>, Box<Expression<'src>>) -> Expression<'src>;

#[derive(Default)]
pub struct State {}

type ParserError<'src> = extra::Full<Simple<'src, char>, State, ()>;

#[derive(PartialEq, Debug)]
pub enum Expression<'src> {
    // Atom
    Number(f64),
    Identifier(&'src str),

    // Unary
    Negate(Box<Expression<'src>>),

    // Binary
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Substract(Box<Expression<'src>>, Box<Expression<'src>>),
    Multiply(Box<Expression<'src>>, Box<Expression<'src>>),
    Divide(Box<Expression<'src>>, Box<Expression<'src>>),
    Power(Box<Expression<'src>>, Box<Expression<'src>>),

    // Tuple
    Call((&'src str, Vec<Expression<'src>>)),
}

impl<'src> Expression<'src> {
    fn boxed(self) -> Box<Expression<'src>> {
        self.into()
    }
}

impl<'src> Add for Expression<'src> {
    type Output = Expression<'src>;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Add(self.boxed(), rhs.boxed())
    }
}

impl<'src> Sub for Expression<'src> {
    type Output = Expression<'src>;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Substract(self.boxed(), rhs.boxed())
    }
}

impl<'src> Mul for Expression<'src> {
    type Output = Expression<'src>;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Multiply(self.boxed(), rhs.boxed())
    }
}

impl<'src> Div for Expression<'src> {
    type Output = Expression<'src>;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Divide(self.boxed(), rhs.boxed())
    }
}

impl<'src> Neg for Expression<'src> {
    type Output = Expression<'src>;

    fn neg(self) -> Self::Output {
        Self::Negate(self.boxed())
    }
}

impl<'src> Pow<Self> for Expression<'src> {
    type Output = Expression<'src>;

    fn pow(self, rhs: Self) -> Self::Output {
        Self::Power(self.boxed(), rhs.boxed())
    }
}

fn binary_parser2<'src>(
    previous_parser: impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone,
    operator1: (char, BinaryOperator<'src>),
    operator2: (char, BinaryOperator<'src>),
) -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone {
    let binary_operator = choice((
        just(operator1.0).to(operator1.1),
        just(operator2.0).to(operator2.1),
    ));
    previous_parser.clone().foldl(
        binary_operator.then(previous_parser).repeated(),
        |a, (op, b)| op(Box::new(a), Box::new(b)),
    )
}

fn binary_parser<'src>(
    previous_parser: impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone,
    operator1: (char, BinaryOperator<'src>),
) -> impl Parser<'src, &'src str, Expression<'src>, ParserError<'src>> + Clone {
    let binary_operator = just(operator1.0).to(operator1.1);
    previous_parser.clone().foldl(
        binary_operator.then(previous_parser).repeated(),
        |a, (op, b)| op(Box::new(a), Box::new(b)),
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
    recursive(|expression| {
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
                .map(|(number, identifier)| Multiply(number.boxed(), identifier.boxed())),
            number
                .clone()
                .then(expression.clone().delimited_by(just('('), just(')')))
                .map(|(number, expression): (_, Expression<'src>)| {
                    Multiply(number.boxed(), expression.boxed())
                }),
            identifier,
            number,
            expression.delimited_by(just('('), just(')')),
        ))
        .padded();
        let unary_operator = choice((just('-').to(true), just('+').to(false)))
            .repeated()
            .foldr(value, |negative, b| {
                if negative {
                    Expression::Negate(b.boxed())
                } else {
                    b
                }
            });
        let exponent = binary_parser(unary_operator, ('^', Power));
        let product = binary_parser2(exponent, ('*', Multiply), ('/', Divide));
        binary_parser2(product, ('+', Add), ('-', Substract))
    })
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
) -> Result<f64, EvalError<'src>> {
    use Expression::*;
    Ok(match expression {
        Number(n) => *n,
        Identifier(id) => variables
            .get(id)
            .copied()
            .ok_or(EvalError::UndefinedVariable(id))?,
        Negate(a) => eval(a, variables, functions).map(|x| -x)?,
        Add(a, b) => eval(a, variables, functions)? + eval(b, variables, functions)?,
        Substract(a, b) => eval(a, variables, functions)? - eval(b, variables, functions)?,
        Multiply(a, b) => eval(a, variables, functions)? * eval(b, variables, functions)?,
        Divide(a, b) => eval(a, variables, functions)? / eval(b, variables, functions)?,
        Power(a, b) => eval(a, variables, functions)?.powf(eval(b, variables, functions)?),
        Call((name, args)) => functions
            .get(name)
            .map(|f| {
                args.iter()
                    .map(|arg| eval(arg, variables, functions))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|args| f(args))
            })
            .ok_or(EvalError::UndefinedFunction(name))??,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        use Expression::*;
        let parser = parser();

        let mut state = State::default();

        assert_eq!(
            parser
                .parse_with_state("-123.0456", &mut state)
                .into_result(),
            Ok(-Number(123.0456))
        );
        assert_eq!(
            parser
                .parse_with_state("+123.0456", &mut state)
                .into_result(),
            Ok(Number(123.0456).into())
        );
        assert_eq!(
            parser
                .parse_with_state("--123.0456", &mut state)
                .into_result(),
            Ok(--Number(123.0456))
        );
        assert_eq!(
            parser.parse_with_state("hello", &mut state).into_result(),
            Ok(Identifier("hello"))
        );
        assert_eq!(
            parser.parse_with_state("1+2*3/4", &mut state).into_result(),
            Ok(Number(1.0) + Number(2.0) * Number(3.0) / Number(4.0))
        );
        assert_eq!(
            parser
                .parse_with_state("(1+2)*3/4", &mut state)
                .into_result(),
            Ok((Number(1.0) + Number(2.0)) * Number(3.0) / Number(4.0))
        );
        assert_eq!(
            parser
                .parse_with_state("2(3x+4y^3)", &mut state)
                .into_result(),
            Ok(Number(2.0)
                * (Number(3.0) * Identifier("x")
                    + (Number(4.0) * Identifier("y")).pow(Number(3.0))))
        );
        assert_eq!(
            parser
                .parse_with_state("2(3x+4y^3)+f(2x+pi,y^3,)", &mut state)
                .into_result(),
            Ok(Number(2.0)
                * (Number(3.0) * Identifier("x")
                    + (Number(4.0) * Identifier("y")).pow(Number(3.0)))
                + Call((
                    "f",
                    vec![
                        Number(2.0) * Identifier("x") + Identifier("pi"),
                        Identifier("y").pow(Number(3.0)),
                    ]
                )))
        );
        assert_eq!(
            eval(
                &parser.parse_with_state("1+2", &mut state).unwrap(),
                &Default::default(),
                &Default::default(),
            ),
            Ok(3.0)
        );
        assert_eq!(
            eval(
                &parser.parse_with_state("(1+x)*3/4", &mut state).unwrap(),
                &HashMap::from([("x", 2.0)]),
                &Default::default(),
            ),
            Ok(2.25)
        );
        // sin(x) + cos(y)
        let sin = |args: Vec<f64>| args[0].sin();
        let cos = |args: Vec<f64>| args[0].cos();
        let x = 4.0;
        let y = 2.0;
        type F = fn(Vec<f64>) -> f64;
        assert_eq!(
            eval(
                &parser
                    .parse_with_state("sin(  x  ) +  cos (  y   )", &mut state)
                    .unwrap(),
                &HashMap::from([("x", x), ("y", y)]),
                &HashMap::from([("sin", sin as F), ("cos", cos as F)]),
            ),
            Ok(x.sin() + y.cos())
        );
    }
}
