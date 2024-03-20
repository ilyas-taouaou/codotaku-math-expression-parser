use std::{collections::HashMap, f64::consts::PI};

use codotaku_math_expression_parser::{chumsky::Parser, eval, parser, State};

type F = fn(Vec<f64>) -> f64;

fn main() {
    // Create parser
    let parser = parser();

    // Parse expression
    let ast = parser
        .parse_with_state("sin(2x)^2 + cos(3y)^pi", &mut State::default())
        .unwrap();

    dbg!(&ast);

    // Define variables
    let x = 4.0;
    let y = 2.0;
    let variables = HashMap::from([("x", x), ("y", y), ("pi", PI)]);

    // Define functions
    let sin = |args: Vec<f64>| args[0].sin();
    let cos = |args: Vec<f64>| args[0].cos();
    let functions = HashMap::from([("sin", sin as F), ("cos", cos as F)]);

    // Evaluate expression
    let result = eval(&ast, &variables, &functions).unwrap();

    // Test result
    assert_eq!(result, (2.0 * x).sin().powf(2.0) + (3.0 * y).cos().powf(PI));
    dbg!(result);
}
