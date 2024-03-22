use std::collections::HashMap;

use codotaku_math_expression_parser::{chumsky::Parser, constant_fold, parser, State};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create parser
    let parser = parser();

    let mut state = State::default();

    // Parse expression
    let ast = parser
        .parse_with_state("(x^1 + x +0 +0) * x^0 + y", &mut state)
        .unwrap()
        .boxed(&mut state);

    constant_fold(ast, &HashMap::from([("x", 4.0)]), &mut state);

    dbg!(&ast);

    // Evaluate expression
    // let result = eval(
    //     state.expressions.get(ast).unwrap(),
    //     &Default::default(),
    //     &Default::default(),
    //     &state,
    // )?;

    // Test result
    // dbg!(result);
    println!("{:#?}", state);
    println!("{}", state.format(&state.expressions[ast]));

    Ok(())
}
