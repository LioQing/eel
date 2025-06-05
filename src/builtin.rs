use chumsky::prelude::*;

use crate::{ast, declare::*, lex};

macro_rules! builtin {
    ($name:literal $expr:ident $env:ident [$($param:ident),* $(,)?] => $body:expr) => {
        (
            $name,
            Value::Closure(
                Some($name),
                vec![$(stringify!($param)),*],
                Box::new(Spanned::new(Expr::Builtin(|expr, env| {
                    #[allow(unused_variables)]
                    let $expr = expr;
                    #[allow(unused_variables)]
                    let $env = env;
                    $(
                        let $param = env.iter()
                            .rev()
                            .find(|(y, _)| *y == stringify!($param))
                            .map(|(_, v)| v)
                            .ok_or_else(|| Error {
                                msg: format!("Variable `{}` not found", stringify!($param)),
                                span: expr.1.clone(),
                            })?;
                    )*
                    $body
                }), 0..0)),
                0,
            ),
        )
    };
    ($name:literal $expr:ident $env:ident [$($param:ident),* $(,)?] in $body:literal) => {{
        let src = $body;

        let toks = lex::lexer().parse(src).unwrap();

        let ast = ast::expr_parser()
            .parse_with_state(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                &mut extra::SimpleState(vec![]),
            )
            .unwrap();

        (
            $name,
            Value::Closure(
                Some($name),
                vec![$(stringify!($param)),*],
                Box::new(Spanned::new(ast.0, 0..0)),
                0,
            ),
        )
    }}
}

pub fn env<'src>() -> Env<'src> {
    vec![
        builtin!("print" expr env [v] => {
            println!("{v}");
            Ok(v.clone())
        }),
        builtin!("eprint" expr env [v] => {
            eprintln!("{v}");
            Ok(v.clone())
        }),
        builtin!("+" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(Error {
                    msg: format!("Cannot add {a} and {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("-" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(Error {
                    msg: format!("Cannot subtract {b} from {a}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("*" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(Error {
                    msg: format!("Cannot multiply {a} and {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("/" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Ok(Value::Int(a / b)),
                (Value::Float(a), Value::Float(b)) if *b != 0.0 => Ok(Value::Float(a / b)),
                _ => Err(Error {
                    msg: format!("Cannot divide {a} by {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("%" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Ok(Value::Int(a % b)),
                _ => Err(Error {
                    msg: format!("Cannot modulo {a} by {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("==" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a == b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a == b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for equality"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("!=" expr env [a, b] in "case ==(a, b)() { 0 => 1 _ => 0 }" ),
        builtin!(">" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a > b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a > b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for greater than"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("<" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a < b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a < b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for less than"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!(">=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a >= b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a >= b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for greater than or equal"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("<=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a <= b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a <= b { 1 } else { 0 })),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for less than or equal"),
                    span: expr.1.clone(),
                }),
            }
        }),
    ]
}
