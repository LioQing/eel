use chumsky::prelude::*;

use crate::declare::*;

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

        let toks = crate::lex::lexer().parse(src).unwrap();

        let ast = crate::ast::expr_parser()
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
                (Value::Unit, Value::Unit) => Ok(Value::Int(1)),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for equality"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("!=" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(if a != b { 1 } else { 0 })),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Int(if a != b { 1 } else { 0 })),
                (Value::Unit, Value::Unit) => Ok(Value::Int(0)),
                _ => Err(Error {
                    msg: format!("Cannot compare {a} and {b} for inequality"),
                    span: expr.1.clone(),
                }),
            }
        }),
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{ast, interp, lex};

    fn eval_src(src: &str) -> Result<Value<'_>, Error> {
        let toks = lex::lexer().parse(src).unwrap();
        let expr = ast::expr_parser()
            .parse_with_state(
                toks.as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
                &mut extra::SimpleState(vec![]),
            )
            .unwrap();

        interp::eval(&expr, env())
    }

    #[test]
    fn test_inequality_supported_values() {
        for (a, b, unequal) in [
            ("1", "2", 1),
            ("2", "1", 1),
            ("1", "1", 0),
            ("-2", "-2", 0),
            ("-2", "2", 1),
            ("1.0", "2.0", 1),
            ("1.0", "1.0", 0),
            ("0.0", "-0.0", 0),
            ("unit", "unit", 0),
        ] {
            for (op, expected) in [("!=", unequal), ("==", 1 - unequal)] {
                for src in [
                    format!("{a} {op} {b}"),
                    format!("{op}({a}, {b})."),
                    format!("{op}({a}, {b})()"),
                ] {
                    assert_eq!(eval_src(&src), Ok(Value::Int(expected)), "{src}");
                }
            }
        }
    }

    #[test]
    fn test_inequality_rejects_unsupported_values() {
        for (a, b) in [
            ("1", "1.0"),
            ("1.0", "1"),
            ("unit", "1"),
            ("1", "unit"),
            ("unit", "1.0"),
            ("1.0", "unit"),
            ("(fn(x) { x })", "(fn(x) { x })"),
            ("+(1, 2)", "+(1, 2)"),
            ("(fn(x) { x })", "1"),
            ("1", "+(1, 2)"),
        ] {
            for (op, comparison) in [("!=", "inequality"), ("==", "equality")] {
                for src in [
                    format!("{a} {op} {b}"),
                    format!("{op}({a}, {b})."),
                    format!("{op}({a}, {b})()"),
                ] {
                    let err = eval_src(&src).expect_err(&src);
                    assert!(err.msg.starts_with("Cannot compare "), "{src}: {err:?}");
                    assert!(
                        err.msg.ends_with(&format!(" for {comparison}")),
                        "{src}: {err:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn test_inequality_application_remains_explicit() {
        for src in ["!=(1, 2)", "!=(1, 1.0)"] {
            assert!(
                matches!(eval_src(src), Ok(Value::Struct(Some("!="), _, _, _))),
                "{src}"
            );
        }
    }

    #[test]
    fn test_inequality_does_not_depend_on_shadowed_equality() {
        let src = "let == = fn(a, b) { 0 } 1 != 1";

        assert_eq!(eval_src(src), Ok(Value::Int(0)));
    }
}
