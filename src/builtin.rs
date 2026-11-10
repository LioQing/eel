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
                (Value::Int(a), Value::Int(b)) => a.checked_add(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} + {b}"),
                        span: expr.1.clone(),
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(Error {
                    msg: format!("Cannot add {a} and {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("-" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.checked_sub(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} - {b}"),
                        span: expr.1.clone(),
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(Error {
                    msg: format!("Cannot subtract {b} from {a}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("*" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.checked_mul(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} * {b}"),
                        span: expr.1.clone(),
                    }),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(Error {
                    msg: format!("Cannot multiply {a} and {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("/" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => a.checked_div(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} / {b}"),
                        span: expr.1.clone(),
                    }),
                (Value::Float(a), Value::Float(b)) if *b != 0.0 => Ok(Value::Float(a / b)),
                _ => Err(Error {
                    msg: format!("Cannot divide {a} by {b}"),
                    span: expr.1.clone(),
                }),
            }
        }),
        builtin!("%" expr env [a, b] => {
            match (a, b) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => a.checked_rem(*b)
                    .map(Value::Int)
                    .ok_or_else(|| Error {
                        msg: format!("Integer overflow: {a} % {b}"),
                        span: expr.1.clone(),
                    }),
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

#[cfg(test)]
mod tests {
    use crate::interp;

    use super::*;

    fn eval_src<'src>(src: &'src str) -> Result<Value<'src>, Error> {
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
    fn test_integer_arithmetic_boundaries() {
        for (a, op, b, expected) in [
            (i64::MAX, "+", 0, i64::MAX),
            (i64::MIN, "+", 0, i64::MIN),
            (i64::MAX, "+", i64::MIN, -1),
            (i64::MAX, "+", -1, i64::MAX - 1),
            (i64::MIN, "+", 1, i64::MIN + 1),
            (i64::MAX, "-", 0, i64::MAX),
            (i64::MIN, "-", 0, i64::MIN),
            (-1, "-", i64::MAX, i64::MIN),
            (i64::MIN, "-", -1, i64::MIN + 1),
            (i64::MAX, "-", i64::MAX, 0),
            (i64::MAX, "*", 1, i64::MAX),
            (i64::MIN, "*", 1, i64::MIN),
            (i64::MAX, "*", 0, 0),
            (i64::MIN, "*", 0, 0),
            (i64::MAX, "*", -1, -i64::MAX),
            (i64::MIN, "/", 1, i64::MIN),
            (i64::MAX, "/", -1, -i64::MAX),
            (i64::MIN, "/", i64::MIN, 1),
            (0, "/", -1, 0),
            (7, "/", -3, -2),
            (-7, "/", 3, -2),
            (-7, "/", -3, 2),
            (i64::MIN, "%", 1, 0),
            (i64::MIN, "%", i64::MIN, 0),
            (i64::MAX, "%", -1, 0),
            (0, "%", -1, 0),
            (-7, "%", 3, -1),
            (7, "%", -3, 1),
        ] {
            for src in [format!("{a} {op} {b}"), format!("{op}({a}, {b}).")] {
                assert_eq!(eval_src(&src), Ok(Value::Int(expected)), "{src}");
            }
        }
    }

    #[test]
    fn test_integer_arithmetic_overflow() {
        for (a, op, b) in [
            (i64::MAX, "+", 1),
            (1, "+", i64::MAX),
            (i64::MIN, "+", -1),
            (-1, "+", i64::MIN),
            (i64::MAX, "+", i64::MAX),
            (i64::MIN, "+", i64::MIN),
            (i64::MIN, "-", 1),
            (i64::MAX, "-", -1),
            (0, "-", i64::MIN),
            (i64::MAX, "-", i64::MIN),
            (i64::MAX, "*", 2),
            (2, "*", i64::MAX),
            (i64::MIN, "*", 2),
            (i64::MIN, "*", -1),
            (-1, "*", i64::MIN),
            (i64::MIN, "*", i64::MIN),
            (i64::MIN, "/", -1),
            (i64::MIN, "%", -1),
        ] {
            let infix_span = a.to_string().len() + 1..a.to_string().len() + 1 + op.len();
            for (src, span) in [
                (format!("{a} {op} {b}"), infix_span),
                (format!("{op}({a}, {b})."), 0..op.len()),
            ] {
                assert_eq!(
                    eval_src(&src),
                    Err(Error {
                        msg: format!("Integer overflow: {a} {op} {b}"),
                        span: span.into(),
                    }),
                    "{src}",
                );
            }
        }
    }

    #[test]
    fn test_integer_zero_divisors() {
        for a in [i64::MIN, -1, 0, 1, i64::MAX] {
            for (op, action) in [("/", "divide"), ("%", "modulo")] {
                for src in [format!("{a} {op} 0"), format!("{op}({a}, 0).")] {
                    let err = eval_src(&src).unwrap_err();
                    assert_eq!(err.msg, format!("Cannot {action} {a} by 0"), "{src}");
                }
            }
        }
    }

    #[test]
    fn test_overflow_in_deferred_and_nested_evaluation() {
        for src in [
            "let x = 9223372036854775807 let y = 1 x + y",
            "let add = + let sum = add(9223372036854775807, 1) sum.",
            "let f = fn(x) { x * 2 } f(9223372036854775807).",
            "0 + (9223372036854775807 + 1)",
        ] {
            let err = eval_src(src).unwrap_err();
            assert!(err.msg.starts_with("Integer overflow:"), "{src}: {err:?}");
            assert!(err.span.start < err.span.end, "{src}: {err:?}");
        }
    }

    #[test]
    fn test_float_arithmetic_unchanged() {
        for (src, expected) in [
            ("1.5 + 2.5", 4.0),
            ("1.5 - 2.5", -1.0),
            ("1.5 * 2.0", 3.0),
            ("7.0 / -2.0", -3.5),
            ("1e308 * 1e308", f64::INFINITY),
            ("-1e308 * 1e308", f64::NEG_INFINITY),
        ] {
            assert_eq!(eval_src(src), Ok(Value::Float(expected)), "{src}");
        }

        assert!(matches!(
            eval_src("1e999 - 1e999"),
            Ok(Value::Float(value)) if value.is_nan()
        ));

        for src in ["1.0 / 0.0", "1.0 / -0.0", "0.0 / 0.0"] {
            let err = eval_src(src).unwrap_err();
            assert!(err.msg.starts_with("Cannot divide"), "{src}: {err:?}");
        }
    }

    #[test]
    fn test_arithmetic_type_policy_unchanged() {
        for (op, action) in [
            ("+", "add"),
            ("-", "subtract"),
            ("*", "multiply"),
            ("/", "divide"),
            ("%", "modulo"),
        ] {
            for (a, b) in [("1", "2.0"), ("1.0", "2"), ("unit", "1"), ("1", "unit")] {
                let src = format!("{a} {op} {b}");
                let err = eval_src(&src).unwrap_err();
                assert!(
                    err.msg.starts_with(&format!("Cannot {action}")),
                    "{src}: {err:?}",
                );
            }
        }

        assert_eq!(
            eval_src("1.0 % 2.0").unwrap_err().msg,
            "Cannot modulo 1 by 2",
        );
    }
}
