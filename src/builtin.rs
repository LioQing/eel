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

pub fn body_at_call_site<'src>(
    mut body: Box<Spanned<Expr<'src>>>,
    span: Span,
) -> Box<Spanned<Expr<'src>>> {
    // Builtin bodies use an empty outer span to mark their internal source.
    // None of their descendant spans may be interpreted as user-source offsets.
    if body.1.end == 0 {
        set_expr_span(&mut body, span);
    }
    body
}

fn set_expr_span(expr: &mut Spanned<Expr<'_>>, span: Span) {
    expr.1 = span;
    match &mut expr.0 {
        Expr::Call(func, args) => {
            set_expr_span(func, span);
            args.1 = span;
            for arg in &mut args.0 {
                set_expr_span(arg, span);
            }
        }
        Expr::Fn(params, body) => {
            params.1 = span;
            set_expr_span(body, span);
        }
        Expr::Case(expr, arms) => {
            set_expr_span(expr, span);
            arms.1 = span;
            for (patt, body) in &mut arms.0 {
                set_patt_span(patt, span);
                set_expr_span(body, span);
            }
        }
        Expr::Op(left, op, right) => {
            set_expr_span(left, span);
            set_expr_span(op, span);
            set_expr_span(right, span);
        }
        Expr::Let(_, _, assign, body) => {
            set_expr_span(assign, span);
            set_expr_span(body, span);
        }
        Expr::Err | Expr::Lit(_) | Expr::Var(_) | Expr::Builtin(_) => {}
    }
}

fn set_patt_span(patt: &mut Spanned<Patt<'_>>, span: Span) {
    patt.1 = span;
    match &mut patt.0 {
        Patt::Struct(_, fields) => {
            fields.1 = span;
            for field in &mut fields.0 {
                set_patt_span(field, span);
            }
        }
        Patt::Var(_) | Patt::Lit(_) => {}
    }
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
    use ariadne::{Label, Report, ReportKind, sources};

    use super::*;
    use crate::interp;

    fn parse<'src>(src: &'src str, span: Option<Span>) -> Spanned<Expr<'src>> {
        let toks = lex::lexer().parse(src).unwrap();
        ast::expr_parser()
            .parse_with_state(
                toks.as_slice().map(
                    span.unwrap_or_else(|| (src.len()..src.len()).into()),
                    |(t, s)| (t, span.as_ref().unwrap_or(s)),
                ),
                &mut extra::SimpleState(vec![]),
            )
            .unwrap()
    }

    fn assert_call_error<'src>(
        src: &'src str,
        builtin: (&'src str, Value<'src>),
        expected: Span,
        msg: &str,
    ) {
        let error = interp::eval(&parse(src, None), vec![builtin]).unwrap_err();
        assert_eq!(error.msg, msg);
        assert_eq!(error.span, expected);
        assert!(src.get(error.span.into_range()).is_some());

        // Exercise Ariadne's byte-indexed rendering, not only the error payload.
        let mut output = Vec::new();
        Report::build(ReportKind::Error, ("caller", error.span.into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(&error.msg)
            .with_label(Label::new(("caller", error.span.into_range())).with_message(&error.msg))
            .finish()
            .write(sources([("caller", src)]), &mut output)
            .unwrap();
        assert!(String::from_utf8(output).unwrap().contains(msg));
    }

    #[test]
    fn eel_builtin_errors_use_infix_and_prefix_call_sites() {
        for (src, span) in [("1 b 2", 2..3), ("b(1, 2)()", 0..1)] {
            assert_call_error(
                src,
                builtin!("b" expr env [a, b] in "case a { _ => missing_internal }"),
                span.into(),
                "Variable `missing_internal` not found",
            );
        }
    }

    #[test]
    fn eel_builtin_errors_use_utf8_call_site_boundaries() {
        for (src, span) in [("1 🦀 2", 2..6), ("🦀(1, 2)()", 0..4)] {
            // The internal operator's 2..5 span would split the caller's emoji.
            assert_call_error(
                src,
                builtin!("🦀" expr env [a, b] in "a bad b"),
                span.into(),
                "Variable `bad` not found",
            );
        }
    }

    #[test]
    fn eel_builtin_remaps_every_parsed_ast_span() {
        let src = "
            let combine = fn(x, y) {
                case x {
                    pair(head, tail(rest)) => { y + head }
                    0 => 1
                    _ => y
                }
            }
            combine(a, b)()
        ";
        let mut body = parse(src, None);
        body.1 = (0..0).into();
        let call_span = (13..17).into();

        assert_eq!(
            *body_at_call_site(Box::new(body), call_span),
            parse(src, Some(call_span)),
        );
    }

    #[test]
    fn user_function_body_spans_are_preserved() {
        let body = Box::new(parse("fn(x) { missing(x) }", None));
        assert_eq!(body_at_call_site(body.clone(), (30..34).into()), body);
    }

    #[test]
    fn returned_eel_builtin_function_keeps_original_call_site() {
        assert_call_error(
            "let f = b()() f(0)()",
            builtin!("b" expr env [] in "fn(x) { missing_internal(x) }"),
            (8..9).into(),
            "Variable `missing_internal` not found",
        );
    }

    #[test]
    fn eel_builtin_calls_do_not_reuse_previous_call_spans() {
        assert_call_error(
            "let _ = b(0, 2)() b(1, 2)()",
            builtin!("b" expr env [a, b] in "case a { 0 => 0 _ => missing_internal }"),
            (18..19).into(),
            "Variable `missing_internal` not found",
        );
    }
}
