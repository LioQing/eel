use std::rc::Rc;

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
                        let $param = env.get(stringify!($param))
                            .map(Rc::as_ref)
                            .ok_or_else(|| Error {
                                msg: format!("Variable `{}` not found", stringify!($param)),
                                span: expr.1.clone(),
                            })?;
                    )*
                    $body
                }), 0..0)),
                Default::default(),
            ),
        )
    };
    ($name:literal $expr:ident $env:ident [$($param:ident),* $(,)?] in $body:literal) => {{
        let src = $body;

        let toks = crate::lex::lexer().parse(src).unwrap();

        let ast = crate::fixity::resolve_parser(crate::parse::expr_parser())
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
                Default::default(),
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
        Patt::Struct(_, args) => {
            args.1 = span;
            for arg in &mut args.0 {
                set_patt_span(arg, span);
            }
        }
        Patt::Var(_) | Patt::Lit(_) => {}
    }
}

pub fn env<'src>() -> Env<'src> {
    [
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
    .into_iter()
    .map(|(name, value)| (name, Rc::new(value)))
    .collect()
}

#[cfg(test)]
mod tests {
    use ariadne::{Label, Report, ReportKind, sources};

    use super::*;
    use crate::interp;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn call<'src>(
        func: Spanned<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Call(Box::new(func), spanned(args)))
    }

    fn eel_builtin<'src>(
        name: &'src str,
        params: Vec<&'src str>,
        body: Spanned<Expr<'src>>,
    ) -> (&'src str, Value<'src>) {
        (
            name,
            Value::Closure(
                Some(name),
                params,
                Box::new((body.0, (0..0).into())),
                Default::default(),
            ),
        )
    }

    fn applications<'src>(
        name: &'src str,
        a: Value<'src>,
        b: Value<'src>,
    ) -> [(Spanned<Expr<'src>>, Span); 2] {
        let infix_span = (10..10 + name.len()).into();
        let prefix_span = (0..name.len()).into();
        let args = vec![spanned(Expr::Lit(a)), spanned(Expr::Lit(b))];
        let infix = spanned(Expr::Op(
            Box::new(args[0].clone()),
            Box::new((Expr::Var(name), infix_span)),
            Box::new(args[1].clone()),
        ));
        let prefix = call(call((Expr::Var(name), prefix_span), args), vec![]);
        [(infix, infix_span), (prefix, prefix_span)]
    }

    fn assert_call_error<'src>(
        src: &'src str,
        expr: Spanned<Expr<'src>>,
        builtin: (&'src str, Value<'src>),
        expected: Span,
        msg: &str,
    ) {
        let env = Env::new().insert(builtin.0, Rc::new(builtin.1));
        let error = interp::eval(&expr, env).unwrap_err();
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
    fn test_eel_builtin_marks_internal_body() {
        let (name, value) = builtin!("b" expr env [a, b] in "a + b");
        let expected = spanned(Expr::Op(
            Box::new((Expr::Var("a"), (0..1).into())),
            Box::new((Expr::Var("+"), (2..3).into())),
            Box::new((Expr::Var("b"), (4..5).into())),
        ));
        assert_eq!(name, "b");
        assert_eq!(
            value,
            Value::Closure(
                Some("b"),
                vec!["a", "b"],
                Box::new(expected),
                Default::default()
            )
        );
    }

    #[test]
    fn test_eel_builtin_errors_use_infix_and_prefix_call_sites() {
        let infix = spanned(Expr::Op(
            Box::new(spanned(Expr::Lit(Value::Int(1)))),
            Box::new((Expr::Var("b"), (2..3).into())),
            Box::new(spanned(Expr::Lit(Value::Int(2)))),
        ));
        let prefix = call(
            call(
                (Expr::Var("b"), (0..1).into()),
                vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(Value::Int(2))),
                ],
            ),
            vec![],
        );
        for (src, expr, span) in [("1 b 2", infix, 2..3), ("b(1, 2)()", prefix, 0..1)] {
            let body = spanned(Expr::Case(
                Box::new((Expr::Var("a"), (5..6).into())),
                (
                    vec![(
                        (Patt::Var("_"), (9..10).into()),
                        Box::new((Expr::Var("missing_internal"), (14..30).into())),
                    )],
                    (7..32).into(),
                ),
            ));
            assert_call_error(
                src,
                expr,
                eel_builtin("b", vec!["a", "b"], body),
                span.into(),
                "Variable `missing_internal` not found",
            );
        }
    }

    #[test]
    fn test_eel_builtin_errors_use_utf8_call_site_boundaries() {
        let infix = spanned(Expr::Op(
            Box::new(spanned(Expr::Lit(Value::Int(1)))),
            Box::new((Expr::Var("🦀"), (2..6).into())),
            Box::new(spanned(Expr::Lit(Value::Int(2)))),
        ));
        let prefix = call(
            call(
                (Expr::Var("🦀"), (0..4).into()),
                vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(Value::Int(2))),
                ],
            ),
            vec![],
        );
        for (src, expr, span) in [("1 🦀 2", infix, 2..6), ("🦀(1, 2)()", prefix, 0..4)] {
            // The internal operator's 2..5 span would split the caller's emoji.
            assert_call_error(
                src,
                expr,
                eel_builtin(
                    "🦀",
                    vec!["a", "b"],
                    spanned(Expr::Op(
                        Box::new((Expr::Var("a"), (0..1).into())),
                        Box::new((Expr::Var("bad"), (2..5).into())),
                        Box::new((Expr::Var("b"), (6..7).into())),
                    )),
                ),
                span.into(),
                "Variable `bad` not found",
            );
        }
    }

    #[test]
    fn test_eel_builtin_remaps_every_ast_span() {
        fn body(span: Span) -> Spanned<Expr<'static>> {
            let var = |name| (Expr::Var(name), span);
            let tail = (
                Patt::Struct("tail", (vec![(Patt::Var("rest"), span)], span)),
                span,
            );
            let pair = (
                Patt::Struct("pair", (vec![(Patt::Var("head"), span), tail], span)),
                span,
            );
            let sum = (
                Expr::Op(
                    Box::new(var("y")),
                    Box::new(var("+")),
                    Box::new(var("head")),
                ),
                span,
            );
            let case = (
                Expr::Case(
                    Box::new(var("x")),
                    (
                        vec![
                            (pair, Box::new(sum)),
                            (
                                (Patt::Lit(Value::Int(0)), span),
                                Box::new((Expr::Lit(Value::Int(1)), span)),
                            ),
                            ((Patt::Var("_"), span), Box::new(var("y"))),
                        ],
                        span,
                    ),
                ),
                span,
            );
            let function = (Expr::Fn((vec!["x", "y"], span), Box::new(case)), span);
            let apply = (
                Expr::Call(Box::new(var("combine")), (vec![var("a"), var("b")], span)),
                span,
            );
            let force = (Expr::Call(Box::new(apply), (vec![], span)), span);
            (
                Expr::Let(None, "combine", Box::new(function), Box::new(force)),
                span,
            )
        }
        let mut original = body((50..60).into());
        original.1 = (0..0).into();
        let call_span = (13..17).into();

        assert_eq!(
            *body_at_call_site(Box::new(original), call_span),
            body(call_span),
        );
        for expr in [
            Expr::Err,
            Expr::Lit(Value::Unit),
            Expr::Builtin(|_, _| Ok(Value::Unit)),
        ] {
            let remapped = body_at_call_site(Box::new(spanned(expr)), call_span);
            assert_eq!(remapped.1, call_span);
        }
    }

    #[test]
    fn test_user_function_body_spans_are_preserved() {
        let body = Box::new((
            Expr::Fn(
                (vec!["x"], (2..5).into()),
                Box::new((
                    Expr::Call(
                        Box::new((Expr::Var("missing"), (8..15).into())),
                        (vec![(Expr::Var("x"), (16..17).into())], (15..18).into()),
                    ),
                    (8..18).into(),
                )),
            ),
            (0..20).into(),
        ));
        assert_eq!(body_at_call_site(body.clone(), (30..34).into()), body);
    }

    #[test]
    fn test_returned_eel_builtin_function_keeps_original_call_site() {
        let expr = spanned(Expr::Let(
            None,
            "f",
            Box::new(call(call((Expr::Var("b"), (8..9).into()), vec![]), vec![])),
            Box::new(call(
                call(
                    (Expr::Var("f"), (14..15).into()),
                    vec![spanned(Expr::Lit(Value::Int(0)))],
                ),
                vec![],
            )),
        ));
        let body = spanned(Expr::Fn(
            (vec!["x"], (2..5).into()),
            Box::new((
                Expr::Call(
                    Box::new((Expr::Var("missing_internal"), (8..24).into())),
                    (vec![(Expr::Var("x"), (25..26).into())], (24..27).into()),
                ),
                (8..27).into(),
            )),
        ));
        assert_call_error(
            "let f = b()() f(0)()",
            expr,
            eel_builtin("b", vec![], body),
            (8..9).into(),
            "Variable `missing_internal` not found",
        );
    }

    #[test]
    fn test_eel_builtin_calls_do_not_reuse_previous_call_spans() {
        let expr = spanned(Expr::Let(
            None,
            "_",
            Box::new(call(
                call(
                    (Expr::Var("b"), (8..9).into()),
                    vec![
                        spanned(Expr::Lit(Value::Int(0))),
                        spanned(Expr::Lit(Value::Int(2))),
                    ],
                ),
                vec![],
            )),
            Box::new(call(
                call(
                    (Expr::Var("b"), (18..19).into()),
                    vec![
                        spanned(Expr::Lit(Value::Int(1))),
                        spanned(Expr::Lit(Value::Int(2))),
                    ],
                ),
                vec![],
            )),
        ));
        let body = spanned(Expr::Case(
            Box::new((Expr::Var("a"), (5..6).into())),
            (
                vec![
                    (
                        (Patt::Lit(Value::Int(0)), (9..10).into()),
                        Box::new((Expr::Lit(Value::Int(0)), (14..15).into())),
                    ),
                    (
                        (Patt::Var("_"), (16..17).into()),
                        Box::new((Expr::Var("missing_internal"), (21..37).into())),
                    ),
                ],
                (7..39).into(),
            ),
        ));
        assert_call_error(
            "let _ = b(0, 2)() b(1, 2)()",
            expr,
            eel_builtin("b", vec!["a", "b"], body),
            (18..19).into(),
            "Variable `missing_internal` not found",
        );
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
            for (expr, _) in applications(op, Value::Int(a), Value::Int(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Ok(Value::Int(expected)),
                    "{a} {op} {b}"
                );
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
            for (expr, span) in applications(op, Value::Int(a), Value::Int(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Err(Error {
                        msg: format!("Integer overflow: {a} {op} {b}"),
                        span,
                    }),
                    "{a} {op} {b}",
                );
            }
        }
    }

    #[test]
    fn test_integer_zero_divisors() {
        for a in [i64::MIN, -1, 0, 1, i64::MAX] {
            for (op, action) in [("/", "divide"), ("%", "modulo")] {
                for (expr, span) in applications(op, Value::Int(a), Value::Int(0)) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert_eq!(err.msg, format!("Cannot {action} {a} by 0"), "{a} {op} 0");
                    assert_eq!(err.span, span);
                }
            }
        }
    }

    #[test]
    fn test_inequality_supported_values() {
        for (a, b, unequal) in [
            (Value::Int(1), Value::Int(2), 1),
            (Value::Int(2), Value::Int(1), 1),
            (Value::Int(1), Value::Int(1), 0),
            (Value::Int(-2), Value::Int(-2), 0),
            (Value::Int(-2), Value::Int(2), 1),
            (Value::Float(1.0), Value::Float(2.0), 1),
            (Value::Float(1.0), Value::Float(1.0), 0),
            (Value::Float(0.0), Value::Float(-0.0), 0),
            (Value::Unit, Value::Unit, 0),
        ] {
            for (op, expected) in [("!=", unequal), ("==", 1 - unequal)] {
                for (expr, _) in applications(op, a.clone(), b.clone()) {
                    assert_eq!(
                        interp::eval(&expr, env()),
                        Ok(Value::Int(expected)),
                        "{a} {op} {b}"
                    );
                }
            }
        }
    }

    #[test]
    fn test_overflow_in_deferred_and_nested_evaluation() {
        let max = spanned(Expr::Lit(Value::Int(i64::MAX)));
        let one = spanned(Expr::Lit(Value::Int(1)));
        let overflow = applications("+", Value::Int(i64::MAX), Value::Int(1))[0]
            .0
            .clone();
        let deferred = spanned(Expr::Let(
            None,
            "sum",
            Box::new(call(
                (Expr::Var("+"), (5..6).into()),
                vec![max.clone(), one.clone()],
            )),
            Box::new(call(spanned(Expr::Var("sum")), vec![])),
        ));
        let variables = spanned(Expr::Let(
            None,
            "x",
            Box::new(max.clone()),
            Box::new(spanned(Expr::Let(
                None,
                "y",
                Box::new(one),
                Box::new(spanned(Expr::Op(
                    Box::new(spanned(Expr::Var("x"))),
                    Box::new((Expr::Var("+"), (10..11).into())),
                    Box::new(spanned(Expr::Var("y"))),
                ))),
            ))),
        ));
        let function = spanned(Expr::Let(
            None,
            "f",
            Box::new((
                Expr::Fn(
                    spanned(vec!["x"]),
                    Box::new((
                        Expr::Op(
                            Box::new(spanned(Expr::Var("x"))),
                            Box::new((Expr::Var("*"), (20..21).into())),
                            Box::new(spanned(Expr::Lit(Value::Int(2)))),
                        ),
                        (18..23).into(),
                    )),
                ),
                (10..25).into(),
            )),
            Box::new(call(call(spanned(Expr::Var("f")), vec![max]), vec![])),
        ));
        let nested = spanned(Expr::Op(
            Box::new(spanned(Expr::Lit(Value::Int(0)))),
            Box::new((Expr::Var("+"), (2..3).into())),
            Box::new(overflow),
        ));
        for expr in [variables, deferred, function, nested] {
            let err = interp::eval(&expr, env()).unwrap_err();
            assert!(
                err.msg.starts_with("Integer overflow:"),
                "{expr:?}: {err:?}"
            );
            assert!(err.span.start < err.span.end, "{expr:?}: {err:?}");
        }
    }

    #[test]
    fn test_float_arithmetic_unchanged() {
        for (a, op, b, expected) in [
            (1.5, "+", 2.5, 4.0),
            (1.5, "-", 2.5, -1.0),
            (1.5, "*", 2.0, 3.0),
            (7.0, "/", -2.0, -3.5),
            (1e308, "*", 1e308, f64::INFINITY),
            (-1e308, "*", 1e308, f64::NEG_INFINITY),
        ] {
            for (expr, _) in applications(op, Value::Float(a), Value::Float(b)) {
                assert_eq!(
                    interp::eval(&expr, env()),
                    Ok(Value::Float(expected)),
                    "{a} {op} {b}"
                );
            }
        }

        assert!(matches!(
            interp::eval(&applications("-", Value::Float(f64::INFINITY), Value::Float(f64::INFINITY))[0].0, env()),
            Ok(Value::Float(value)) if value.is_nan()
        ));

        for (a, b) in [(1.0, 0.0), (1.0, -0.0), (0.0, 0.0)] {
            for (expr, span) in applications("/", Value::Float(a), Value::Float(b)) {
                let err = interp::eval(&expr, env()).unwrap_err();
                assert!(err.msg.starts_with("Cannot divide"), "{a} / {b}: {err:?}");
                assert_eq!(err.span, span);
            }
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
            for (a, b) in [
                (Value::Int(1), Value::Float(2.0)),
                (Value::Float(1.0), Value::Int(2)),
                (Value::Unit, Value::Int(1)),
                (Value::Int(1), Value::Unit),
            ] {
                for (expr, span) in applications(op, a.clone(), b.clone()) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert!(
                        err.msg.starts_with(&format!("Cannot {action}")),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert_eq!(err.span, span);
                }
            }
        }

        assert_eq!(
            interp::eval(
                &applications("%", Value::Float(1.0), Value::Float(2.0))[0].0,
                env()
            )
            .unwrap_err()
            .msg,
            "Cannot modulo 1 by 2",
        );
    }

    #[test]
    fn test_inequality_rejects_unsupported_values() {
        let closure = Value::Closure(
            None,
            vec!["x"],
            Box::new(spanned(Expr::Var("x"))),
            Default::default(),
        );
        let args: StructArgs<'_> =
            Rc::from([("a", Rc::new(Value::Int(1))), ("b", Rc::new(Value::Int(2)))]);
        let applied_env = args.iter().cloned().collect();
        let deferred = Value::Struct(
            Some("+"),
            Box::new(spanned(Expr::Lit(Value::Int(3)))),
            args,
            applied_env,
        );
        for (a, b) in [
            (Value::Int(1), Value::Float(1.0)),
            (Value::Float(1.0), Value::Int(1)),
            (Value::Unit, Value::Int(1)),
            (Value::Int(1), Value::Unit),
            (Value::Unit, Value::Float(1.0)),
            (Value::Float(1.0), Value::Unit),
            (closure.clone(), closure.clone()),
            (deferred.clone(), deferred.clone()),
            (closure, Value::Int(1)),
            (Value::Int(1), deferred),
        ] {
            for (op, comparison) in [("!=", "inequality"), ("==", "equality")] {
                for (expr, span) in applications(op, a.clone(), b.clone()) {
                    let err = interp::eval(&expr, env()).unwrap_err();
                    assert!(
                        err.msg.starts_with("Cannot compare "),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert!(
                        err.msg.ends_with(&format!(" for {comparison}")),
                        "{a} {op} {b}: {err:?}"
                    );
                    assert_eq!(err.span, span);
                }
            }
        }
    }

    #[test]
    fn test_inequality_application_remains_explicit() {
        for value in [Value::Int(2), Value::Float(1.0)] {
            let expr = call(
                spanned(Expr::Var("!=")),
                vec![
                    spanned(Expr::Lit(Value::Int(1))),
                    spanned(Expr::Lit(value.clone())),
                ],
            );
            let result = interp::eval(&expr, env());
            assert!(
                matches!(result, Ok(Value::Struct(Some("!="), _, ref args, _))
                    if args.iter().map(|(name, v)| (*name, v.as_ref()))
                        .eq([("a", &Value::Int(1)), ("b", &value)])),
                "{expr:?}: {result:?}"
            );
        }
    }

    #[test]
    fn test_inequality_does_not_depend_on_shadowed_equality() {
        let equality = spanned(Expr::Fn(
            spanned(vec!["a", "b"]),
            Box::new(spanned(Expr::Lit(Value::Int(0)))),
        ));
        let inequality = applications("!=", Value::Int(1), Value::Int(1))[0]
            .0
            .clone();
        let expr = spanned(Expr::Let(
            None,
            "==",
            Box::new(equality),
            Box::new(inequality),
        ));
        assert_eq!(interp::eval(&expr, env()), Ok(Value::Int(0)));
    }
}
