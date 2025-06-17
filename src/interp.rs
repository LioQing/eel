use crate::declare::*;

pub fn eval<'src>(expr: &Spanned<Expr<'src>>, env: Env<'src>) -> Result<Value<'src>, Error> {
    Ok(match &expr.0 {
        Expr::Err => unreachable!(), // Valid AST does not contain Expr::Err
        Expr::Lit(v) => v.clone(),
        Expr::Var(x) => env
            .iter()
            .rev()
            .find(|(y, _)| y == x)
            .map(|(_, v)| v.clone())
            .ok_or_else(|| Error {
                msg: format!("Variable `{x}` not found"),
                span: expr.1.clone(),
            })?,
        Expr::Call(f, args) => match eval(f, env.clone())? {
            Value::Closure(name, params, body, captured_env) => {
                if params.len() != args.0.len() {
                    return Err(Error {
                        msg: format!(
                            "Expected {} arguments for function {}, but got {}",
                            params.len(),
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            args.0.len(),
                        ),
                        span: expr.1.clone(),
                    });
                }

                let body = match body.1.end {
                    0 => Box::new((body.0, f.1.clone())), // Builtin functions have no span
                    _ => body.clone(),
                };

                let args_env = params
                    .iter()
                    .zip(args.0.iter())
                    .map(|(param, arg)| Ok((*param, eval(arg, env.clone())?)))
                    .collect::<Result<_, _>>()?;

                Value::Struct(name, body, args_env, captured_env)
            }
            Value::Struct(name, body, args_env, captured_env) => {
                if !args.0.is_empty() {
                    return Err(Error {
                        msg: format!(
                            "Expected no argument for struct {}, but got {}",
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            args.0.len()
                        ),
                        span: expr.1.clone(),
                    });
                }

                let mut new_env = env[..captured_env].to_vec();
                new_env.extend(args_env);

                eval(&body, new_env)?
            }
            f => {
                return Err(Error {
                    msg: format!("Expected a function or struct, but got {f}"),
                    span: expr.1.clone(),
                });
            }
        },
        Expr::Fn(params, body) => Value::Closure(None, params.0.clone(), body.clone(), env.len()),
        Expr::Case(e, arms) => {
            let v = eval(e, env.clone())?;

            let (patt_env, body) = arms
                .0
                .iter()
                .find_map(|(patt, body)| match_patt(&patt.0, &v).map(|env| (env, body)))
                .ok_or_else(|| Error {
                    msg: format!("No pattern matched for value {v}"),
                    span: expr.1.clone(),
                })?;

            let mut new_env = env;
            new_env.extend(patt_env);

            eval(body, new_env)?
        }
        Expr::Op(e1, op, e2) => match eval(op, env.clone())? {
            Value::Closure(name, params, body, captured_env) => {
                if params.len() != 2 {
                    return Err(Error {
                        msg: format!(
                            "Expected 2 parameters for operator {}, but got {}",
                            name.map(|n| format!("`{n}`"))
                                .unwrap_or_else(|| "<anonymous>".to_string()),
                            params.len(),
                        ),
                        span: op.1,
                    });
                }

                let body = match body.1.end {
                    0 => Box::new((body.0, op.1)), // Builtin functions have no span
                    _ => body.clone(),
                };

                let arg1 = eval(e1, env.clone())?;
                let arg2 = eval(e2, env.clone())?;

                let mut new_env = env[..captured_env].to_vec();
                new_env.extend([(params[0], arg1), (params[1], arg2)]);

                eval(&body, new_env)?
            }
            v => {
                return Err(Error {
                    msg: format!("Expected an operator, but got {v}"),
                    span: op.1,
                });
            }
        },
        Expr::Let(_, x, e, body) => {
            let mut v = eval(e, env.clone())?;

            if let Value::Closure(name, _, _, new_env) = &mut v {
                *name = Some(x);
                *new_env += 1;
            }

            let mut new_env = env;
            new_env.push((x, v));

            eval(body, new_env)?
        }
        Expr::Builtin(f) => f(&expr, &env)?,
    })
}

pub fn match_patt<'src>(patt: &Patt<'src>, value: &Value<'src>) -> Option<Env<'src>> {
    match (patt, value) {
        (Patt::Lit(p), v) if p == v => Some(vec![]),
        (Patt::Var(x), _) => Some(vec![(x, value.clone())]),
        (Patt::Struct(x, ps), Value::Struct(y, _, args_env, _)) if Some(*x) == *y => {
            let mut env = vec![];
            for ((p, _), (_, v)) in ps.0.iter().zip(args_env.iter()) {
                if let Some(new_env) = match_patt(p, v) {
                    env.extend(new_env);
                } else {
                    return None;
                }
            }

            Some(env)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::builtin;

    use super::*;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn assert_eval<'src>(expr: Spanned<Expr<'src>>, env: Env<'src>, expected: Value<'src>) {
        let value = match eval(&expr, env) {
            Ok(value) => value,
            Err(e) => panic!("{}", e.msg),
        };

        assert_eq!(value, expected);
    }

    #[test]
    fn test_lit() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // 42
        let expr = spanned(Expr::Lit(Value::Int(43)));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_var() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // x
        let expr = spanned(Expr::Var("x"));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_call() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "id",
                Value::Closure(Some("id"), vec!["y"], Box::new(spanned(Expr::Var("y"))), 0),
            ),
        ];

        // id(x)()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Call(
                Box::new(spanned(Expr::Var("id"))),
                spanned(vec![spanned(Expr::Var("x"))]),
            ))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_call_closure() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "id",
                Value::Closure(Some("id"), vec!["y"], Box::new(spanned(Expr::Var("y"))), 0),
            ),
        ];

        // id(x)
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("id"))),
            spanned(vec![spanned(Expr::Var("x"))]),
        ));

        assert_eval(
            expr,
            env,
            Value::Struct(
                Some("id"),
                Box::new(spanned(Expr::Var("y"))),
                vec![("y", Value::Int(42))],
                0,
            ),
        );
    }

    #[test]
    fn test_call_struct() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "x_struct",
                Value::Struct(
                    Some("x_struct"),
                    Box::new(spanned(Expr::Var("x"))),
                    vec![("x", Value::Int(43))],
                    2,
                ),
            ),
        ];

        // x_struct()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Var("x_struct"))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_fn() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // fn(y) { y }
        let expr = spanned(Expr::Fn(
            spanned(vec!["y"]),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(
            expr,
            env,
            Value::Closure(None, vec!["y"], Box::new(spanned(Expr::Var("y"))), 2),
        );
    }

    #[test]
    fn test_case() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // case x {
        //     42 => y
        //     _ => x
        // }
        let expr = spanned(Expr::Case(
            Box::new(spanned(Expr::Var("x"))),
            spanned(vec![
                (
                    spanned(Patt::Lit(Value::Int(42))),
                    Box::new(spanned(Expr::Var("y"))),
                ),
                (spanned(Patt::Var("_")), Box::new(spanned(Expr::Var("x")))),
            ]),
        ));

        assert_eval(expr, env, Value::Float(3.14));
    }

    #[test]
    fn test_op() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Int(3)),
            builtin::env().into_iter().find(|(k, _)| *k == "+").unwrap(),
        ];

        // x + y
        let expr = spanned(Expr::Op(
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("+"))),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_op_ident() {
        let env = vec![
            ("x", Value::Int(42)),
            ("y", Value::Int(3)),
            builtin::env()
                .into_iter()
                .find(|(k, _)| *k == "+")
                .map(|(_, v)| ("add", v))
                .unwrap(),
        ];

        // x add y
        let expr = spanned(Expr::Op(
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("add"))),
            Box::new(spanned(Expr::Var("y"))),
        ));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_let_fn() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // let id = fn(y) { y } id
        let expr = spanned(Expr::Let(
            None,
            "id",
            Box::new(spanned(Expr::Fn(
                spanned(vec!["y"]),
                Box::new(spanned(Expr::Var("y"))),
            ))),
            Box::new(spanned(Expr::Var("id"))),
        ));

        assert_eval(
            expr,
            env,
            Value::Closure(Some("id"), vec!["y"], Box::new(spanned(Expr::Var("y"))), 3),
        );
    }

    #[test]
    fn test_let() {
        let env = vec![("x", Value::Int(42)), ("y", Value::Float(3.14))];

        // let z = x z
        let expr = spanned(Expr::Let(
            None,
            "z",
            Box::new(spanned(Expr::Var("x"))),
            Box::new(spanned(Expr::Var("z"))),
        ));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_builtin() {
        let mut env = builtin::env();
        env.extend([("x", Value::Int(42)), ("y", Value::Int(3))]);

        // +(x, y)()
        let expr = spanned(Expr::Call(
            Box::new(spanned(Expr::Call(
                Box::new(spanned(Expr::Var("+"))),
                spanned(vec![spanned(Expr::Var("x")), spanned(Expr::Var("y"))]),
            ))),
            spanned(vec![]),
        ));

        assert_eval(expr, env, Value::Int(45));
    }
}
