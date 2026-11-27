use std::rc::Rc;

use super::{builtin, declare::*};

fn insert_recursive_bindings<'src>(env: &mut Env<'src>, group: &Rc<ClosureGroup<'src>>) {
    for (member, def) in group.members.iter().enumerate() {
        if let Some(name) = def.rec_name {
            let closure = Value::Closure(Closure {
                name: Some(name),
                group: Rc::clone(group),
                member,
            });
            env.insert_mut(name, Rc::new(closure));
        }
    }
}

fn insert_bindings<'src>(env: &mut Env<'src>, bindings: impl IntoIterator<Item = Binding<'src>>) {
    for Binding { name, value } in bindings {
        env.insert_mut(name, value);
    }
}

fn prepare_closure_call<'src>(
    closure: &Closure<'src>,
    args: impl ExactSizeIterator<Item = Result<(Rc<Value<'src>>, Option<Span>), Error>>,
    call_span: Span,
    callee_span: Span,
) -> Result<Struct<'src>, Error> {
    let name = closure.name;
    let ClosureDef { params, body, .. } = closure.definition();
    if params.len() != args.len() {
        return Err(Error::new(
            call_span,
            format!(
                "Expected {} arguments for function {}, but got {}",
                params.len(),
                name.map(|n| format!("`{n}`"))
                    .unwrap_or_else(|| "<anonymous>".to_string()),
                args.len(),
            ),
        ));
    }

    let body = builtin::with_span(Rc::clone(body), callee_span);
    let mut applied_env = closure.group.env.clone();
    insert_recursive_bindings(&mut applied_env, &closure.group);

    // Every constructor in the parameter list refers to the definition scope,
    // even when an earlier parameter binds the same name.
    let pattern_env = applied_env.clone();

    let mut param_names = Vec::new();
    let args = params
        .iter()
        .zip(args)
        .map(|(patt, arg)| {
            let (value, span) = arg?;
            let bindings =
                require_match(patt, &value, &pattern_env).map_err(|error| match span {
                    Some(span) => error.with_secondary(span, "Argument passed here"),
                    None => error,
                })?;
            param_names.extend(bindings.iter().map(|binding| binding.name));
            insert_bindings(&mut applied_env, bindings);

            let name = match patt.value {
                Patt::Var(binder) => Some(binder.name),
                _ => None,
            };

            Ok(StructArg { name, value })
        })
        .collect::<Result<_, Error>>()?;

    Ok(Struct {
        ctor: closure.definition().id,
        name,
        body,
        args,
        applied_env,
        param_names: param_names.into(),
    })
}

fn match_fields<'a, 'src: 'a>(
    patterns: &[Spanned<Patt<'src>>],
    values: impl ExactSizeIterator<Item = &'a Rc<Value<'src>>>,
    env: &Env<'src>,
) -> Option<Result<Bindings<'src>, Error>> {
    if patterns.len() != values.len() {
        return None;
    }

    // A structural mismatch rules out this arm, even if an earlier field
    // would be invalid as an operator. Report errors only for a matched arm.
    let mut bindings = Ok(vec![]);
    for (patt, value) in patterns.iter().zip(values) {
        let field = match_patt(patt, value, env)?;
        bindings = bindings.and_then(|mut bindings| {
            bindings.extend(field?);
            Ok(bindings)
        });
    }
    Some(bindings)
}

fn match_patt<'src>(
    patt: &Spanned<Patt<'src>>,
    value: &Rc<Value<'src>>,
    env: &Env<'src>,
) -> Option<Result<Bindings<'src>, Error>> {
    match (&patt.value, value.as_ref()) {
        (Patt::Lit(p), v) if p == v => Some(Ok(vec![])),
        (Patt::Var(binder), _) => Some(
            validated_binding(*binder, Rc::clone(value), patt.span).map(|binding| vec![binding]),
        ),
        (Patt::Tuple(patterns), Value::Tuple(elements)) => {
            match_fields(patterns, elements.iter(), env)
        }
        (Patt::Struct(patt), Value::Struct(value)) => {
            let Value::Closure(closure) = env.get(patt.name)?.as_ref() else {
                return None;
            };
            if value.ctor != closure.definition().id {
                return None;
            }
            match_fields(
                &patt.args.value,
                value.args.iter().map(|arg| &arg.value),
                env,
            )
        }
        _ => None,
    }
}

fn validate_operator_arity(name: Option<&str>, arity: usize, span: Span) -> Result<(), Error> {
    if arity == 2 {
        return Ok(());
    }

    Err(Error::new(
        span,
        format!(
            "Expected an operator, which is a function with exactly 2 parameters, but {} has {arity}",
            name.map(|n| format!("`{n}`"))
                .unwrap_or_else(|| "<anonymous>".to_string())
        ),
    ))
}

fn require_match<'src>(
    patt: &Spanned<Patt<'src>>,
    value: &Rc<Value<'src>>,
    env: &Env<'src>,
) -> Result<Bindings<'src>, Error> {
    match_patt(patt, value, env).ok_or_else(|| {
        Error::new(
            patt.span,
            format!("Pattern {} did not match value {value}", patt.value),
        )
    })?
}

fn validated_binding<'src>(
    binder: Binder<'src>,
    value: Rc<Value<'src>>,
    span: Span,
) -> Result<Binding<'src>, Error> {
    if binder.fixity.is_some() {
        let Value::Closure(closure) = value.as_ref() else {
            return Err(Error::new(
                span,
                format!(
                    "Expected an operator, which is a function with exactly 2 parameters, but got {value}"
                ),
            ));
        };

        validate_operator_arity(Some(binder.name), closure.definition().params.len(), span)?;
    }

    Ok(Binding {
        name: binder.name,
        value,
    })
}

/// Matches the initializer in the old environment, installing bindings only on success.
pub fn bind<'src>(
    patt: &Spanned<Patt<'src>>,
    expr: &Spanned<Expr<'src>>,
    env: &mut Env<'src>,
) -> Result<(), Error> {
    let Patt::Var(binder) = patt.value else {
        let value = Rc::new(eval(expr, env.clone())?);
        let bindings = require_match(patt, &value, env)
            .map_err(|error| error.with_secondary(expr.span, "Initializer supplied here"))?;

        insert_bindings(env, bindings);
        return Ok(());
    };

    // A simple name can introduce a recursive function definition or a named alias.
    let v = match &expr.value {
        Expr::Fn(FnExpr { params, body }) => Value::Closure(Closure {
            name: Some(binder.name),
            group: Rc::new(ClosureGroup {
                env: env.clone(),
                members: Box::new([ClosureDef {
                    rec_name: Some(binder.name),
                    id: DefinitionId::Source(params.span),
                    params: params.value.clone(),
                    body: Rc::clone(body),
                }]),
            }),
            member: 0,
        }),
        _ => {
            let mut v = eval(expr, env.clone())?;
            // An alias changes the display and struct name, not the scope.
            if let Value::Closure(closure) = &mut v {
                closure.name = Some(binder.name);
            }
            v
        }
    };

    let binding = validated_binding(binder, Rc::new(v), patt.span)
        .map_err(|error| error.with_secondary(expr.span, "Initializer supplied here"))?;

    env.insert_mut(binding.name, binding.value);
    Ok(())
}

/// Evaluates the expression in the old environment and installs it on success.
pub fn bind_rec_group<'src>(
    members: &[RecFn<'src, Rc<Spanned<Expr<'src>>>>],
    env: &mut Env<'src>,
) -> Result<(), Error> {
    for member in members {
        if member.binder.value.fixity.is_some() {
            validate_operator_arity(
                Some(member.binder.value.name),
                member.params.value.len(),
                member.params.span,
            )?;
        }
    }

    let group = Rc::new(ClosureGroup {
        env: env.clone(),
        members: members
            .iter()
            .map(|member| ClosureDef {
                rec_name: Some(member.binder.value.name),
                id: DefinitionId::Source(member.params.span),
                params: member.params.value.clone(),
                body: Rc::clone(&member.body),
            })
            .collect(),
    });

    insert_recursive_bindings(env, &group);
    Ok(())
}

/// Applies and evaluates a unary closure using an already evaluated argument.
///
/// Used by the runners to evaluate I/O plan continuations.
pub fn eval_unary_closure<'src>(
    closure: &Closure<'src>,
    arg: Rc<Value<'src>>,
    span: Span,
) -> Result<Value<'src>, Error> {
    // The runner supplies an evaluated value, not a source-level argument.
    let applied = prepare_closure_call(closure, std::iter::once(Ok((arg, None))), span, span)?;
    eval(&applied.body, applied.applied_env)
}

fn eval_field<'src>(
    applied: Struct<'src>,
    field: Spanned<&'src str>,
) -> Result<Value<'src>, Error> {
    fn binds(patt: &Patt<'_>, name: &str) -> bool {
        match patt {
            Patt::Var(binder) => binder.name == name,
            Patt::Tuple(elements) => elements.iter().any(|patt| binds(&patt.value, name)),
            Patt::Struct(patt) => patt.args.value.iter().any(|patt| binds(&patt.value, name)),
            Patt::Lit(_) => false,
        }
    }

    let Struct {
        body,
        mut applied_env,
        param_names,
        ..
    } = applied;
    let mut is_local = param_names.contains(&field.value);
    let mut expr = body.as_ref();

    loop {
        expr = match &expr.value {
            Expr::Let(Let { patt, init, body }) => {
                bind(patt, init, &mut applied_env)?;
                is_local |= binds(&patt.value, field.value);
                body
            }
            Expr::LetRec(LetRec { bindings, body }) => {
                bind_rec_group(bindings, &mut applied_env)?;
                is_local |= bindings
                    .iter()
                    .any(|member| member.binder.value.name == field.value);
                body
            }
            _ => break,
        };
    }

    applied_env
        .get(field.value)
        .filter(|_| is_local)
        .map(|value| value.as_ref().clone())
        .ok_or_else(|| Error::new(field.span, format!("Struct has no field `{}`", field.value)))
}

/// Evaluates the expression in the given environment and returns the resulting value.
pub fn eval<'src>(expr: &Spanned<Expr<'src>>, env: Env<'src>) -> Result<Value<'src>, Error> {
    Ok(match &expr.value {
        Expr::Err => unreachable!(), // Valid AST does not contain Expr::Err
        Expr::Lit(v) => v.clone(),
        Expr::Var(x) => env
            .get(*x)
            .map(|v| v.as_ref().clone())
            .ok_or_else(|| Error::new(expr.span, format!("Variable `{x}` not found")))?,
        Expr::Tuple(elements) => Value::Tuple(
            elements
                .iter()
                .map(|element| eval(element, env.clone()).map(Rc::new))
                .collect::<Result<_, _>>()?,
        ),
        Expr::Call(Call { callee, args }) => match eval(callee, env.clone())? {
            Value::Closure(closure) => Value::Struct(prepare_closure_call(
                &closure,
                args.value.iter().map(|arg| {
                    eval(arg, env.clone()).map(|value| (Rc::new(value), Some(arg.span)))
                }),
                expr.span,
                callee.span,
            )?),
            v @ Value::Struct(_) => {
                return Err(Error::new(
                    expr.span,
                    format!("Expected a function, but got {v}. Use `.` to evaluate it"),
                ));
            }
            v => {
                return Err(Error::new(
                    expr.span,
                    format!("Expected a function, but got {v}"),
                ));
            }
        },
        Expr::Eval(Eval { operand, field }) => match eval(operand, env)? {
            Value::Struct(applied) => match field {
                Some(field) => eval_field(applied, *field)?,
                None => eval(&applied.body, applied.applied_env)?,
            },
            v => {
                return Err(Error::new(
                    expr.span,
                    format!("Expected a struct, but got {v}"),
                ));
            }
        },
        Expr::Fn(FnExpr { params, body }) => Value::Closure(Closure {
            name: None,
            group: Rc::new(ClosureGroup {
                env,
                members: Box::new([ClosureDef {
                    rec_name: None,
                    id: DefinitionId::Source(params.span),
                    params: params.value.clone(),
                    body: Rc::clone(body),
                }]),
            }),
            member: 0,
        }),
        Expr::Case(Case { scrutinee, arms }) => {
            let v = Rc::new(eval(scrutinee, env.clone())?);

            let (patt_bindings, body) = arms
                .value
                .iter()
                .find_map(|CaseArm { patt, body }| {
                    match_patt(patt, &v, &env).map(|bindings| (bindings, body))
                })
                .ok_or_else(|| {
                    Error::new(expr.span, format!("No pattern matched for value {v}"))
                })?;

            let bindings = patt_bindings
                .map_err(|error| error.with_secondary(scrutinee.span, "Value matched here"))?;

            let mut new_env = env;
            insert_bindings(&mut new_env, bindings);

            eval(body, new_env)?
        }
        Expr::Op(Op { lhs, op, rhs }) => match eval(op, env.clone())? {
            Value::Closure(closure) => {
                let name = closure.name;

                validate_operator_arity(name, closure.definition().params.len(), op.span)?;

                let applied = prepare_closure_call(
                    &closure,
                    [lhs, rhs].into_iter().map(|arg| {
                        eval(arg, env.clone()).map(|value| (Rc::new(value), Some(arg.span)))
                    }),
                    expr.span,
                    op.span,
                )?;

                eval(&applied.body, applied.applied_env)?
            }
            v => {
                return Err(Error::new(
                    op.span,
                    format!("Expected an operator, but got {v}"),
                ));
            }
        },
        Expr::Let(Let { patt, init, body }) => {
            let mut new_env = env;
            bind(patt, init, &mut new_env)?;
            eval(body, new_env)?
        }
        Expr::LetRec(LetRec { bindings, body }) => {
            let mut new_env = env;
            bind_rec_group(bindings, &mut new_env)?;
            eval(body, new_env)?
        }
        Expr::Scope(body) => eval(body, env)?,
        Expr::Builtin(f) => f(expr, &env)?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spanned<T: Clone>(value: T) -> Spanned<T> {
        Spanned::new(value, 0..0)
    }

    fn env_from<'src>(bindings: impl IntoIterator<Item = (&'src str, Value<'src>)>) -> Env<'src> {
        let mut env = Env::new();
        insert_bindings(
            &mut env,
            bindings.into_iter().map(|(name, value)| Binding {
                name,
                value: Rc::new(value),
            }),
        );
        env
    }

    fn assert_eval<'src>(expr: Spanned<Expr<'src>>, env: Env<'src>, expected: Value<'src>) {
        let value = match eval(&expr, env) {
            Ok(value) => value,
            Err(e) => panic!("{}", e.msg),
        };

        assert_eq!(value, expected);
    }

    fn int<'src>(value: i64) -> Spanned<Expr<'src>> {
        spanned(Expr::Lit(Value::Int(value)))
    }

    fn var(name: &str) -> Spanned<Expr<'_>> {
        spanned(Expr::Var(name))
    }

    fn function<'src>(params: Vec<&'src str>, body: Spanned<Expr<'src>>) -> Spanned<Expr<'src>> {
        spanned(Expr::Fn(FnExpr {
            params: spanned(params.into_iter().map(Into::into).collect()),
            body: Rc::new(body),
        }))
    }

    fn call<'src>(
        func: Spanned<Expr<'src>>,
        args: Vec<Spanned<Expr<'src>>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Call(Call {
            callee: Box::new(func),
            args: spanned(args),
        }))
    }

    fn evaluate(operand: Spanned<Expr<'_>>) -> Spanned<Expr<'_>> {
        spanned(Expr::Eval(Eval::new(operand, None)))
    }

    fn binding<'src>(
        name: &'src str,
        assign: Spanned<Expr<'src>>,
        body: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Let(Let {
            patt: Box::new(spanned(Patt::Var(name.into()))),
            init: Box::new(assign),
            body: Box::new(body),
        }))
    }

    fn op<'src>(
        left: Spanned<Expr<'src>>,
        name: &'src str,
        right: Spanned<Expr<'src>>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Op(Op {
            lhs: Box::new(left),
            op: Box::new(var(name)),
            rhs: Box::new(right),
        }))
    }

    fn case<'src>(
        value: Spanned<Expr<'src>>,
        arms: Vec<(Spanned<Patt<'src>>, Spanned<Expr<'src>>)>,
    ) -> Spanned<Expr<'src>> {
        spanned(Expr::Case(Case {
            scrutinee: Box::new(value),
            arms: spanned(
                arms.into_iter()
                    .map(|(patt, body)| CaseArm {
                        patt,
                        body: Box::new(body),
                    })
                    .collect(),
            ),
        }))
    }

    fn pattern_env<'src>() -> Env<'src> {
        let mut env = builtin::env();
        for (index, (name, params)) in [
            ("empty", vec![]),
            ("box", vec!["x"]),
            ("wrap", vec!["x"]),
            ("pair", vec!["x", "y"]),
            ("inner", vec!["x"]),
            ("outer", vec!["tag", "item"]),
        ]
        .into_iter()
        .enumerate()
        {
            let mut expr = function(params, spanned(Expr::Lit(Value::Unit)));
            let Expr::Fn(f) = &mut expr.value else {
                unreachable!()
            };
            f.params.span = (index..index + 1).into();
            bind(&spanned(Patt::Var(name.into())), &expr, &mut env).unwrap();
        }
        env
    }

    fn struct_value<'src>(name: &'src str, args: Vec<(&'src str, Value<'src>)>) -> Value<'src> {
        let args = args
            .into_iter()
            .map(|(_, value)| spanned(Expr::Lit(value)))
            .collect();
        eval(&call(var(name), args), pattern_env()).unwrap()
    }

    fn struct_pattern<'src>(
        name: &'src str,
        args: Vec<Spanned<Patt<'src>>>,
    ) -> Spanned<Patt<'src>> {
        spanned(Patt::Struct(StructPatt {
            name,
            args: spanned(args),
        }))
    }

    #[test]
    fn test_lit() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // 42
        let expr = spanned(Expr::Lit(Value::Int(43)));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_var() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // x
        let expr = spanned(Expr::Var("x"));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_bind_preserves_previous_binding_when_expression_fails() {
        let mut env = env_from([("x", Value::Int(42))]);

        assert!(bind(&spanned(Patt::Var("x".into())), &var("missing"), &mut env).is_err());
        assert_eval(var("x"), env, Value::Int(42));
    }

    #[test]
    fn test_call_then_eval() {
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "id",
                Value::Closure(Closure {
                    name: Some("id"),
                    group: Rc::new(ClosureGroup {
                        env: Default::default(),
                        members: Box::new([ClosureDef {
                            id: DefinitionId::Source((0..0).into()),
                            rec_name: None,
                            params: vec!["y".into()],
                            body: Rc::new(spanned(Expr::Var("y"))),
                        }]),
                    }),
                    member: 0,
                }),
            ),
        ]);

        // id(x).
        let expr = evaluate(call(var("id"), vec![var("x")]));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_call_closure() {
        let body = Rc::new(Spanned::new(Expr::Var("y"), 8..9));
        let id = Value::Closure(Closure {
            name: Some("id"),
            group: Rc::new(ClosureGroup {
                env: Default::default(),
                members: Box::new([ClosureDef {
                    id: DefinitionId::Source((0..0).into()),
                    rec_name: Some("id"),
                    params: vec!["y".into()],
                    body: Rc::clone(&body),
                }]),
            }),
            member: 0,
        });
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            ("id", id.clone()),
        ]);

        // id(x)
        let expr = call(var("id"), vec![var("x")]);

        let value = eval(&expr, env).unwrap();
        assert_eq!(
            value,
            Value::Struct(Struct {
                ctor: DefinitionId::Source((0..0).into()),
                name: Some("id"),
                body: Rc::clone(&body),
                args: Rc::from([StructArg {
                    name: Some("y"),
                    value: Rc::new(Value::Int(42))
                }]),
                applied_env: env_from([("id", id), ("y", Value::Int(42))]),
                param_names: Rc::from(["y"]),
            }),
        );
        let Value::Struct(Struct {
            body: applied_body,
            args,
            applied_env,
            ..
        }) = value
        else {
            panic!("expected a struct");
        };
        assert!(Rc::ptr_eq(&applied_body, &body));
        assert!(Rc::ptr_eq(&args[0].value, applied_env.get("y").unwrap()));
    }

    #[test]
    fn test_eval_struct() {
        let x_arg = Rc::new(Value::Int(43));
        let applied_env = env_from([("y", Value::Float(3.14))]).insert("x", Rc::clone(&x_arg));
        let env = env_from([
            ("x", Value::Int(42)),
            ("y", Value::Float(3.14)),
            (
                "x_struct",
                Value::Struct(Struct {
                    ctor: DefinitionId::Source((0..0).into()),
                    name: Some("x_struct"),
                    body: Rc::new(var("x")),
                    args: Rc::from([StructArg {
                        name: Some("x"),
                        value: x_arg,
                    }]),
                    applied_env,
                    param_names: Rc::from(["x"]),
                }),
            ),
        ]);

        // x_struct.
        let expr = evaluate(var("x_struct"));

        assert_eval(expr, env, Value::Int(43));
    }

    #[test]
    fn test_fn() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // fn(y) { y }
        let expr = function(vec!["y"], var("y"));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(Closure {
                name: None,
                group: Rc::new(ClosureGroup {
                    env,
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Source((0..0).into()),
                        rec_name: None,
                        params: vec!["y".into()],
                        body: Rc::new(spanned(Expr::Var("y"))),
                    }]),
                }),
                member: 0,
            }),
        );
    }

    #[test]
    fn test_closures_share_snapshot_until_shadowing() {
        let env = env_from([("x", Value::Int(1)), ("y", Value::Int(2))]);
        let function = function(vec![], var("x"));
        let Value::Closure(first) = eval(&function, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        let Value::Closure(second) = eval(&function, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        assert!(Rc::ptr_eq(
            &first.definition().body,
            &second.definition().body
        ));
        let first = &first.group.env;
        assert!(first.ptr_eq(&env));
        assert!(second.group.env.ptr_eq(first));

        let shadow = binding("x", int(42), function);
        let Value::Closure(shadowed) = eval(&shadow, env.clone()).unwrap() else {
            panic!("expected a closure");
        };
        let shadowed = &shadowed.group.env;
        assert!(!shadowed.ptr_eq(first));
        assert_eq!(first.get("x").unwrap().as_ref(), &Value::Int(1));
        assert_eq!(shadowed.get("x").unwrap().as_ref(), &Value::Int(42));
        assert!(Rc::ptr_eq(
            first.get("y").unwrap(),
            shadowed.get("y").unwrap()
        ));
    }

    #[test]
    fn test_struct_reuses_applied_snapshot_and_shares_args() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = call(make, vec![int(42)]);
        let value = eval(&expr, Env::new()).unwrap();
        let Value::Struct(Struct {
            args, applied_env, ..
        }) = &value
        else {
            panic!("expected a struct");
        };
        assert!(Rc::ptr_eq(&args[0].value, applied_env.get("x").unwrap()));

        let env = env_from([("value", value.clone())]);
        let force = evaluate(var("value"));
        for _ in 0..2 {
            let Value::Closure(closure) = eval(&force, env.clone()).unwrap() else {
                panic!("expected a closure");
            };
            assert!(closure.group.env.ptr_eq(applied_env));
        }
    }

    #[test]
    fn test_case() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // case x {
        //     42 => y
        //     _ => x
        // }
        let expr = case(
            var("x"),
            vec![
                (spanned(Patt::Lit(Value::Int(42))), var("y")),
                (spanned(Patt::Var("_".into())), var("x")),
            ],
        );

        assert_eval(expr, env, Value::Float(3.14));
    }

    #[test]
    fn test_op() {
        let mut env = builtin::env();
        insert_bindings(
            &mut env,
            [
                Binding {
                    name: "x",
                    value: Rc::new(Value::Int(42)),
                },
                Binding {
                    name: "y",
                    value: Rc::new(Value::Int(3)),
                },
            ],
        );

        // x + y
        let expr = op(var("x"), "+", var("y"));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_op_ident() {
        let mut env = env_from([("x", Value::Int(42)), ("y", Value::Int(3))]);
        env.insert_mut("add", builtin::env().get("+").unwrap().clone());

        // x add y
        let expr = op(var("x"), "add", var("y"));

        assert_eval(expr, env, Value::Int(42 + 3));
    }

    #[test]
    fn test_let_fn() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // let id = fn(y) { y } id
        let expr = binding("id", function(vec!["y"], var("y")), var("id"));

        assert_eval(
            expr,
            env.clone(),
            Value::Closure(Closure {
                name: Some("id"),
                group: Rc::new(ClosureGroup {
                    env,
                    members: Box::new([ClosureDef {
                        id: DefinitionId::Source((0..0).into()),
                        rec_name: Some("id"),
                        params: vec!["y".into()],
                        body: Rc::new(spanned(Expr::Var("y"))),
                    }]),
                }),
                member: 0,
            }),
        );
    }

    #[test]
    fn test_let() {
        let env = env_from([("x", Value::Int(42)), ("y", Value::Float(3.14))]);

        // let z = x z
        let expr = binding("z", var("x"), var("z"));

        assert_eval(expr, env, Value::Int(42));
    }

    #[test]
    fn test_builtin() {
        let mut env = builtin::env();
        insert_bindings(
            &mut env,
            [
                Binding {
                    name: "x",
                    value: Rc::new(Value::Int(42)),
                },
                Binding {
                    name: "y",
                    value: Rc::new(Value::Int(3)),
                },
            ],
        );

        // +(x, y).
        let expr = evaluate(call(var("+"), vec![var("x"), var("y")]));

        assert_eval(expr, env, Value::Int(45));
    }

    #[test]
    fn test_returned_closure_retains_parameter() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = binding(
            "make",
            make,
            binding(
                "f",
                evaluate(call(var("make"), vec![int(42)])),
                evaluate(call(var("f"), vec![])),
            ),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_block_closure_retains_shadowed_binding() {
        let expr = binding(
            "x",
            int(0),
            binding(
                "f",
                binding("x", int(42), function(vec![], var("x"))),
                binding("padding", int(99), evaluate(call(var("f"), vec![]))),
            ),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_separate_calls_retain_separate_environments() {
        let make = function(vec!["x"], function(vec![], var("x")));
        let expr = binding(
            "make",
            make,
            binding(
                "first",
                evaluate(call(var("make"), vec![int(20)])),
                binding(
                    "second",
                    evaluate(call(var("make"), vec![int(22)])),
                    op(
                        evaluate(call(var("first"), vec![])),
                        "+",
                        evaluate(call(var("second"), vec![])),
                    ),
                ),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_escaped_struct_retains_parameter() {
        let make = function(
            vec!["x"],
            binding("get", function(vec![], var("x")), call(var("get"), vec![])),
        );
        let expr = binding(
            "make",
            make,
            binding(
                "value",
                evaluate(call(var("make"), vec![int(42)])),
                evaluate(var("value")),
            ),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_block_struct_retains_shadowed_binding() {
        let value = binding(
            "x",
            int(42),
            binding("get", function(vec![], var("x")), call(var("get"), vec![])),
        );
        let expr = binding(
            "x",
            int(0),
            binding(
                "value",
                value,
                binding("padding", int(99), evaluate(var("value"))),
            ),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_caller_bindings_do_not_replace_lexical_bindings() {
        let closure = Value::Closure(Closure {
            name: Some("f"),
            group: Rc::new(ClosureGroup {
                env: env_from([("x", Value::Int(42))]),
                members: Box::new([ClosureDef {
                    id: DefinitionId::Source((0..0).into()),
                    rec_name: None,
                    params: vec![],
                    body: Rc::new(var("x")),
                }]),
            }),
            member: 0,
        });
        let expr = evaluate(call(var("f"), vec![]));
        assert_eval(
            expr,
            env_from([("f", closure), ("x", Value::Int(99))]),
            Value::Int(42),
        );
    }

    #[test]
    fn test_parameters_shadow_captured_bindings() {
        let closure = Value::Closure(Closure {
            name: Some("make"),
            group: Rc::new(ClosureGroup {
                env: env_from([("x", Value::Int(99))]),
                members: Box::new([ClosureDef {
                    id: DefinitionId::Source((0..0).into()),
                    rec_name: None,
                    params: vec!["x".into()],
                    body: Rc::new(function(vec![], var("x"))),
                }]),
            }),
            member: 0,
        });
        let expr = binding(
            "f",
            evaluate(call(var("make"), vec![int(42)])),
            evaluate(call(var("f"), vec![])),
        );
        assert_eval(expr, env_from([("make", closure)]), Value::Int(42));
    }

    #[test]
    fn test_returned_operator_retains_lexical_environment() {
        let expr = binding(
            "op",
            binding(
                "x",
                int(40),
                function(
                    vec!["a", "b"],
                    op(var("x"), "+", op(var("a"), "+", var("b"))),
                ),
            ),
            op(int(1), "op", int(1)),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_returned_closure_retains_pattern_bindings() {
        let value = struct_value("wrap", vec![("x", Value::Int(42))]);
        let expr = binding(
            "f",
            case(
                spanned(Expr::Lit(value)),
                vec![(
                    struct_pattern("wrap", vec![spanned(Patt::Var("x".into()))]),
                    function(vec![], var("x")),
                )],
            ),
            evaluate(call(var("f"), vec![])),
        );
        assert_eval(expr, pattern_env(), Value::Int(42));
    }

    fn factorial<'src>() -> Spanned<Expr<'src>> {
        function(
            vec!["n"],
            case(
                var("n"),
                vec![
                    (spanned(Patt::Lit(Value::Int(0))), int(1)),
                    (
                        spanned(Patt::Var("_".into())),
                        op(
                            var("n"),
                            "*",
                            evaluate(call(var("fact"), vec![op(var("n"), "-", int(1))])),
                        ),
                    ),
                ],
            ),
        )
    }

    #[test]
    fn test_recursive_function_retains_escaped_environment() {
        let sum = function(
            vec!["n"],
            case(
                var("n"),
                vec![
                    (spanned(Patt::Lit(Value::Int(0))), var("base")),
                    (
                        spanned(Patt::Var("_".into())),
                        op(
                            var("n"),
                            "+",
                            evaluate(call(var("sum"), vec![op(var("n"), "-", int(1))])),
                        ),
                    ),
                ],
            ),
        );
        let make = function(vec!["base"], binding("sum", sum, var("sum")));
        let expr = binding(
            "make",
            make,
            binding(
                "sum",
                evaluate(call(var("make"), vec![int(27)])),
                evaluate(call(var("sum"), vec![int(5)])),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_recursive_alias_keeps_original_recursive_binding() {
        let expr = binding(
            "fact",
            factorial(),
            binding(
                "alias",
                var("fact"),
                binding(
                    "fact",
                    function(vec!["n"], int(0)),
                    evaluate(call(var("alias"), vec![int(5)])),
                ),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(120));
    }

    #[test]
    fn test_escaped_struct_retains_recursive_binding() {
        let expr = binding(
            "value",
            binding("fact", factorial(), call(var("fact"), vec![int(5)])),
            evaluate(var("value")),
        );
        assert_eval(expr, builtin::env(), Value::Int(120));
    }

    #[test]
    fn test_recursive_group_outlives_scope_but_drops_with_last_reference() {
        let members = [
            RecFn {
                binder: spanned("first".into()),
                params: spanned(vec![]),
                body: Rc::new(evaluate(call(var("second"), vec![]))),
            },
            RecFn {
                binder: spanned("second".into()),
                params: spanned(vec![]),
                body: Rc::new(var("base")),
            },
        ];
        let mut env = env_from([("base", Value::Int(42))]);
        bind_rec_group(&members, &mut env).unwrap();
        let Value::Closure(first) = env.get("first").unwrap().as_ref() else {
            panic!("expected a group member");
        };
        let Value::Closure(second) = env.get("second").unwrap().as_ref() else {
            panic!("expected a group member");
        };
        assert!(Rc::ptr_eq(&first.group, &second.group));
        assert!(Rc::ptr_eq(&first.definition().body, &members[0].body));
        assert!(first.group.env.get("first").is_none());
        assert!(first.group.env.get("second").is_none());
        let group = Rc::downgrade(&first.group);

        let deferred = eval(&call(var("first"), vec![]), env).unwrap();
        assert!(group.upgrade().is_some());
        assert_eval(
            evaluate(var("deferred")),
            env_from([("deferred", deferred)]),
            Value::Int(42),
        );
        assert!(group.upgrade().is_none());
    }

    #[test]
    fn test_parameters_shadow_recursive_binding() {
        let expr = binding(
            "f",
            function(vec!["f"], var("f")),
            evaluate(call(var("f"), vec![int(42)])),
        );
        assert_eval(expr, Env::new(), Value::Int(42));
    }

    #[test]
    fn test_builtin_alias_evaluates_arguments_in_caller_environment() {
        let expr = binding(
            "add",
            var("+"),
            binding(
                "x",
                int(42),
                evaluate(call(var("add"), vec![var("x"), int(0)])),
            ),
        );
        assert_eval(expr, builtin::env(), Value::Int(42));
    }

    #[test]
    fn test_caller_cannot_supply_missing_lexical_binding() {
        let closure = Value::Closure(Closure {
            name: Some("f"),
            group: Rc::new(ClosureGroup {
                env: Default::default(),
                members: Box::new([ClosureDef {
                    id: DefinitionId::Source((0..0).into()),
                    rec_name: None,
                    params: vec![],
                    body: Rc::new(Spanned::new(Expr::Var("x"), 12..13)),
                }]),
            }),
            member: 0,
        });
        let expr = evaluate(call(var("f"), vec![]));
        assert_eq!(
            eval(&expr, env_from([("f", closure), ("x", Value::Int(42))])),
            Err(Error::new((12..13).into(), "Variable `x` not found"))
        );
    }

    #[test]
    fn test_struct_pattern_requires_exact_arg_arity() {
        for (name, args, patterns) in [
            (
                "pair",
                vec![("x", Value::Int(42)), ("y", Value::Int(7))],
                vec![vec![spanned(Patt::Var("a".into()))]],
            ),
            (
                "box",
                vec![("x", Value::Int(42))],
                vec![
                    vec![],
                    vec![
                        spanned(Patt::Var("a".into())),
                        spanned(Patt::Var("b".into())),
                    ],
                ],
            ),
            ("empty", vec![], vec![vec![spanned(Patt::Var("a".into()))]]),
        ] {
            let value = struct_value(name, args);
            for arg_patterns in patterns {
                let pattern = struct_pattern(name, arg_patterns);
                assert_eq!(
                    match_patt(&pattern, &Rc::new(value.clone()), &pattern_env())
                        .transpose()
                        .unwrap(),
                    None
                );
                let expr = case(
                    spanned(Expr::Lit(value.clone())),
                    vec![(pattern, int(1)), (spanned(Patt::Var("_".into())), int(0))],
                );
                assert_eval(expr, pattern_env(), Value::Int(0));
            }
        }
    }

    #[test]
    fn test_struct_pattern_matches_zero_args() {
        let value = struct_value("empty", vec![]);
        let pattern = struct_pattern("empty", vec![]);
        assert_eq!(
            match_patt(&pattern, &Rc::new(value.clone()), &pattern_env())
                .transpose()
                .unwrap(),
            Some(vec![])
        );
        assert_eval(
            case(spanned(Expr::Lit(value)), vec![(pattern, int(1))]),
            pattern_env(),
            Value::Int(1),
        );
    }

    #[test]
    fn test_struct_pattern_matches_exact_arity_and_binds_args() {
        let value = struct_value("pair", vec![("x", Value::Int(42)), ("y", Value::Int(7))]);
        let pattern = struct_pattern(
            "pair",
            vec![
                spanned(Patt::Var("a".into())),
                spanned(Patt::Var("b".into())),
            ],
        );
        let bindings = match_patt(&pattern, &Rc::new(value.clone()), &pattern_env())
            .transpose()
            .unwrap()
            .unwrap();
        assert_eq!(
            bindings,
            vec![
                Binding {
                    name: "a",
                    value: Rc::new(Value::Int(42)),
                },
                Binding {
                    name: "b",
                    value: Rc::new(Value::Int(7)),
                },
            ]
        );
        let Value::Struct(pair) = &value else {
            panic!("expected a struct");
        };
        for (binding, arg) in bindings.iter().zip(pair.args.iter()) {
            assert!(Rc::ptr_eq(&binding.value, &arg.value));
        }
        assert_eval(
            case(
                spanned(Expr::Lit(value)),
                vec![(pattern, op(var("a"), "-", var("b")))],
            ),
            pattern_env(),
            Value::Int(35),
        );
    }

    #[test]
    fn test_struct_pattern_matches_nested_args_and_binds_values() {
        let value = struct_value(
            "pair",
            vec![
                ("x", Value::Int(7)),
                ("y", struct_value("box", vec![("x", Value::Int(42))])),
            ],
        );
        let pattern = struct_pattern(
            "pair",
            vec![
                spanned(Patt::Var("a".into())),
                struct_pattern("box", vec![spanned(Patt::Var("b".into()))]),
            ],
        );
        assert_eq!(
            match_patt(&pattern, &Rc::new(value.clone()), &pattern_env())
                .transpose()
                .unwrap(),
            Some(vec![
                Binding {
                    name: "a",
                    value: Rc::new(Value::Int(7)),
                },
                Binding {
                    name: "b",
                    value: Rc::new(Value::Int(42)),
                }
            ])
        );
        assert_eval(
            case(
                spanned(Expr::Lit(value)),
                vec![(pattern, op(var("a"), "+", var("b")))],
            ),
            pattern_env(),
            Value::Int(49),
        );
    }

    #[test]
    fn test_struct_pattern_nested_arity_mismatch_preserves_fallback_bindings() {
        let value = struct_value(
            "outer",
            vec![
                ("tag", Value::Int(7)),
                ("item", struct_value("inner", vec![("x", Value::Int(42))])),
            ],
        );
        for arg_patterns in [
            vec![],
            vec![
                spanned(Patt::Var("a".into())),
                spanned(Patt::Var("b".into())),
            ],
        ] {
            let pattern = struct_pattern(
                "outer",
                vec![
                    spanned(Patt::Var("tag".into())),
                    struct_pattern("inner", arg_patterns),
                ],
            );
            assert_eq!(
                match_patt(&pattern, &Rc::new(value.clone()), &pattern_env())
                    .transpose()
                    .unwrap(),
                None
            );
            let expr = case(
                spanned(Expr::Lit(value.clone())),
                vec![
                    (pattern, int(1)),
                    (spanned(Patt::Var("_".into())), var("tag")),
                ],
            );
            assert_eval(
                expr,
                pattern_env().insert("tag", Rc::new(Value::Int(99))),
                Value::Int(99),
            );
        }
    }

    #[test]
    fn test_struct_pattern_falls_through_to_matching_arm() {
        let value = struct_value("box", vec![("x", Value::Int(42))]);
        let expr = case(
            spanned(Expr::Lit(value)),
            vec![
                (struct_pattern("box", vec![]), int(0)),
                (
                    struct_pattern(
                        "box",
                        vec![
                            spanned(Patt::Var("a".into())),
                            spanned(Patt::Var("b".into())),
                        ],
                    ),
                    int(1),
                ),
                (
                    struct_pattern("box", vec![spanned(Patt::Lit(Value::Int(41)))]),
                    int(2),
                ),
                (
                    struct_pattern("box", vec![spanned(Patt::Var("a".into()))]),
                    var("a"),
                ),
                (spanned(Patt::Var("_".into())), int(3)),
            ],
        );
        assert_eval(expr, pattern_env(), Value::Int(42));
    }
}
