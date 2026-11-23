use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{declare::*, interp};

#[derive(Debug)]
pub enum ActionError {
    Runtime(Error),
    Output(io::Error),
}

/// Runs only a direct, top-level `Io(plan)`. An `Io` returned by the plan
/// itself is just a result value and is not run again.
pub fn run_result<'src>(
    result: Spanned<Value<'src>>,
    mut output: impl Write,
    mut diagnostics: impl Write,
) -> Result<Value<'src>, ActionError> {
    let Value::Struct(wrapper) = &result.value else {
        return Ok(result.value);
    };
    if wrapper.name != Some("Io") {
        return Ok(result.value);
    }
    let [plan] = wrapper.args.as_ref() else {
        return Err(ActionError::Runtime(Error {
            msg: "Expected one argument in `Io(plan)`".into(),
            span: result.span,
        }));
    };

    let mut current = Rc::clone(&plan.value);
    let mut span = result.span;
    let mut pending = Vec::new();
    loop {
        let Value::Struct(plan) = current.as_ref() else {
            return Err(ActionError::Runtime(Error {
                msg: "Expected an IO action struct".into(),
                span,
            }));
        };

        let value = match (plan.name, plan.args.as_ref()) {
            (Some("Pure"), [arg]) => Rc::clone(&arg.value),
            (Some("Print"), [arg]) => {
                writeln!(output, "{}", arg.value).map_err(ActionError::Output)?;
                Rc::new(Value::Unit)
            }
            (Some("Eprint"), [arg]) => {
                writeln!(diagnostics, "{}", arg.value).map_err(ActionError::Output)?;
                Rc::new(Value::Unit)
            }
            (Some("Bind"), [first, next]) => {
                let next = match next.value.as_ref() {
                    Value::Closure(next) if next.definition().params.len() == 1 => next,
                    _ => {
                        return Err(ActionError::Runtime(Error {
                            msg: "Expected a unary function in `Bind(plan, next)`".into(),
                            span: plan.body.span,
                        }));
                    }
                };
                span = plan.body.span;
                pending.push((next.clone(), span));
                current = Rc::clone(&first.value);
                continue;
            }
            (Some(name @ ("Pure" | "Print" | "Eprint" | "Bind")), _) => {
                return Err(ActionError::Runtime(Error {
                    msg: format!("Wrong number of arguments for IO action `{name}`"),
                    span,
                }));
            }
            _ => {
                return Err(ActionError::Runtime(Error {
                    msg: "Expected an IO action (`Pure`, `Print`, `Eprint`, or `Bind`)".into(),
                    span,
                }));
            }
        };

        let Some((next, call_span)) = pending.pop() else {
            return Ok(value.as_ref().clone());
        };

        current = Rc::new(
            interp::eval_unary_closure(&next, value, call_span).map_err(ActionError::Runtime)?,
        );
        span = call_span;
    }
}
