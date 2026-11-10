use chumsky::prelude::*;

use crate::{ast, builtin, declare::*, interp, lex};

fn eval_source(src: &str) -> Result<Value<'_>, Error> {
    let toks = lex::lexer().parse(src).unwrap();
    let expr = ast::expr_parser()
        .parse_with_state(
            toks.as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            &mut extra::SimpleState(vec![]),
        )
        .unwrap();

    interp::eval(&expr, builtin::env())
}

fn assert_int(src: &str, expected: i64) {
    assert_eq!(eval_source(src).unwrap(), Value::Int(expected));
}

#[test]
fn returned_closure_retains_parameter() {
    assert_int("let make = fn(x) { fn() { x } } let f = make(42). f().", 42);
}

#[test]
fn block_closure_retains_shadowed_binding() {
    assert_int(
        "let x = 0 let f = { let x = 42 fn() { x } } let padding = 99 f().",
        42,
    );
}

#[test]
fn separate_calls_retain_separate_environments() {
    assert_int(
        "let make = fn(x) { fn() { x } }
         let first = make(20).
         let second = make(22).
         first(). + second().",
        42,
    );
}

#[test]
fn escaped_struct_retains_parameter() {
    assert_int(
        "let make = fn(x) { let get = fn() { x } get() }
         let value = make(42).
         value.",
        42,
    );
}

#[test]
fn block_struct_retains_shadowed_binding() {
    assert_int(
        "let x = 0
         let value = { let x = 42 let get = fn() { x } get() }
         let padding = 99
         value.",
        42,
    );
}

#[test]
fn caller_bindings_do_not_replace_lexical_bindings() {
    assert_int(
        "let x = 42
         let f = fn() { x }
         let invoke = fn(x) { f(). }
         invoke(99).",
        42,
    );
}

#[test]
fn parameters_shadow_captured_bindings() {
    assert_int(
        "let x = 99
         let make = fn(x) { fn() { x } }
         let f = make(42).
         f().",
        42,
    );
}

#[test]
fn returned_operator_retains_lexical_environment() {
    assert_int(
        "let op = { let x = 40 fn(a, b) { x + (a + b) } } 1 op 1",
        42,
    );
}

#[test]
fn returned_closure_retains_pattern_bindings() {
    assert_int(
        "let wrap = fn(x) { unit }
         let make = fn(value) { case value { wrap(x) => fn() { x } } }
         let f = make(wrap(42)).
         f().",
        42,
    );
}

#[test]
fn recursive_function_retains_escaped_environment() {
    assert_int(
        "let make = fn(base) {
             let sum = fn(n) { case n { 0 => base _ => { n + sum(n - 1). } } }
             sum
         }
         let sum = make(27).
         sum(5).",
        42,
    );
}

#[test]
fn recursive_alias_keeps_original_recursive_binding() {
    assert_int(
        "let fact = fn(n) { case n { 0 => 1 _ => { n * fact(n - 1). } } }
         let alias = fact
         let fact = fn(n) { 0 }
         alias(5).",
        120,
    );
}

#[test]
fn escaped_struct_retains_recursive_binding() {
    assert_int(
        "let value = {
             let fact = fn(n) { case n { 0 => 1 _ => { n * fact(n - 1). } } }
             fact(5)
         }
         value.",
        120,
    );
}

#[test]
fn parameters_shadow_recursive_binding() {
    assert_int("let f = fn(f) { f } f(42).", 42);
}

#[test]
fn builtin_alias_evaluates_arguments_in_caller_environment() {
    assert_int("let add = + let x = 42 add(x, 0).", 42);
}

#[test]
fn caller_cannot_supply_missing_lexical_binding() {
    let error = eval_source("let f = fn() { x } let x = 42 f().").unwrap_err();
    assert_eq!(error.msg, "Variable `x` not found");
}
