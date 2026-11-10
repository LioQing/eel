use chumsky::prelude::*;

use crate::{ast, builtin, declare::Value, lex};

use super::eval;

fn assert_eval<'src>(src: &'src str, expected: Value<'src>) {
    let toks = lex::lexer().parse(src).unwrap();
    let expr = ast::expr_parser()
        .parse_with_state(
            toks.as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            &mut extra::SimpleState(vec![]),
        )
        .unwrap();

    assert_eq!(
        eval(&expr, builtin::env()).unwrap(),
        expected,
        "Source: {src}"
    );
}

#[test]
fn test_struct_pattern_rejects_too_few_fields() {
    assert_eval(
        "let pair = fn(x, y) { unit } case pair(42, 7) { pair(a) => 1 _ => 0 }",
        Value::Int(0),
    );
}

#[test]
fn test_struct_pattern_rejects_too_many_fields() {
    assert_eval(
        "let box = fn(x) { unit } case box(42) { box(a, b) => 1 _ => 0 }",
        Value::Int(0),
    );
}

#[test]
fn test_struct_pattern_matches_zero_fields() {
    assert_eval(
        "let empty = fn() { unit } case empty() { empty() => 1 _ => 0 }",
        Value::Int(1),
    );
}

#[test]
fn test_struct_pattern_rejects_empty_pattern_for_nonempty_struct() {
    assert_eval(
        "let box = fn(x) { unit } case box(42) { box() => 1 _ => 0 }",
        Value::Int(0),
    );
}

#[test]
fn test_struct_pattern_rejects_nonempty_pattern_for_empty_struct() {
    assert_eval(
        "let empty = fn() { unit } case empty() { empty(a) => 1 _ => 0 }",
        Value::Int(0),
    );
}

#[test]
fn test_struct_pattern_matches_exact_arity_and_binds_fields() {
    assert_eval(
        "let pair = fn(x, y) { unit } case pair(42, 7) { pair(a, b) => { a - b } _ => 0 }",
        Value::Int(35),
    );
}

#[test]
fn test_struct_pattern_matches_nested_fields_and_binds_values() {
    assert_eval(
        "
        let box = fn(x) { unit }
        let pair = fn(x, y) { unit }
        case pair(7, box(42)) {
            pair(a, box(b)) => { a + b }
            _ => 0
        }
        ",
        Value::Int(49),
    );
}

#[test]
fn test_struct_pattern_nested_arity_mismatch_preserves_fallback_bindings() {
    for pattern in ["outer(tag, inner())", "outer(tag, inner(a, b))"] {
        let src = format!(
            "
            let tag = 99
            let inner = fn(x) {{ unit }}
            let outer = fn(tag, item) {{ unit }}
            case outer(7, inner(42)) {{
                {pattern} => 1
                _ => tag
            }}
            "
        );

        assert_eval(&src, Value::Int(99));
    }
}

#[test]
fn test_struct_pattern_falls_through_to_matching_arm() {
    assert_eval(
        "
        let box = fn(x) { unit }
        case box(42) {
            box() => 0
            box(a, b) => 1
            box(41) => 2
            box(a) => a
            _ => 3
        }
        ",
        Value::Int(42),
    );
}
