mod common;

use common::{SourceFile, assert_failure, assert_returned};

#[test]
fn test_call_creates_a_struct_without_evaluating_its_body() {
    assert_returned("let f = fn() { missing } case f() { f() => 42 }", 42);
}

#[test]
fn test_call_accepts_let_expressions_as_arguments() {
    assert_returned(
        "let n = 2
         let sum = fn(a, b, c) { a + b + c }
         sum(let (x, y) = (10, 20) x + y, let n = 10 n, n,).",
        42,
    );
}

#[test]
fn test_call_accepts_a_recursive_let_group_as_an_argument() {
    assert_returned(
        "let sum = fn(a, b) { a + b }
         sum(let { first = fn() { second(). } second = fn() 40 } first()., 2).",
        42,
    );
}

#[test]
fn test_dot_evaluates_a_struct() {
    assert_returned("let f = fn() { 42 } let value = f() value.", 42);
    assert_returned("let f = fn(x) { fn() { x } } f(42).().", 42);
}

#[test]
fn test_structs_cannot_be_called() {
    for source in [
        "let f = fn() { 42 } f()()",
        "let f = fn(x) { x } let value = f(42) value()",
        "let f = fn() { missing_body } let value = f() value(missing_arg)",
    ] {
        assert_failure(&SourceFile::new(source).run(), "Use `.` to evaluate it");
    }
}

#[test]
fn test_dot_requires_a_struct() {
    for source in ["let f = fn() { 42 } f.", "(42)."] {
        assert_failure(&SourceFile::new(source).run(), "Expected a struct");
    }
}
