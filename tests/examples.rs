use std::path::Path;

mod common;

use common::{assert_exit, assert_failure, assert_source_diagnostic, run_file};

const CONS_STDOUT: &str = concat!(
    "cons(x=1, xs=cons(x=2, xs=cons(x=3, xs=nil() { unit }) { unit }) { unit }) { unit }\n",
    "cons(x=2, xs=cons(x=4, xs=cons(x=6, xs=nil() { unit }) { unit }) { unit }) { unit }\n",
    "3\n",
);

fn assert_example(path: &str, code: u8, expected_stdout: &str) {
    let output = run_file(Path::new(env!("CARGO_MANIFEST_DIR")).join(path));
    assert_exit(&output, code, expected_stdout, "");
}

#[test]
fn test_fibonacci_example_returns_55() {
    assert_example("examples/fib.eel", 55, "");
}

#[test]
fn test_cons_example_prints_lists_and_length_then_returns_zero() {
    assert_example("examples/cons.eel", 0, CONS_STDOUT);
}

#[test]
fn test_invalid_example_fails_with_source_diagnostic() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/invalid-example.eel");
    let output = run_file(&path);
    assert_failure(&output, "Parsing failed");
    assert_source_diagnostic(&output, &path);
}
