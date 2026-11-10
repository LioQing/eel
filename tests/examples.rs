use std::path::Path;
use std::process::{Command, Output};

const FIB_STDOUT: &str = "Returned: 55\n";
const CONS_STDOUT: &str = concat!(
    "cons(x=1, xs=cons(x=2, xs=cons(x=3, xs=nil() { unit }) { unit }) { unit }) { unit }\n",
    "cons(x=2, xs=cons(x=4, xs=cons(x=6, xs=nil() { unit }) { unit }) { unit }) { unit }\n",
    "3\n",
    "Returned: 0\n",
);

fn run_file(path: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_eel"))
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join(path))
        .output()
        .expect("failed to run eel")
}

fn is_successful_example(output: &Output, expected_stdout: &str) -> bool {
    output.status.success()
        && output.stdout == expected_stdout.as_bytes()
        && output.stderr.is_empty()
}

fn assert_example(path: &str, expected_stdout: &str) {
    let output = run_file(path);
    assert!(
        is_successful_example(&output, expected_stdout),
        "unexpected result for {path}:\nstatus: {}\nexpected stdout:\n{expected_stdout}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn fibonacci_example_returns_55() {
    assert_example("examples/fib.eel", FIB_STDOUT);
}

#[test]
fn cons_example_prints_lists_and_length_then_returns_zero() {
    assert_example("examples/cons.eel", CONS_STDOUT);
}

#[test]
fn invalid_source_cannot_pass_example_checks() {
    let output = run_file("tests/fixtures/invalid-example.eel");

    // Do not depend on the CLI's failure exit code: diagnostics alone must also
    // prevent an invalid program from passing the example conformance checks.
    let diagnostics = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(diagnostics.contains("Error"), "{diagnostics}");
    assert!(!is_successful_example(&output, FIB_STDOUT));
    assert!(!is_successful_example(&output, CONS_STDOUT));
}
