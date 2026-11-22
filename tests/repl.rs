mod common;

use std::{
    io::Write,
    process::{Output, Stdio},
};

use common::{SourceFile, assert_failure, assert_success, cli};

fn run_repl(input: &str) -> Output {
    let mut child = cli()
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    child.wait_with_output().unwrap()
}

#[test]
fn test_repl_remembers_bindings_fixity_and_it_until_quit() {
    let output = run_repl(
        "\
let x = 10
let opr 1 sub = -
x sub 3 sub 2
let x = x + 1
x + it
:q
999
",
    );
    assert_success(&output, "9\n20\n", "");
}

#[test]
fn test_repl_evaluates_a_multiline_recursive_definition() {
    let output = run_repl(
        "\
:{
let sum = fn(n) {
    case n {
        0 => 0
        _ => { n + sum(n - 1). }
    }
}
:}
sum(4).",
    );
    assert_success(&output, "10\n", "");
}

#[test]
fn test_repl_persists_a_multiline_recursive_group_and_operator() {
    let output = run_repl(
        "\
42
:{
let {
    odd = fn(n) { case n { 0 => 0 _ => even(n - 1). } }
    even = fn(n) { case n { 0 => 1 _ => odd(n - 1). } }
    opr 2 sub = fn(a, b) { a - b }
}
:}
it
even(4).
odd(4).
10 sub 3 sub 2
",
    );
    assert_success(&output, "42\n42\n1\n0\n9\n", "");
}

#[test]
fn test_repl_recovers_from_errors_and_reports_the_original_definition() {
    let output = run_repl(
        "\
40
:{
let bad = fn() {
    missing
}
:}
bad().
let x =
9223372036854775808
it + 2
:quit
",
    );
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(std::str::from_utf8(&output.stdout).unwrap(), "40\n42\n");
    let stderr = std::str::from_utf8(&output.stderr).unwrap();
    for expected in [
        "<repl:2>:2:5",
        "Variable `missing` not found",
        "found end of input",
        "Invalid integer literal",
    ] {
        assert!(stderr.contains(expected), "{stderr}");
    }
}

#[test]
fn test_file_mode_still_requires_a_let_body() {
    let file = SourceFile::new("let x = 42");
    assert_failure(&file.run(), "Parsing failed");
}
