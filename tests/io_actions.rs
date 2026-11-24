mod common;

use common::{SourceFile, assert_exit, assert_failure, assert_source_diagnostic, assert_success};

#[test]
fn test_requested_io_expression() {
    let output = SourceFile::new("Io((fn(n) { pure(n + 1). })(5). bind print)").run();
    assert_success(&output, "6\n", "");
}

#[test]
fn test_bind_sequences_actions_and_preserves_lexical_capture() {
    let output = SourceFile::new(
        "let offset = 40
         let next = fn(n) { print(n + offset). }
         let offset = 99
         Io(print(1).
             bind fn(u) { case u { unit => pure(2). } }
             bind next
             bind fn(_) { eprint(3). }
             bind fn(_) { print(4). }
             bind fn(_) { pure(5). })",
    )
    .run();
    assert_exit(&output, 5, "1\n42\n4\n", "3\n");
}

#[test]
fn test_evaluating_structs_does_not_run_them() {
    for source in [
        "let _ = print(99). let _ = eprint(99). 42",
        "let _ = Io(print(99).) 42",
        "let _ = Io(print(99).). 42",
        "let _ = (print(99).). 42",
        "let _ = (print(99). bind fn(_) { missing }) 42",
        "let plan = print(99). case plan { Print(x) => { x - 57 } }",
        "let wrapped = Io(print(99).) case wrapped { Io(Print(x)) => { x - 57 } }",
    ] {
        assert_exit(&SourceFile::new(source).run(), 42, "", "");
    }
}

#[test]
fn test_only_a_direct_io_result_runs_once() {
    for source in [
        "print(99).",
        "Io(print(99).).",
        "let Wrap = Io Wrap(print(99).)",
        "let box = fn(x) { x } box(Io(print(99).))",
        "Io(pure(Io(print(99).)).)",
    ] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, "Expected an integer exit code or `unit`");
        assert_source_diagnostic(&output, &file.0);
    }
}

#[test]
fn test_runner_uses_names_and_positional_args_not_bodies() {
    let output =
        SourceFile::new("let Io = fn(x) { missing } let Print = fn(x) { missing } Io(Print(6))")
            .run();
    assert_success(&output, "6\n", "");
}

#[test]
fn test_invalid_io_result_preserves_earlier_output() {
    let file = SourceFile::new("Io(print(7). bind fn(_) { pure(256). })");
    let output = file.run();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert_eq!(output.stdout, b"7\n");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("Exit code must be in 0..=255, got 256")
    );
    assert_source_diagnostic(&output, &file.0);
}

#[test]
fn test_malformed_io_is_a_runtime_diagnostic() {
    for (source, message) in [
        ("let Io = fn() { unit } Io()", "Expected one argument"),
        ("Io(42)", "Expected an IO action struct"),
        ("Io(print(42))", "Expected an IO action (`Pure`"),
        (
            "let Pure = fn() { unit } Io(Pure())",
            "Wrong number of arguments for IO action `Pure`",
        ),
        ("Io(42 bind print)", "Expected an IO action struct"),
        ("Io(print(42). bind 0)", "Expected a unary function"),
        (
            "Io(print(42). bind fn() { unit })",
            "Expected a unary function",
        ),
        (
            "Io(pure(42). bind fn(x) { x })",
            "Expected an IO action struct",
        ),
        (
            "Io(pure(42). bind fn(x) { print(x) })",
            "Expected an IO action (`Pure`",
        ),
    ] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, message);
        assert_source_diagnostic(&output, &file.0);
    }
}

#[test]
fn test_failure_stops_later_actions_but_keeps_earlier_output() {
    let output =
        SourceFile::new("Io(print(1). bind fn(_) { missing } bind fn(_) { print(2). })").run();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert_eq!(output.stdout, b"1\n");
    assert!(String::from_utf8_lossy(&output.stderr).contains("Variable `missing` not found"));
}
