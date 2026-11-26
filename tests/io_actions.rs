mod common;

use common::{SourceFile, assert_exit, assert_failure, assert_source_diagnostic, assert_success};

#[test]
fn test_monad_operations_require_local_bindings() {
    for (source, name) in [
        ("let x <- io_return(1). x", "bind"),
        ("return(1).", "return"),
    ] {
        assert_failure(
            &SourceFile::new(source).run(),
            &format!("Variable `{name}` not found"),
        );
    }
}

#[test]
fn test_generic_chain_with_custom_and_io_monads() {
    let output = SourceFile::new(
        "let increment = fn(ops, action) {
            case (ops) {
                Monad(bind, return) => { let x <- action return(x + 1). }
            }
         }
         let Identity = Monad(fn(x, next) next(x)., fn(x) x)
         let n = increment(Identity, 40).
         let plan = increment(IoMonad, io_return(n).).
         Io(plan io_bind print)",
    )
    .run();
    assert_success(&output, "42\n", "");
}

#[test]
fn test_nested_let_bind_captures_results() {
    let file = SourceFile::new(
        "Io(case IoMonad {
            Monad(bind, return) => {
                let x <- return(20).
                let y <- return(x + 2).
                let _ <- print(y).
                return(x + y).
            }
        })",
    );
    assert_exit(&file.run(), 42, "22\n", "");
}

#[test]
fn test_let_bind_uses_local_operator_and_defers_body() {
    let file = SourceFile::new(
        "let opr 9 bind = fn(x, next) { case x { 0 => 42 _ => next(x). } }
        let x <- 1
        let _ <- (x - 1)
        missing",
    );
    assert_exit(&file.run(), 42, "", "");
}

#[test]
fn test_requested_io_expression() {
    let output = SourceFile::new("Io((fn(n) { io_return(n + 1). })(5). io_bind print)").run();
    assert_success(&output, "6\n", "");
}

#[test]
fn test_bind_sequences_actions_and_preserves_lexical_capture() {
    let output = SourceFile::new(
        "let offset = 40
         let next = fn(n) { print(n + offset). }
         let offset = 99
         Io(print(1).
             io_bind fn(u) { case u { unit => io_return(2). } }
             io_bind next
             io_bind fn(_) { eprint(3). }
             io_bind fn(_) { print(4). }
             io_bind fn(_) { io_return(5). })",
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
        "let _ = (print(99). io_bind fn(_) { missing }) 42",
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
        "Io(io_return(Io(print(99).)).)",
    ] {
        let file = SourceFile::new(source);
        let output = file.run();
        assert_failure(&output, "Expected an integer exit code or `unit`");
        assert_source_diagnostic(&output, &file.0);
    }
}

#[test]
fn test_builtin_action_aliases_keep_their_original_behavior() {
    let output =
        SourceFile::new("let P = Print let Print = IoReturn Io(IoBind(P(6), fn(_) { Print(42) }))")
            .run();
    assert_exit(&output, 42, "6\n", "");
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
    let file = SourceFile::new("Io(print(7). io_bind fn(_) { io_return(256). })");
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
        ("Io(print(42))", "Expected an IO action (`IoReturn`"),
        (
            "let IoReturn = fn() { unit } Io(IoReturn())",
            "Wrong number of arguments for IO action `IoReturn`",
        ),
        ("Io(42 io_bind print)", "Expected an IO action struct"),
        ("Io(print(42). io_bind 0)", "Expected a unary function"),
        (
            "Io(print(42). io_bind fn() { unit })",
            "Expected a unary function",
        ),
        (
            "Io(io_return(42). io_bind fn(x) { x })",
            "Expected an IO action struct",
        ),
        (
            "Io(io_return(42). io_bind fn(x) { print(x) })",
            "Expected an IO action (`IoReturn`",
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
        SourceFile::new("Io(print(1). io_bind fn(_) { missing } io_bind fn(_) { print(2). })")
            .run();
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    assert_eq!(output.stdout, b"1\n");
    assert!(String::from_utf8_lossy(&output.stderr).contains("Variable `missing` not found"));
}
