use std::{
    io::{self, BufRead, Write},
    rc::Rc,
};

use chumsky::prelude::*;
use typed_arena::Arena;

use crate::lang::{
    builtin,
    declare::{Env, LetItem, OpSpec, ReplItem, Spanned, Value},
    fixity, interp, parse,
};

use super::{
    io_action::{self, ActionError},
    source::{Diagnostic, SourceMap},
};

const HELP: &str = "\
Enter an expression to evaluate it, `let pattern = expression` to bind the matched names,
`let { foo = fn(...) ... bar = fn(...) ... }` to define a recursive function group.
:help       Show this help
:quit, :q   Exit the REPL
:{          Begin multiline input
:}          Evaluate the multiline input";

pub fn run(
    mut input: impl BufRead,
    mut output: impl Write,
    mut diagnostics: impl Write,
    show_prompt: bool,
) -> io::Result<()> {
    let arena = Arena::new();
    let mut session = Session::new(&arena);
    let mut line = String::new();
    let mut multiline: Option<String> = None;

    loop {
        if show_prompt {
            let prompt = if multiline.is_some() {
                "eel| "
            } else {
                "eel> "
            };
            write!(output, "{prompt}")?;
            output.flush()?;
        }

        line.clear();
        if input.read_line(&mut line)? == 0 {
            if show_prompt {
                writeln!(output)?;
            }
            if multiline.is_some() {
                writeln!(diagnostics, "Unterminated multiline input: expected :}}")?;
            }
            return Ok(());
        }

        let trimmed = line.trim();
        let code = if let Some(buffer) = &mut multiline {
            if trimmed != ":}" {
                buffer.push_str(&line);
                continue;
            }
            multiline.take().unwrap()
        } else {
            match trimmed {
                "" => continue,
                ":help" => {
                    writeln!(output, "{HELP}")?;
                    continue;
                }
                ":quit" | ":q" => return Ok(()),
                ":{" => {
                    multiline = Some(String::new());
                    continue;
                }
                ":}" => {
                    writeln!(
                        diagnostics,
                        "No multiline input to end. Start one with :{{."
                    )?;
                    continue;
                }
                command if command.starts_with(':') => {
                    writeln!(diagnostics, "Unknown command: {command}. See :help.")?;
                    continue;
                }
                _ => std::mem::take(&mut line),
            }
        };

        match session.submit(code, &mut output, &mut diagnostics) {
            Ok(Some(value)) => writeln!(output, "{value}")?,
            Ok(None) => {}
            Err(SubmitError::Diagnostics(errors)) => session
                .sources
                .write_diagnostics(errors, &mut diagnostics)?,
            Err(SubmitError::Output(error)) => return Err(error),
        }
    }
}

#[derive(Debug)]
enum SubmitError {
    Diagnostics(Vec<Diagnostic>),
    Output(io::Error),
}

struct Session<'src> {
    sources: SourceMap<'src>,
    next_input: usize,
    env: Env<'src>,
    specs: Vec<OpSpec<'src>>,
}

impl<'src> Session<'src> {
    fn new(arena: &'src Arena<String>) -> Self {
        Self {
            sources: SourceMap::new(arena),
            next_input: 1,
            env: builtin::env(),
            specs: Vec::new(),
        }
    }

    /// Runs any action returned by an expression before updating `it`.
    fn submit(
        &mut self,
        input: String,
        output: impl Write,
        diagnostics: impl Write,
    ) -> Result<Option<Rc<Value<'src>>>, SubmitError> {
        let name = format!("<repl:{}>", self.next_input);
        self.next_input += 1;
        let source = self.sources.add(name, input);
        let toks = source.lex().map_err(SubmitError::Diagnostics)?;
        if toks.is_empty() {
            return Ok(None);
        }

        let eoi = source.eof();
        let (_, item_parser) = parse::expr_and_item_parsers();
        let item = item_parser
            .parse(toks.as_slice().map(eoi, Spanned::as_parts))
            .into_result()
            .map_err(|errors| {
                SubmitError::Diagnostics(errors.into_iter().map(Into::into).collect())
            })?;

        match item {
            ReplItem::Expr(expr) => {
                let expr = fixity::resolve(expr, &self.specs);
                let value = interp::eval(&expr, self.env.clone())
                    .map_err(|error| SubmitError::Diagnostics(vec![error.into()]))?;
                let value = Spanned::new(value, expr.span);
                let value = match io_action::run_result(value, output, diagnostics) {
                    Ok(value) => Rc::new(value),
                    Err(ActionError::Runtime(error)) => {
                        return Err(SubmitError::Diagnostics(vec![error.into()]));
                    }
                    Err(ActionError::Output(error)) => return Err(SubmitError::Output(error)),
                };

                self.env.insert_mut("it", Rc::clone(&value));

                // An implicit binding shadows any explicit fixity for `it`.
                self.specs.retain(|spec| spec.name != "it");
                Ok(Some(value))
            }
            ReplItem::Let(LetItem { patt, init }) => {
                let (expr, specs) = fixity::resolve_let(&patt.value, init, &self.specs);
                interp::bind(&patt, &expr, &mut self.env)
                    .map_err(|error| SubmitError::Diagnostics(vec![error.into()]))?;
                self.specs = specs;
                Ok(None)
            }
            ReplItem::LetRec(members) => {
                let (members, specs) = fixity::resolve_let_rec(members, &self.specs);
                interp::bind_rec_group(&members, &mut self.env)
                    .map_err(|error| SubmitError::Diagnostics(vec![error.into()]))?;
                self.specs = specs;
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn submit<'src>(
        session: &mut Session<'src>,
        input: &str,
    ) -> Result<Option<Rc<Value<'src>>>, SubmitError> {
        session.submit(input.into(), io::sink(), io::sink())
    }

    fn expect_diagnostics(session: &mut Session<'_>, input: &str) -> Vec<Diagnostic> {
        match submit(session, input) {
            Err(SubmitError::Diagnostics(errors)) => errors,
            other => panic!("expected diagnostics for {input:?}, got {other:?}"),
        }
    }

    fn run_with_input(input: &str, show_prompt: bool) -> (String, String) {
        let mut output = Vec::new();
        let mut diagnostics = Vec::new();
        run(input.as_bytes(), &mut output, &mut diagnostics, show_prompt).unwrap();
        (
            String::from_utf8(output).unwrap(),
            String::from_utf8(diagnostics).unwrap(),
        )
    }

    #[test]
    fn test_prompt_collects_multiline_input_and_runs_commands() {
        let (output, diagnostics) =
            run_with_input(":help\n:{\nlet x = {\n40 + 2\n}\n:}\nx\n:q\n0\n", true);
        assert_eq!(
            output,
            format!("eel> {HELP}\neel> eel| eel| eel| eel| eel> 42\neel> ")
        );
        assert!(diagnostics.is_empty(), "{diagnostics}");
    }

    #[test]
    fn test_prompt_continues_after_command_and_evaluation_errors() {
        let (output, diagnostics) =
            run_with_input("\n:}\n:nope\nlet x =\nmissing\n42\n:quit\n0\n", false);
        assert_eq!(output, "42\n");
        for message in [
            "No multiline input to end",
            "Unknown command: :nope",
            "expected",
            "Variable `missing` not found",
        ] {
            assert!(diagnostics.contains(message), "{diagnostics}");
        }
    }

    #[test]
    fn test_prompt_handles_eof_without_evaluating_unfinished_multiline_input() {
        let (output, diagnostics) = run_with_input("42", false);
        assert_eq!(output, "42\n");
        assert!(diagnostics.is_empty(), "{diagnostics}");

        let (output, diagnostics) = run_with_input(":{\n42\n", false);
        assert!(output.is_empty(), "{output}");
        assert!(diagnostics.contains("Unterminated multiline input: expected :}"));
    }

    #[test]
    fn test_io_actions_write_to_the_repl_writers() {
        let (output, diagnostics) =
            run_with_input("Io(print(1). io_bind fn(_) { eprint(2). })\n", false);
        assert_eq!(output, "1\nunit\n");
        assert_eq!(diagnostics, "2\n");
    }

    fn assert_result(session: &mut Session<'_>, input: &str, expected: i64) {
        let result = submit(session, input).unwrap();
        assert_eq!(result.as_deref(), Some(&Value::Int(expected)));
    }

    #[test]
    fn test_session_persists_bindings_and_last_successful_expression() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        expect_diagnostics(&mut session, "it");
        assert!(submit(&mut session, "let x = 40").unwrap().is_none());
        assert_result(&mut session, "x + 2", 42);
        assert!(submit(&mut session, "let x = x + 1").unwrap().is_none());
        for input in ["let x = 9223372036854775808", "let x =", "let x = 1 / 0"] {
            expect_diagnostics(&mut session, input);
        }
        assert_result(&mut session, "x + it", 83);
    }

    #[test]
    fn test_session_commits_fixity_only_after_successful_binding() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert!(submit(&mut session, "let opr 1 sub = -").unwrap().is_none());
        expect_diagnostics(&mut session, "let sub = missing");
        assert_result(&mut session, "10 sub 3 sub 2", 9);
        assert!(submit(&mut session, "let sub = -").unwrap().is_none());
        assert_result(&mut session, "10 sub 3 sub 2", 5);
    }

    #[test]
    fn test_repl_pattern_bindings_commit_together() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "42", 42);
        submit(&mut session, "let (x, opr 1 sub) = (20, -)").unwrap();
        for input in [
            "let (x, sub, fresh, 0) = (1, +, 2, 1)",
            "let (x, sub, fresh, opr 1 bad) = (1, +, 2, 0)",
        ] {
            expect_diagnostics(&mut session, input);
        }
        expect_diagnostics(&mut session, "fresh");
        assert_result(&mut session, "it", 42);
        assert_result(&mut session, "x sub 3 sub 2", 19);
    }

    #[test]
    fn test_session_installs_recursive_group_without_replacing_it() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "42", 42);

        let group = "let {
            first = fn() { 10 sub 3 sub 2 }
            opr 2 sub = fn(a, b) { a - b }
        }";
        assert!(submit(&mut session, group).unwrap().is_none());
        assert_result(&mut session, "it", 42);
        assert_result(&mut session, "first().", 9);
        assert_result(&mut session, "10 sub 3 sub 2", 9);
    }

    #[test]
    fn test_invalid_recursive_group_leaves_session_unchanged() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        submit(&mut session, "let opr 2 sub = -").unwrap();
        assert_result(&mut session, "42", 42);

        for group in [
            "let { opl 2 sub = fn(a, b) { a + b } fresh = fn() 0 sub = fn() 0 }",
            "let { opl 2 sub = fn(a, b) { a + b } fresh = fn() 0 bad = 0 }",
        ] {
            expect_diagnostics(&mut session, group);
        }

        expect_diagnostics(&mut session, "fresh");
        assert_result(&mut session, "it", 42);
        assert_result(&mut session, "10 sub 3 sub 2", 9);
    }

    #[test]
    fn test_recursive_let_with_continuation_is_local_in_repl() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "let { local = fn() 42 } local().", 42);
        expect_diagnostics(&mut session, "local");
        expect_diagnostics(&mut session, "let x = let { f = fn() 42 }");
    }

    #[test]
    fn test_implicit_it_shadows_explicit_fixity() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert!(submit(&mut session, "let opr 1 it = -").unwrap().is_none());
        assert!(submit(&mut session, "-").unwrap().is_some());
        assert_result(&mut session, "10 it 3 it 2", 5);
    }

    #[test]
    fn case_matches_the_captured_constructor_across_redefinitions() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        let definition = "let F = fn(x) { x }";
        submit(&mut session, definition).unwrap();
        submit(&mut session, "let alias = F").unwrap();
        submit(
            &mut session,
            "let G = fn(x) { case x { F(y) => y _ => 0 } }",
        )
        .unwrap();
        submit(&mut session, definition).unwrap();
        assert_result(&mut session, "G(F(42)).", 0);
        assert_result(&mut session, "G(alias(42)).", 42);
    }

    #[test]
    fn identical_redefinitions_keep_distinct_source_locations() {
        let (output, diagnostics) = run_with_input(
            "let F = fn() { missing }\nlet old = F\nlet F = fn() { missing }\nold().\nF().\n",
            false,
        );
        assert!(output.is_empty(), "{output}");
        for source in ["<repl:1>:1:16", "<repl:3>:1:16"] {
            assert!(diagnostics.contains(source), "{diagnostics}");
        }
    }

    #[test]
    fn test_session_keeps_lexical_bindings_and_source_locations() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "40", 40);
        submit(&mut session, "let old = fn() { it + 2 }").unwrap();
        assert_result(&mut session, "0", 0);
        assert_result(&mut session, "old().", 42);

        submit(&mut session, "let bad = fn() { missing }").unwrap();
        let errors = expect_diagnostics(&mut session, "bad().");
        let mut output = Vec::new();
        session
            .sources
            .write_diagnostics(errors, &mut output)
            .unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("<repl:5>:1:18"), "{output}");
    }
}
