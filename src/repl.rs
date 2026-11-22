use std::{
    io::{self, BufRead, Write},
    rc::Rc,
};

use chumsky::prelude::*;
use typed_arena::Arena;

use crate::{
    builtin,
    declare::{Env, OpSpec, ReplItem, Value},
    fixity, interp, parse,
    source::{Diagnostic, SourceMap},
};

const HELP: &str = "\
Enter an expression to evaluate it, `let name = expression` to define a binding, 
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

        match session.eval(code) {
            Ok(Some(value)) => writeln!(output, "{value}")?,
            Ok(None) => {}
            Err(errors) => session
                .sources
                .write_diagnostics(errors, &mut diagnostics)?,
        }
    }
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

    /// Returns a value only for expressions. The caller decides what to display.
    fn eval(&mut self, input: String) -> Result<Option<Rc<Value<'src>>>, Vec<Diagnostic>> {
        let name = format!("<repl:{}>", self.next_input);
        self.next_input += 1;
        let source = self.sources.add(name, input);
        let toks = source.lex()?;
        if toks.is_empty() {
            return Ok(None);
        }

        let eoi = source.eof();
        let (_, item_parser) = parse::expr_and_item_parsers();
        let item = item_parser
            .parse(toks.as_slice().map(eoi, |(t, s)| (t, s)))
            .into_result()
            .map_err(|errors| errors.into_iter().map(Into::into).collect::<Vec<_>>())?;

        match item {
            ReplItem::Expr(expr) => {
                let expr = fixity::resolve(expr, &self.specs);
                let value =
                    Rc::new(interp::eval(&expr, self.env.clone()).map_err(|err| vec![err.into()])?);
                self.env.insert_mut("it", Rc::clone(&value));

                // An implicit binding shadows any explicit fixity for `it`.
                self.specs.retain(|(name, ..)| *name != "it");

                Ok(Some(value))
            }
            ReplItem::Let(op_spec, name, expr) => {
                let (expr, specs) = fixity::resolve_let(name, op_spec, expr, &self.specs);
                interp::bind(name, &expr, &mut self.env).map_err(|err| vec![err.into()])?;
                self.specs = specs;
                Ok(None)
            }
            ReplItem::LetRec(members) => {
                let (members, specs) = fixity::resolve_let_rec(members, &self.specs);
                interp::bind_rec_group(&members, &mut self.env);
                self.specs = specs;
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    fn assert_result(session: &mut Session<'_>, input: &str, expected: i64) {
        let result = session.eval(input.into()).unwrap();
        assert_eq!(result.as_deref(), Some(&Value::Int(expected)));
    }

    #[test]
    fn test_session_persists_bindings_and_last_successful_expression() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert!(session.eval("it".into()).is_err());
        assert!(session.eval("let x = 40".into()).unwrap().is_none());
        assert_result(&mut session, "x + 2", 42);
        assert!(session.eval("let x = x + 1".into()).unwrap().is_none());
        for input in ["let x = 9223372036854775808", "let x =", "let x = 1 / 0"] {
            assert!(session.eval(input.into()).is_err(), "{input}");
        }
        assert_result(&mut session, "x + it", 83);
    }

    #[test]
    fn test_session_commits_fixity_only_after_successful_binding() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert!(session.eval("let opr 1 sub = -".into()).unwrap().is_none());
        assert!(session.eval("let sub = missing".into()).is_err());
        assert_result(&mut session, "10 sub 3 sub 2", 9);
        assert!(session.eval("let sub = -".into()).unwrap().is_none());
        assert_result(&mut session, "10 sub 3 sub 2", 5);
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
        assert!(session.eval(group.into()).unwrap().is_none());
        assert_result(&mut session, "it", 42);
        assert_result(&mut session, "first().", 9);
        assert_result(&mut session, "10 sub 3 sub 2", 9);
    }

    #[test]
    fn test_invalid_recursive_group_leaves_session_unchanged() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        session.eval("let opr 2 sub = -".into()).unwrap();
        assert_result(&mut session, "42", 42);

        for group in [
            "let { opl 2 sub = fn(a, b) { a + b } fresh = fn() 0 sub = fn() 0 }",
            "let { opl 2 sub = fn(a, b) { a + b } fresh = fn() 0 bad = 0 }",
        ] {
            assert!(session.eval(group.into()).is_err(), "{group}");
        }

        assert!(session.eval("fresh".into()).is_err());
        assert_result(&mut session, "it", 42);
        assert_result(&mut session, "10 sub 3 sub 2", 9);
    }

    #[test]
    fn test_recursive_let_with_continuation_is_local_in_repl() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "let { local = fn() 42 } local().", 42);
        assert!(session.eval("local".into()).is_err());
        assert!(session.eval("let x = let { f = fn() 42 }".into()).is_err());
    }

    #[test]
    fn test_implicit_it_shadows_explicit_fixity() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert!(session.eval("let opr 1 it = -".into()).unwrap().is_none());
        assert!(session.eval("-".into()).unwrap().is_some());
        assert_result(&mut session, "10 it 3 it 2", 5);
    }

    #[test]
    fn test_session_keeps_lexical_bindings_and_source_locations() {
        let arena = Arena::new();
        let mut session = Session::new(&arena);
        assert_result(&mut session, "40", 40);
        session.eval("let old = fn() { it + 2 }".into()).unwrap();
        assert_result(&mut session, "0", 0);
        assert_result(&mut session, "old().", 42);

        session.eval("let bad = fn() { missing }".into()).unwrap();
        let errors = session.eval("bad().".into()).unwrap_err();
        let mut output = Vec::new();
        session
            .sources
            .write_diagnostics(errors, &mut output)
            .unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("<repl:5>:1:18"), "{output}");
    }
}
