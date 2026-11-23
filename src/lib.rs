use std::{
    fmt,
    io::{self, IsTerminal, Write},
    path::{Path, PathBuf},
};

use chumsky::prelude::*;

use crate::declare::Spanned;
use crate::io_action::ActionError;
use crate::source::SourceMap;

mod builtin;
mod declare;
mod fixity;
mod interp;
mod io_action;
mod lex;
mod parse;
mod repl;
mod source;

#[derive(Debug)]
pub enum RunError {
    Source { filename: PathBuf, error: io::Error },
    Lex,
    Parse,
    Runtime,
    Output(io::Error),
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Source { filename, error } => {
                write!(
                    f,
                    "Failed to read source file `{}`: {error}",
                    filename.display()
                )
            }
            Self::Lex => f.write_str("Lexing failed"),
            Self::Parse => f.write_str("Parsing failed"),
            Self::Runtime => f.write_str("Evaluation failed"),
            Self::Output(error) => write!(f, "Failed to write interpreter output: {error}"),
        }
    }
}

impl std::error::Error for RunError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Source { error, .. } | Self::Output(error) => Some(error),
            _ => None,
        }
    }
}

/// Runs the REPL on standard input, showing prompts only on a terminal.
pub fn run_repl() -> io::Result<()> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let show_prompt = stdin.is_terminal() && stdout.is_terminal();
    repl::run(stdin.lock(), stdout, io::stderr(), show_prompt)
}

/// Runs a source file and any returned `Io` action, then displays the result.
///
/// Returns an error if reading, lexing, parsing, evaluation, or output fails.
/// Source diagnostics are written to stderr. Recovered tokens and expressions
/// are never evaluated after lexer or parser errors.
pub fn run(filename: impl AsRef<Path>) -> Result<(), RunError> {
    let filename = filename.as_ref();
    let src = std::fs::read_to_string(filename).map_err(|error| RunError::Source {
        filename: filename.to_path_buf(),
        error,
    })?;

    let arena = typed_arena::Arena::new();
    let mut sources = SourceMap::new(&arena);
    let source = sources.add(filename.to_string_lossy().into_owned(), src);
    let toks = match source.lex() {
        Ok(toks) => toks,
        Err(errors) => {
            sources
                .write_diagnostics(errors, io::stderr().lock())
                .map_err(RunError::Output)?;
            return Err(RunError::Lex);
        }
    };
    let eoi = source.eof();

    let (expr, _) = parse::expr_and_item_parsers();
    let (syntax, parse_errs) = expr
        .parse(toks.as_slice().map(eoi, Spanned::as_parts))
        .into_output_errors();

    if !parse_errs.is_empty() {
        sources
            .write_diagnostics(parse_errs.into_iter().map(Into::into), io::stderr().lock())
            .map_err(RunError::Output)?;
        return Err(RunError::Parse);
    }

    let expr = fixity::resolve(syntax.ok_or(RunError::Parse)?, &[]);

    let mut output = io::stdout().lock();
    let mut diagnostics = io::stderr().lock();

    let result = interp::eval(&expr, builtin::env())
        .map_err(ActionError::Runtime)
        .and_then(|value| {
            io_action::run_result(
                Spanned::new(value, expr.span),
                &mut output,
                &mut diagnostics,
            )
        });

    match result {
        Ok(v) => writeln!(output, "Returned: {v}").map_err(RunError::Output),
        Err(ActionError::Output(error)) => Err(RunError::Output(error)),
        Err(ActionError::Runtime(e)) => {
            sources
                .write_diagnostics([e.into()], &mut diagnostics)
                .map_err(RunError::Output)?;
            Err(RunError::Runtime)
        }
    }
}
