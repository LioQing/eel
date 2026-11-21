use std::{
    fmt,
    io::{self, IsTerminal, Write},
    path::{Path, PathBuf},
};

use chumsky::prelude::*;

use crate::source::SourceMap;

mod builtin;
mod declare;
mod fixity;
mod interp;
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

/// Runs a source file, printing its returned value or diagnostics.
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
    let (syntax, parse_errs) = parse::expr_parser()
        .parse(toks.as_slice().map(eoi, |(t, s)| (t, s)))
        .into_output_errors();

    if !parse_errs.is_empty() {
        sources
            .write_diagnostics(parse_errs.into_iter().map(Into::into), io::stderr().lock())
            .map_err(RunError::Output)?;
        return Err(RunError::Parse);
    }

    let expr = fixity::resolve(syntax.ok_or(RunError::Parse)?, &[]);

    match interp::eval(&expr, builtin::env()) {
        Ok(v) => writeln!(io::stdout().lock(), "Returned: {v}").map_err(RunError::Output),
        Err(e) => {
            sources
                .write_diagnostics([e.into()], io::stderr().lock())
                .map_err(RunError::Output)?;
            Err(RunError::Runtime)
        }
    }
}
