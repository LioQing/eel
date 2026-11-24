use std::{
    fmt, io,
    path::{Path, PathBuf},
};

use chumsky::prelude::*;

use crate::lang::{
    builtin,
    declare::{Spanned, Value},
    fixity, interp, parse,
};

use super::{
    io_action::{self, ActionError},
    source::{Diagnostic, SourceMap},
};

#[derive(Debug)]
pub enum RunError {
    Source { filename: PathBuf, error: io::Error },
    Lex,
    Parse,
    Runtime,
    InvalidExitCode,
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
            Self::InvalidExitCode => f.write_str("Invalid program exit code"),
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

pub fn run(filename: &Path) -> Result<u8, RunError> {
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

    let value = match result {
        Ok(value) => value,
        Err(ActionError::Output(error)) => return Err(RunError::Output(error)),
        Err(ActionError::Runtime(e)) => {
            sources
                .write_diagnostics([e.into()], &mut diagnostics)
                .map_err(RunError::Output)?;
            return Err(RunError::Runtime);
        }
    };

    let message = match value {
        Value::Unit => return Ok(0),
        Value::Int(code) => match u8::try_from(code) {
            Ok(code) => return Ok(code),
            Err(_) => format!("Exit code must be in 0..=255, got {code}"),
        },
        _ => "Expected an integer exit code or `unit`".into(),
    };

    sources
        .write_diagnostics([Diagnostic::new(expr.span, message)], &mut diagnostics)
        .map_err(RunError::Output)?;

    Err(RunError::InvalidExitCode)
}
