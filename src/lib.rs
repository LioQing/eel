use std::{
    fmt,
    io::{self, Write},
    path::{Path, PathBuf},
};

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;

mod builtin;
mod declare;
mod fixity;
mod interp;
mod lex;
mod parse;

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
    let filename = filename.to_string_lossy().into_owned();

    let (toks, lex_errs) = lex::lexer().parse(src.as_str()).into_output_errors();

    if !lex_errs.is_empty() {
        print_errs(
            filename,
            &src,
            lex_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .map_err(RunError::Output)?;
        return Err(RunError::Lex);
    }
    let toks = toks.ok_or(RunError::Lex)?;

    let eoi = (src.len()..src.len()).into();
    let (expr, ast_errs) = fixity::resolve_parser(parse::expr_parser())
        .parse_with_state(
            toks.as_slice().map(eoi, |(t, s)| (t, s)),
            &mut extra::SimpleState(vec![]),
        )
        .into_output_errors();

    if !ast_errs.is_empty() {
        print_errs(
            filename,
            &src,
            ast_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .map_err(RunError::Output)?;
        return Err(RunError::Parse);
    }

    let expr = expr.ok_or(RunError::Parse)?;

    match interp::eval(&expr, builtin::env()) {
        Ok(v) => writeln!(io::stdout().lock(), "Returned: {v}").map_err(RunError::Output),
        Err(e) => {
            print_errs(filename, &src, [Rich::custom(e.span, e.msg)]).map_err(RunError::Output)?;
            Err(RunError::Runtime)
        }
    }
}

fn print_errs<'src>(
    filename: String,
    src: &'src str,
    errs: impl IntoIterator<Item = Rich<'src, String>>,
) -> io::Result<()> {
    for e in errs {
        Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(e.to_string())
            .with_label(
                Label::new((filename.clone(), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((filename.clone(), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .eprint(sources([(filename.clone(), src)]))?;
    }
    Ok(())
}
