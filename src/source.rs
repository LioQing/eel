use std::{
    fmt,
    io::{self, Write},
};

use ariadne::{Color, Label, Report, ReportKind, Source as ReportSource};
use chumsky::prelude::*;
use typed_arena::Arena;

use crate::{
    declare::{Error, Span, Spanned, Token},
    lex,
};

#[derive(Debug)]
pub struct Diagnostic {
    span: Span,
    message: String,
    reason: String,
    contexts: Vec<(String, Span)>,
}

impl Diagnostic {
    fn from_rich(error: Rich<'_, impl fmt::Display>, offset: usize) -> Self {
        Self {
            span: offset_span(*error.span(), offset),
            message: error.to_string(),
            reason: error.reason().to_string(),
            contexts: error
                .contexts()
                .map(|(label, span)| (label.to_string(), offset_span(*span, offset)))
                .collect(),
        }
    }
}

impl<T: fmt::Display> From<Rich<'_, T>> for Diagnostic {
    fn from(error: Rich<'_, T>) -> Self {
        Self::from_rich(error, 0)
    }
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        Self {
            span: error.span,
            message: error.msg.clone(),
            reason: error.msg,
            contexts: Vec::new(),
        }
    }
}

fn offset_span(span: Span, offset: usize) -> Span {
    (span.start + offset..span.end + offset).into()
}

pub struct Source<'src> {
    name: String,
    text: &'src str,
    start: usize,
}

impl<'src> Source<'src> {
    pub fn eof(&self) -> Span {
        let end = self.start + self.text.len();
        (end..end).into()
    }

    pub fn lex(&self) -> Result<Vec<Spanned<Token<'src>>>, Vec<Diagnostic>> {
        // Convert local lexer offsets before returning them to the caller.
        let mut tokens = lex::lexer()
            .parse(self.text)
            .into_result()
            .map_err(|errors| {
                errors
                    .into_iter()
                    .map(|error| Diagnostic::from_rich(error, self.start))
                    .collect::<Vec<_>>()
            })?;
        for token in &mut tokens {
            token.span = offset_span(token.span, self.start);
        }
        Ok(tokens)
    }

    fn write_diagnostic(&self, error: Diagnostic, output: impl Write) -> io::Result<()> {
        let location = |span: Span| {
            (
                self.name.as_str(),
                span.start - self.start..span.end - self.start,
            )
        };
        Report::build(ReportKind::Error, location(error.span))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(error.message)
            .with_label(
                Label::new(location(error.span))
                    .with_message(error.reason)
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts.into_iter().map(|(label, span)| {
                Label::new(location(span))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write((self.name.as_str(), ReportSource::from(self.text)), output)
    }
}

/// Gives each source a disjoint byte range so saved AST spans still identify
/// their source even when evaluated by later REPL input.
pub struct SourceMap<'src> {
    arena: &'src Arena<String>,
    entries: Vec<Source<'src>>,
}

impl<'src> SourceMap<'src> {
    pub fn new(arena: &'src Arena<String>) -> Self {
        Self {
            arena,
            entries: Vec::new(),
        }
    }

    pub fn add(&mut self, name: String, text: String) -> &Source<'src> {
        // Leave the previous source's zero-width EOF span in its own range.
        let start = self.entries.last().map_or(0, |source| source.eof().end + 1);
        // ASTs borrow identifiers directly from input text.
        let text = self.arena.alloc(text).as_str();
        self.entries.push(Source { name, text, start });
        self.entries.last().unwrap()
    }

    pub fn write_diagnostics(
        &self,
        errors: impl IntoIterator<Item = Diagnostic>,
        mut output: impl Write,
    ) -> io::Result<()> {
        for error in errors {
            let index = self
                .entries
                .partition_point(|source| source.start <= error.span.start);
            let source = &self.entries[index - 1];
            // Parse contexts are from the same source as the primary span.
            source.write_diagnostic(error, &mut output)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{builtin, fixity, interp, parse};

    #[test]
    fn runtime_diagnostic_uses_the_original_source_and_byte_offsets() {
        let arena = Arena::new();
        let mut sources = SourceMap::new(&arena);
        sources.add("first".into(), "0".into());
        let source = sources.add("definition".into(), "fn() { // 🦀\n    missing\n}".into());
        let toks = source.lex().unwrap();
        let (expr, _) = parse::expr_and_item_parsers();
        let syntax = expr
            .parse(toks.as_slice().map(source.eof(), Spanned::as_parts))
            .unwrap();
        let closure = interp::eval(&fixity::resolve(syntax, &[]), builtin::env()).unwrap();

        let source = sources.add("call".into(), "f().".into());
        let toks = source.lex().unwrap();
        let (expr, _) = parse::expr_and_item_parsers();
        let syntax = expr
            .parse(toks.as_slice().map(source.eof(), Spanned::as_parts))
            .unwrap();
        let env = builtin::env().insert("f", std::rc::Rc::new(closure));
        let error = interp::eval(&fixity::resolve(syntax, &[]), env).unwrap_err();

        let mut output = Vec::new();
        sources
            .write_diagnostics([error.into()], &mut output)
            .unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("definition:2:5"), "{output}");
        assert!(output.contains("Variable `missing` not found"), "{output}");
    }

    #[test]
    fn lexer_returns_diagnostics_for_the_correct_source() {
        let arena = Arena::new();
        let mut sources = SourceMap::new(&arena);
        sources.add("first".into(), "0".into());
        let source = sources.add("second".into(), "  9223372036854775808".into());
        let errors = source.lex().unwrap_err();
        let mut output = Vec::new();
        sources.write_diagnostics(errors, &mut output).unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("second:1:3"), "{output}");
    }

    #[test]
    fn parser_diagnostic_at_eof_stays_with_its_source() {
        let arena = Arena::new();
        let mut sources = SourceMap::new(&arena);
        sources.add("first".into(), "0".into());
        let source = sources.add("incomplete".into(), "let x =".into());
        let toks = source.lex().unwrap();
        let (expr, _) = parse::expr_and_item_parsers();
        let errors = expr
            .parse(toks.as_slice().map(source.eof(), Spanned::as_parts))
            .into_errors();
        sources.add("next".into(), "42".into());

        let mut output = Vec::new();
        sources
            .write_diagnostics(errors.into_iter().map(Into::into), &mut output)
            .unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.contains("incomplete:1:8"), "{output}");
    }
}
