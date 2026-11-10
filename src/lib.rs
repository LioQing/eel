use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;

mod ast;
mod builtin;
mod declare;
mod interp;
mod lex;

pub fn run(filename: impl AsRef<Path>) {
    let src = std::fs::read_to_string(&filename).expect("Failed to read source file");

    let (toks, lex_errs) = lex::lexer().parse(src.as_str()).into_output_errors();

    let ast_errs = if let Some(toks) = &toks {
        let eoi = (src.len()..src.len()).into();

        let (expr, ast_errs) = ast::expr_parser()
            .parse_with_state(
                toks.as_slice().map(eoi, |(t, s)| (t, s)),
                &mut extra::SimpleState(vec![]),
            )
            .into_output_errors();

        let go = |expr| {
            let Some(expr) = expr else {
                return ast_errs;
            };

            if !ast_errs.is_empty() {
                return ast_errs;
            }

            match interp::eval(&expr, builtin::env()) {
                Ok(v) => {
                    println!("Returned: {v}");
                    vec![]
                }
                Err(e) => vec![Rich::custom(e.span, e.msg)],
            }
        };

        go(expr)
    } else {
        vec![]
    };

    print_errs(
        filename.as_ref().to_string_lossy().to_string(),
        &src,
        lex_errs
            .into_iter()
            .map(|e| e.map_token(|tok| tok.to_string()))
            .chain(
                ast_errs
                    .into_iter()
                    .map(|e| e.map_token(|tok| tok.to_string())),
            ),
    );
}

fn print_errs<'src>(
    filename: String,
    src: &'src str,
    errs: impl IntoIterator<Item = Rich<'src, String>>,
) {
    errs.into_iter().for_each(|e| {
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
            .print(sources([(filename.clone(), src)]))
            .unwrap()
    });
}
