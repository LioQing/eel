use chumsky::{input::ValueInput, prelude::*};

use crate::declare::*;

pub fn resolve_parser<
    'toks,
    'src: 'toks,
    I: ValueInput<'toks, Token = Token<'src>, Span = Span>,
    P: Parser<
            'toks,
            I,
            Spanned<SyntaxExpr<'src>>,
            extra::Full<Rich<'toks, Token<'src>, Span>, OpSpecs<'src>, ()>,
        > + Clone,
>(
    parser: P,
) -> impl Parser<
    'toks,
    I,
    Spanned<Expr<'src>>,
    extra::Full<Rich<'toks, Token<'src>, Span>, OpSpecs<'src>, ()>,
> + Clone {
    parser.map_with(|expr, e| {
        let specs: &mut OpSpecs<'src> = e.state();
        resolve(expr, &specs.0)
    })
}

fn op_spec(op: &Spanned<Expr<'_>>, specs: &[OpSpec<'_>]) -> (OpAssoc, i64) {
    if let Expr::Var(name) = &op.0 {
        if let Some((_, assoc, prec)) = specs.iter().rev().find(|(x, _, _)| x == name) {
            return (*assoc, *prec);
        }

        let prec = match *name {
            "==" | "!=" => 1,
            "<" | "<=" | ">" | ">=" => 2,
            "+" | "-" => 3,
            "*" | "/" | "%" => 4,
            _ => 0,
        };
        (OpAssoc::Left, prec)
    } else {
        (OpAssoc::Left, 0)
    }
}

fn reduce_op<'src>(operands: &mut Vec<Spanned<Expr<'src>>>, op: Spanned<Expr<'src>>) {
    let right = operands.pop().unwrap();
    let left = operands.pop().unwrap();
    let span = (left.1.start..right.1.end).into();
    operands.push((
        Expr::Op(Box::new(left), Box::new(op), Box::new(right)),
        span,
    ));
}

fn shadow_patt<'src>(patt: &Patt<'src>, specs: &mut Vec<OpSpec<'src>>) {
    match patt {
        Patt::Var(name) => specs.push((name, OpAssoc::Left, 0)),
        Patt::Struct(_, fields) => {
            for (patt, _) in &fields.0 {
                shadow_patt(patt, specs);
            }
        }
        Patt::Lit(_) => {}
    }
}

fn resolve<'src>(
    (expr, span): Spanned<SyntaxExpr<'src>>,
    specs: &[OpSpec<'src>],
) -> Spanned<Expr<'src>> {
    let expr = match expr {
        SyntaxExpr::Atom(expr) => expr,
        SyntaxExpr::Call(func, (args, args_span)) => Expr::Call(
            Box::new(resolve(*func, specs)),
            (
                args.into_iter().map(|arg| resolve(arg, specs)).collect(),
                args_span,
            ),
        ),
        SyntaxExpr::Fn(params, body) => {
            let mut scoped_specs = specs.to_vec();
            scoped_specs.extend(params.0.iter().map(|name| (*name, OpAssoc::Left, 0)));
            Expr::Fn(params, Box::new(resolve(*body, &scoped_specs)))
        }
        SyntaxExpr::Case(expr, (arms, arms_span)) => Expr::Case(
            Box::new(resolve(*expr, specs)),
            (
                arms.into_iter()
                    .map(|(patt, body)| {
                        let mut scoped_specs = specs.to_vec();
                        shadow_patt(&patt.0, &mut scoped_specs);
                        (patt, Box::new(resolve(*body, &scoped_specs)))
                    })
                    .collect(),
                arms_span,
            ),
        ),
        SyntaxExpr::Ops(first, rest) => {
            let mut operands = vec![resolve(*first, specs)];
            let mut operators = Vec::new();
            for (op, right) in rest {
                let op = resolve(op, specs);
                let (assoc, prec) = op_spec(&op, specs);
                while operators.last().is_some_and(|prev| {
                    let (_, prev_prec) = op_spec(prev, specs);
                    prev_prec > prec || (prev_prec == prec && assoc == OpAssoc::Left)
                }) {
                    reduce_op(&mut operands, operators.pop().unwrap());
                }
                operators.push(op);
                operands.push(resolve(right, specs));
            }
            while let Some(op) = operators.pop() {
                reduce_op(&mut operands, op);
            }
            return (operands.pop().unwrap().0, span);
        }
        SyntaxExpr::Let(spec, name, assign, body) => {
            let mut scoped_specs = specs.to_vec();
            let (assoc, prec) = spec.unwrap_or((OpAssoc::Left, 0));
            scoped_specs.push((name, assoc, prec));
            // A function binding is recursive, but an ordinary initializer still sees
            // the previous binding, just as it does during evaluation.
            let assign_specs = if matches!(&assign.0, SyntaxExpr::Fn(..)) {
                scoped_specs.as_slice()
            } else {
                specs
            };
            Expr::Let(
                spec,
                name,
                Box::new(resolve(*assign, assign_specs)),
                Box::new(resolve(*body, &scoped_specs)),
            )
        }
    };
    (expr, span)
}
