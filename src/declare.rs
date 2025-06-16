use std::fmt;

use chumsky::prelude::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub trait SpanlessPartialEq {
    fn spanless_eq(&self, other: &Self) -> bool;
}

impl<T: SpanlessPartialEq> SpanlessPartialEq for Spanned<T> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.0.spanless_eq(&other.0)
    }
}

impl<T: SpanlessPartialEq> SpanlessPartialEq for Vec<T> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(l, r)| l.spanless_eq(r))
    }
}

impl SpanlessPartialEq for Vec<&str> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(l, r)| l.eq(r))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    Float(f64),
    Int(i64),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Dot,
    Fn,
    Let,
    Case,
    Opl,
    Opr,
    PattArm,
    Assign,
    Ident(&'src str),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Float(value) => write!(f, "{value}"),
            Token::Int(value) => write!(f, "{value}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Case => write!(f, "case"),
            Token::Opl => write!(f, "opl"),
            Token::Opr => write!(f, "opr"),
            Token::PattArm => write!(f, "=>"),
            Token::Assign => write!(f, "="),
            Token::Ident(ident) => write!(f, "{ident}"),
        }
    }
}

impl SpanlessPartialEq for Token<'_> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub msg: String,
    pub span: Span,
}

pub type Env<'src> = Vec<(&'src str, Value<'src>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    Float(f64),
    Int(i64),
    Closure(
        Option<&'src str>,
        Vec<&'src str>,
        Box<Spanned<Expr<'src>>>,
        usize,
    ),
    Struct(
        Option<&'src str>,
        Box<Spanned<Expr<'src>>>,
        Env<'src>,
        usize,
    ),
}

impl<'src> fmt::Display for Value<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Float(value) => write!(f, "{value}"),
            Value::Int(value) => write!(f, "{value}"),
            Value::Closure(name, params, body, _) => {
                write!(
                    f,
                    "fn {}({}) {{ {} }}",
                    match name {
                        Some(n) => format!(" {n}"),
                        None => String::new(),
                    },
                    params
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    body.0,
                )
            }
            Value::Struct(name, expr, args_env, _) => {
                write!(
                    f,
                    "{}({}) {{ {} }}",
                    match name {
                        Some(n) => format!("{n} "),
                        None => String::new(),
                    },
                    args_env
                        .iter()
                        .map(|(k, v)| format!("{k}={v}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    expr.0,
                )
            }
        }
    }
}

impl<'src> SpanlessPartialEq for Value<'src> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Patt<'src> {
    Var(&'src str),
    Lit(Value<'src>),
    Struct(&'src str, Spanned<Vec<Spanned<Self>>>),
}

impl<'src> fmt::Display for Patt<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Patt::Var(ident) => write!(f, "{ident}"),
            Patt::Lit(value) => write!(f, "{value}"),
            Patt::Struct(name, fields) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    fields
                        .0
                        .iter()
                        .map(|p| p.0.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
        }
    }
}

impl<'src> SpanlessPartialEq for Patt<'src> {
    fn spanless_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Patt::Var(l), Patt::Var(r)) => l == r,
            (Patt::Lit(l), Patt::Lit(r)) => l.spanless_eq(r),
            (Patt::Struct(l_name, l_fields), Patt::Struct(r_name, r_fields)) => {
                l_name == r_name && l_fields.spanless_eq(r_fields)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'src> {
    Err,
    Lit(Value<'src>),
    Var(&'src str),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
    Fn(Spanned<Vec<&'src str>>, Box<Spanned<Self>>),
    Case(
        Box<Spanned<Self>>,
        Spanned<Vec<(Spanned<Patt<'src>>, Box<Spanned<Self>>)>>,
    ),
    Op(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let(
        Option<(OpAssoc, i64)>,
        &'src str,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Seq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Builtin(fn(&Spanned<Expr<'src>>, &Env<'src>) -> Result<Value<'src>, Error>),
}

impl<'src> fmt::Display for Expr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Err => write!(f, "<error>"),
            Expr::Lit(value) => write!(f, "{value}"),
            Expr::Var(ident) => write!(f, "{ident}"),
            Expr::Call(func, args) => write!(
                f,
                "{}({})",
                func.0,
                args.0
                    .iter()
                    .map(|e| e.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Expr::Fn(params, body) => {
                write!(
                    f,
                    "fn({}) {{{}}}",
                    params
                        .0
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    body.0,
                )
            }
            Expr::Case(expr, arms) => {
                write!(
                    f,
                    "case {} {{ {} }}",
                    expr.0,
                    arms.0
                        .iter()
                        .map(|(patt, body)| format!("{} => {}", patt.0, body.0))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            Expr::Op(l, op, r) => write!(f, "({} {} {})", l.0, op.0, r.0),
            Expr::Let(op_spec, ident, assign, body) => {
                write!(
                    f,
                    "let{} {} = {} {}",
                    match op_spec {
                        Some((OpAssoc::Left, prec)) => format!(" opl {prec}"),
                        Some((OpAssoc::Right, prec)) => format!(" opr {prec}"),
                        None => "".to_string(),
                    },
                    ident,
                    assign.0,
                    body.0
                )
            }
            Expr::Seq(first, second) => write!(f, "{} {}", first.0, second.0),
            Expr::Builtin(_) => write!(f, "<builtin>"),
        }
    }
}

impl<'src> SpanlessPartialEq for Expr<'src> {
    fn spanless_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Err, Expr::Err) => true,
            (Expr::Lit(l), Expr::Lit(r)) => l.spanless_eq(r),
            (Expr::Var(l), Expr::Var(r)) => l == r,
            (Expr::Call(l_func, l_args), Expr::Call(r_func, r_args)) => {
                l_func.spanless_eq(r_func) && l_args.spanless_eq(r_args)
            }
            (Expr::Fn(l_params, l_body), Expr::Fn(r_params, r_body)) => {
                l_params.spanless_eq(r_params) && l_body.spanless_eq(r_body)
            }
            (Expr::Case(l_expr, l_arms), Expr::Case(r_expr, r_arms)) => {
                l_expr.spanless_eq(r_expr)
                    && l_arms.0.iter().zip(r_arms.0.iter()).all(
                        |((l_patt, l_body), (r_patt, r_body))| {
                            l_patt.spanless_eq(r_patt) && l_body.spanless_eq(r_body)
                        },
                    )
            }
            (Expr::Op(l_left, l_op, l_right), Expr::Op(r_left, r_op, r_right)) => {
                l_left.spanless_eq(r_left) && l_op.spanless_eq(r_op) && l_right.spanless_eq(r_right)
            }
            (
                Expr::Let(l_op_spec, l_ident, l_assign, l_body),
                Expr::Let(r_op_spec, r_ident, r_assign, r_body),
            ) => {
                l_op_spec == r_op_spec
                    && l_ident == r_ident
                    && l_assign.spanless_eq(r_assign)
                    && l_body.spanless_eq(r_body)
            }
            (Expr::Seq(l_first, l_second), Expr::Seq(r_first, r_second)) => {
                l_first.spanless_eq(r_first) && l_second.spanless_eq(r_second)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum OpAssoc {
    #[default]
    Left,
    Right,
}

impl fmt::Display for OpAssoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpAssoc::Left => write!(f, "left"),
            OpAssoc::Right => write!(f, "right"),
        }
    }
}

pub type OpSpec<'src> = (&'src str, OpAssoc, i64);

pub type OpSpecs<'src> = extra::SimpleState<Vec<OpSpec<'src>>>;
