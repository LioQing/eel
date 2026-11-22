pub use std::fmt;
use std::rc::Rc;

use chumsky::span::SimpleSpan;

pub use fixity::*;
pub use interp::*;
pub use lex::*;
pub use parse::*;

pub type Span = SimpleSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: impl Into<Span>) -> Self {
        Self {
            value,
            span: span.into(),
        }
    }

    pub fn as_parts(&self) -> (&T, &Span) {
        (&self.value, &self.span)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned::new(f(self.value), self.span)
    }
}

#[allow(dead_code)] // trait only used by tests
pub trait SpanlessPartialEq {
    fn spanless_eq(&self, other: &Self) -> bool;
}

impl<T: SpanlessPartialEq> SpanlessPartialEq for Spanned<T> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.value.spanless_eq(&other.value)
    }
}

impl<T: SpanlessPartialEq> SpanlessPartialEq for Box<T> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.as_ref().spanless_eq(other.as_ref())
    }
}

impl<T: SpanlessPartialEq> SpanlessPartialEq for Rc<T> {
    fn spanless_eq(&self, other: &Self) -> bool {
        self.as_ref().spanless_eq(other.as_ref())
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

mod lex {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Token<'src> {
        Unit,
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
                Token::Unit => write!(f, "unit"),
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
}

mod parse {
    use super::*;

    pub(super) fn fmt_fixity(f: &mut fmt::Formatter<'_>, fixity: Option<Fixity>) -> fmt::Result {
        match fixity {
            Some(Fixity {
                assoc: OpAssoc::Left,
                precedence,
            }) => write!(f, " opl {precedence}"),
            Some(Fixity {
                assoc: OpAssoc::Right,
                precedence,
            }) => write!(f, " opr {precedence}"),
            None => Ok(()),
        }
    }

    pub(super) fn fmt_let(
        f: &mut fmt::Formatter<'_>,
        fixity: Option<Fixity>,
        name: &str,
        init: &impl fmt::Display,
    ) -> fmt::Result {
        write!(f, "let")?;
        fmt_fixity(f, fixity)?;
        write!(f, " {name} = {init}")
    }

    fn fmt_let_rec<'src>(
        f: &mut fmt::Formatter<'_>,
        bindings: &[RecFn<'src, Spanned<SyntaxExpr<'src>>>],
    ) -> fmt::Result {
        write!(f, "let {{")?;
        for binding in bindings {
            fmt_fixity(f, binding.fixity)?;
            write!(
                f,
                " {} = fn({}) {{ {} }}",
                binding.name.value,
                binding.params.value.join(", "),
                binding.body.value,
            )?;
        }
        write!(f, " }}")
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct RecFn<'src, Body> {
        pub name: Spanned<&'src str>,
        pub fixity: Option<Fixity>,
        pub params: Spanned<Vec<&'src str>>,
        pub body: Body,
    }

    impl<Body: SpanlessPartialEq> SpanlessPartialEq for RecFn<'_, Body> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.name.value == other.name.value
                && self.fixity == other.fixity
                && self.params.spanless_eq(&other.params)
                && self.body.spanless_eq(&other.body)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Call<E> {
        pub callee: Box<Spanned<E>>,
        pub args: Spanned<Vec<Spanned<E>>>,
    }

    impl<E: SpanlessPartialEq> SpanlessPartialEq for Call<E> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.callee.spanless_eq(&other.callee) && self.args.spanless_eq(&other.args)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct FnExpr<'src, Body> {
        pub params: Spanned<Vec<&'src str>>,
        pub body: Body,
    }

    impl<Body: SpanlessPartialEq> SpanlessPartialEq for FnExpr<'_, Body> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.params.spanless_eq(&other.params) && self.body.spanless_eq(&other.body)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Case<'src, E> {
        pub scrutinee: Box<Spanned<E>>,
        pub arms: Spanned<Vec<CaseArm<'src, E>>>,
    }

    impl<E: SpanlessPartialEq> SpanlessPartialEq for Case<'_, E> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.scrutinee.spanless_eq(&other.scrutinee) && self.arms.spanless_eq(&other.arms)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct CaseArm<'src, E> {
        pub patt: Spanned<Patt<'src>>,
        pub body: Box<Spanned<E>>,
    }

    impl<E: SpanlessPartialEq> SpanlessPartialEq for CaseArm<'_, E> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.patt.spanless_eq(&other.patt) && self.body.spanless_eq(&other.body)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Let<'src, E> {
        pub fixity: Option<Fixity>,
        pub name: &'src str,
        pub init: Box<Spanned<E>>,
        pub body: Box<Spanned<E>>,
    }

    impl<E: SpanlessPartialEq> SpanlessPartialEq for Let<'_, E> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.fixity == other.fixity
                && self.name == other.name
                && self.init.spanless_eq(&other.init)
                && self.body.spanless_eq(&other.body)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct LetRec<'src, E, FnBody> {
        pub bindings: Vec<RecFn<'src, FnBody>>,
        pub body: Box<Spanned<E>>,
    }

    impl<E: SpanlessPartialEq, FnBody: SpanlessPartialEq> SpanlessPartialEq for LetRec<'_, E, FnBody> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.bindings.spanless_eq(&other.bindings) && self.body.spanless_eq(&other.body)
        }
    }

    #[derive(Debug)]
    pub struct LetBinding<'src> {
        pub fixity: Option<Fixity>,
        pub name: &'src str,
        pub init: Spanned<SyntaxExpr<'src>>,
    }

    impl SpanlessPartialEq for LetBinding<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.fixity == other.fixity
                && self.name == other.name
                && self.init.spanless_eq(&other.init)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Ops<'src> {
        pub first: Box<Spanned<SyntaxExpr<'src>>>,
        pub rest: Vec<OpStep<'src>>,
    }

    impl SpanlessPartialEq for Ops<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.first.spanless_eq(&other.first) && self.rest.spanless_eq(&other.rest)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct OpStep<'src> {
        pub op: Spanned<SyntaxExpr<'src>>,
        pub rhs: Spanned<SyntaxExpr<'src>>,
    }

    impl SpanlessPartialEq for OpStep<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.op.spanless_eq(&other.op) && self.rhs.spanless_eq(&other.rhs)
        }
    }

    #[derive(Debug)]
    pub enum ReplItem<'src> {
        Expr(Spanned<SyntaxExpr<'src>>),
        Let(LetBinding<'src>),
        LetRec(Vec<RecFn<'src, Spanned<SyntaxExpr<'src>>>>),
    }

    impl fmt::Display for ReplItem<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Expr(expr) => write!(f, "{}", expr.value),
                Self::Let(LetBinding { fixity, name, init }) => {
                    fmt_let(f, *fixity, name, &init.value)
                }
                Self::LetRec(bindings) => fmt_let_rec(f, bindings),
            }
        }
    }

    impl SpanlessPartialEq for ReplItem<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Expr(l), Self::Expr(r)) => l.spanless_eq(r),
                (Self::Let(l), Self::Let(r)) => l.spanless_eq(r),
                (Self::LetRec(l), Self::LetRec(r)) => l.spanless_eq(r),
                _ => false,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum SyntaxExpr<'src> {
        Atom(Expr<'src>),
        Call(Call<Self>),
        Fn(FnExpr<'src, Box<Spanned<Self>>>),
        Case(Case<'src, Self>),
        Ops(Ops<'src>),
        Let(Let<'src, Self>),
        LetRec(LetRec<'src, Self, Spanned<Self>>),
    }

    impl<'src> fmt::Display for SyntaxExpr<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                SyntaxExpr::Atom(expr) => write!(f, "{expr}"),
                SyntaxExpr::Call(Call { callee, args }) => write!(
                    f,
                    "{}({})",
                    callee.value,
                    args.value
                        .iter()
                        .map(|e| e.value.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                SyntaxExpr::Fn(FnExpr { params, body }) => {
                    write!(
                        f,
                        "fn({}) {{{}}}",
                        params
                            .value
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.value,
                    )
                }
                SyntaxExpr::Case(Case { scrutinee, arms }) => {
                    write!(
                        f,
                        "case {} {{ {} }}",
                        scrutinee.value,
                        arms.value
                            .iter()
                            .map(|CaseArm { patt, body }| format!(
                                "{} => {}",
                                patt.value, body.value
                            ))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                SyntaxExpr::Ops(Ops { first, rest }) => {
                    let mut result = first.value.to_string();
                    for OpStep { op, rhs } in rest {
                        result.push_str(&format!(" {} {}", op.value, rhs.value));
                    }
                    write!(f, "({result})")
                }
                SyntaxExpr::Let(Let {
                    fixity,
                    name,
                    init,
                    body,
                }) => {
                    fmt_let(f, *fixity, name, &init.value)?;
                    write!(f, " {}", body.value)
                }
                SyntaxExpr::LetRec(LetRec { bindings, body }) => {
                    fmt_let_rec(f, bindings)?;
                    write!(f, " {}", body.value)
                }
            }
        }
    }

    impl SpanlessPartialEq for SyntaxExpr<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            match (self, other) {
                (SyntaxExpr::Atom(l), SyntaxExpr::Atom(r)) => l.spanless_eq(r),
                (SyntaxExpr::Call(l), SyntaxExpr::Call(r)) => l.spanless_eq(r),
                (SyntaxExpr::Fn(l), SyntaxExpr::Fn(r)) => l.spanless_eq(r),
                (SyntaxExpr::Case(l), SyntaxExpr::Case(r)) => l.spanless_eq(r),
                (SyntaxExpr::Ops(l), SyntaxExpr::Ops(r)) => l.spanless_eq(r),
                (SyntaxExpr::Let(l), SyntaxExpr::Let(r)) => l.spanless_eq(r),
                (SyntaxExpr::LetRec(l), SyntaxExpr::LetRec(r)) => l.spanless_eq(r),
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

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct Fixity {
        pub assoc: OpAssoc,
        pub precedence: i64,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct OpSpec<'src> {
        pub name: &'src str,
        pub fixity: Fixity,
    }
}

mod fixity {
    use super::parse::{fmt_fixity, fmt_let};
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Closure<'src> {
        pub name: Option<&'src str>, // Display and struct name
        pub group: Rc<ClosureGroup<'src>>,
        pub member: usize,
    }

    impl<'src> Closure<'src> {
        pub fn definition(&self) -> &ClosureDef<'src> {
            &self.group.members[self.member]
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct ClosureGroup<'src> {
        pub env: Env<'src>,
        pub members: Box<[ClosureDef<'src>]>,
    }

    #[derive(Debug, PartialEq)]
    pub struct ClosureDef<'src> {
        pub rec_name: Option<&'src str>,
        pub params: Vec<&'src str>,
        pub body: Rc<Spanned<Expr<'src>>>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Struct<'src> {
        pub name: Option<&'src str>,
        pub body: Rc<Spanned<Expr<'src>>>,
        pub args: StructArgs<'src>,
        pub applied_env: Env<'src>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Value<'src> {
        Unit,
        Float(f64),
        Int(i64),
        Closure(Closure<'src>),
        Struct(Struct<'src>),
    }

    impl<'src> fmt::Display for Value<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Value::Unit => write!(f, "unit"),
                Value::Float(value) => write!(f, "{value}"),
                Value::Int(value) => write!(f, "{value}"),
                Value::Closure(closure) => {
                    let ClosureDef {
                        rec_name,
                        params,
                        body,
                    } = closure.definition();
                    write!(
                        f,
                        "fn{}{}({}) {{ {} }}",
                        match rec_name {
                            Some(n) => format!(" {n}"),
                            None => String::new(),
                        },
                        match closure.name {
                            Some(n) if Some(n) != *rec_name => format!("`{n}`"),
                            _ => String::new(),
                        },
                        params
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.value,
                    )
                }
                Value::Struct(Struct {
                    name, body, args, ..
                }) => {
                    write!(
                        f,
                        "{}({}) {{ {} }}",
                        name.unwrap_or(""),
                        args.iter()
                            .map(|StructArg { param, value }| format!("{param}={value}"))
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.value,
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
    pub struct StructPatt<'src> {
        pub name: &'src str,
        pub args: Spanned<Vec<Spanned<Patt<'src>>>>,
    }

    impl SpanlessPartialEq for StructPatt<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.name == other.name && self.args.spanless_eq(&other.args)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Patt<'src> {
        Var(&'src str),
        Lit(Value<'src>),
        Struct(StructPatt<'src>),
    }

    impl<'src> fmt::Display for Patt<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Patt::Var(ident) => write!(f, "{ident}"),
                Patt::Lit(value) => write!(f, "{value}"),
                Patt::Struct(StructPatt { name, args }) => {
                    write!(
                        f,
                        "{}({})",
                        name,
                        args.value
                            .iter()
                            .map(|p| p.value.to_string())
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
                (Patt::Struct(l), Patt::Struct(r)) => l.spanless_eq(r),
                _ => false,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Op<'src> {
        pub lhs: Box<Spanned<Expr<'src>>>,
        pub op: Box<Spanned<Expr<'src>>>,
        pub rhs: Box<Spanned<Expr<'src>>>,
    }

    impl SpanlessPartialEq for Op<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            self.lhs.spanless_eq(&other.lhs)
                && self.op.spanless_eq(&other.op)
                && self.rhs.spanless_eq(&other.rhs)
        }
    }

    #[allow(unpredictable_function_pointer_comparisons)] // builtin contains function pointers
    #[derive(Debug, Clone, PartialEq)]
    pub enum Expr<'src> {
        Err,
        Lit(Value<'src>),
        Var(&'src str),
        Call(Call<Self>),
        Fn(FnExpr<'src, Rc<Spanned<Self>>>),
        Case(Case<'src, Self>),
        Op(Op<'src>),
        Let(Let<'src, Self>),
        LetRec(LetRec<'src, Self, Rc<Spanned<Self>>>),
        Builtin(fn(&Spanned<Expr<'src>>, &Env<'src>) -> Result<Value<'src>, Error>),
    }

    impl<'src> fmt::Display for Expr<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Expr::Err => write!(f, "<error>"),
                Expr::Lit(value) => write!(f, "{value}"),
                Expr::Var(ident) => write!(f, "{ident}"),
                Expr::Call(Call { callee, args }) => write!(
                    f,
                    "{}({})",
                    callee.value,
                    args.value
                        .iter()
                        .map(|e| e.value.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                Expr::Fn(FnExpr { params, body }) => {
                    write!(
                        f,
                        "fn({}) {{{}}}",
                        params
                            .value
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.value,
                    )
                }
                Expr::Case(Case { scrutinee, arms }) => {
                    write!(
                        f,
                        "case {} {{ {} }}",
                        scrutinee.value,
                        arms.value
                            .iter()
                            .map(|CaseArm { patt, body }| format!(
                                "{} => {}",
                                patt.value, body.value
                            ))
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                Expr::Op(Op { lhs, op, rhs }) => {
                    write!(f, "({} {} {})", lhs.value, op.value, rhs.value)
                }
                Expr::Let(Let {
                    fixity,
                    name,
                    init,
                    body,
                }) => {
                    fmt_let(f, *fixity, name, &init.value)?;
                    write!(f, " {}", body.value)
                }
                Expr::LetRec(LetRec { bindings, body }) => {
                    write!(f, "let {{")?;
                    for binding in bindings {
                        fmt_fixity(f, binding.fixity)?;
                        write!(
                            f,
                            " {} = fn({}) {{ {} }}",
                            binding.name.value,
                            binding.params.value.join(", "),
                            binding.body.value,
                        )?;
                    }
                    write!(f, " }} {}", body.value)
                }
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
                (Expr::Call(l), Expr::Call(r)) => l.spanless_eq(r),
                (Expr::Fn(l), Expr::Fn(r)) => l.spanless_eq(r),
                (Expr::Case(l), Expr::Case(r)) => l.spanless_eq(r),
                (Expr::Op(l), Expr::Op(r)) => l.spanless_eq(r),
                (Expr::Let(l), Expr::Let(r)) => l.spanless_eq(r),
                (Expr::LetRec(l), Expr::LetRec(r)) => l.spanless_eq(r),
                _ => false,
            }
        }
    }
}

mod interp {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Error {
        pub msg: String,
        pub span: Span,
    }

    pub type Env<'src> = rpds::HashTrieMap<&'src str, Rc<Value<'src>>>;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Binding<'src> {
        pub name: &'src str,
        pub value: Value<'src>,
    }

    pub type Bindings<'src> = Vec<Binding<'src>>;

    #[derive(Debug, Clone, PartialEq)]
    pub struct StructArg<'src> {
        pub param: &'src str,
        pub value: Rc<Value<'src>>,
    }

    pub type StructArgs<'src> = Rc<[StructArg<'src>]>;
}
