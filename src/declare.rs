pub use std::fmt;

use chumsky::prelude::*;

pub use fixity::*;
pub use interp::*;
pub use lex::*;
pub use parse::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[allow(dead_code)] // trait only used by tests
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

    pub type LetHead<'src> = (Option<(OpAssoc, i64)>, &'src str);

    #[derive(Debug)]
    pub enum ReplItem<'src> {
        Expr(Spanned<SyntaxExpr<'src>>),
        Let(Option<(OpAssoc, i64)>, &'src str, Spanned<SyntaxExpr<'src>>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum SyntaxExpr<'src> {
        Atom(Expr<'src>),
        Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
        Fn(Spanned<Vec<&'src str>>, Box<Spanned<Self>>),
        Case(
            Box<Spanned<Self>>,
            Spanned<Vec<(Spanned<Patt<'src>>, Box<Spanned<Self>>)>>,
        ),
        Ops(Box<Spanned<Self>>, Vec<(Spanned<Self>, Spanned<Self>)>),
        Let(
            Option<(OpAssoc, i64)>,
            &'src str,
            Box<Spanned<Self>>,
            Box<Spanned<Self>>,
        ),
    }

    impl<'src> fmt::Display for SyntaxExpr<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                SyntaxExpr::Atom(expr) => write!(f, "{expr}"),
                SyntaxExpr::Call(func, args) => write!(
                    f,
                    "{}({})",
                    func.0,
                    args.0
                        .iter()
                        .map(|e| e.0.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                SyntaxExpr::Fn(params, body) => {
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
                SyntaxExpr::Case(expr, arms) => {
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
                SyntaxExpr::Ops(left, ops) => {
                    let mut result = left.0.to_string();
                    for (op, right) in ops {
                        result.push_str(&format!(" {} {}", op.0, right.0));
                    }
                    write!(f, "({result})")
                }
                SyntaxExpr::Let(op_spec, ident, assign, body) => {
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
            }
        }
    }

    impl SpanlessPartialEq for SyntaxExpr<'_> {
        fn spanless_eq(&self, other: &Self) -> bool {
            match (self, other) {
                (SyntaxExpr::Atom(l), SyntaxExpr::Atom(r)) => l.spanless_eq(r),
                (SyntaxExpr::Call(l_func, l_args), SyntaxExpr::Call(r_func, r_args)) => {
                    l_func.spanless_eq(r_func) && l_args.spanless_eq(r_args)
                }
                (SyntaxExpr::Fn(l_params, l_body), SyntaxExpr::Fn(r_params, r_body)) => {
                    l_params.spanless_eq(r_params) && l_body.spanless_eq(r_body)
                }
                (SyntaxExpr::Case(l_expr, l_arms), SyntaxExpr::Case(r_expr, r_arms)) => {
                    l_expr.spanless_eq(r_expr)
                        && l_arms.0.len() == r_arms.0.len()
                        && l_arms.0.iter().zip(&r_arms.0).all(
                            |((l_patt, l_body), (r_patt, r_body))| {
                                l_patt.spanless_eq(r_patt) && l_body.spanless_eq(r_body)
                            },
                        )
                }
                (SyntaxExpr::Ops(l_first, l_rest), SyntaxExpr::Ops(r_first, r_rest)) => {
                    l_first.spanless_eq(r_first)
                        && l_rest.len() == r_rest.len()
                        && l_rest
                            .iter()
                            .zip(r_rest)
                            .all(|((l_op, l_rhs), (r_op, r_rhs))| {
                                l_op.spanless_eq(r_op) && l_rhs.spanless_eq(r_rhs)
                            })
                }
                (
                    SyntaxExpr::Let(l_spec, l_name, l_assign, l_body),
                    SyntaxExpr::Let(r_spec, r_name, r_assign, r_body),
                ) => {
                    l_spec == r_spec
                        && l_name == r_name
                        && l_assign.spanless_eq(r_assign)
                        && l_body.spanless_eq(r_body)
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
}

mod fixity {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Value<'src> {
        Unit,
        Float(f64),
        Int(i64),
        Closure(
            Option<&'src str>, // Display and struct name
            Option<&'src str>, // Recursive name from the definition
            Vec<&'src str>,
            Box<Spanned<Expr<'src>>>,
            Env<'src>,
        ),
        Struct(
            Option<&'src str>,
            Box<Spanned<Expr<'src>>>,
            StructArgs<'src>,
            Env<'src>,
        ),
    }

    impl<'src> fmt::Display for Value<'src> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Value::Unit => write!(f, "unit"),
                Value::Float(value) => write!(f, "{value}"),
                Value::Int(value) => write!(f, "{value}"),
                Value::Closure(name, rec_name, params, body, _) => {
                    write!(
                        f,
                        "fn{}{}({}) {{ {} }}",
                        match name {
                            Some(n) => format!(" {n}"),
                            None => String::new(),
                        },
                        match rec_name {
                            Some(n) if n != &name.unwrap_or("") => format!("`{n}`"),
                            _ => String::new(),
                        },
                        params
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        body.0,
                    )
                }
                Value::Struct(name, expr, args, _) => {
                    write!(
                        f,
                        "{}({}) {{ {} }}",
                        name.unwrap_or(""),
                        args.iter()
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
                Patt::Struct(name, args) => {
                    write!(
                        f,
                        "{}({})",
                        name,
                        args.0
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
                (Patt::Struct(l_name, l_args), Patt::Struct(r_name, r_args)) => {
                    l_name == r_name && l_args.spanless_eq(r_args)
                }
                _ => false,
            }
        }
    }

    #[allow(unpredictable_function_pointer_comparisons)] // builtin contains function pointers
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
                    l_left.spanless_eq(r_left)
                        && l_op.spanless_eq(r_op)
                        && l_right.spanless_eq(r_right)
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
                _ => false,
            }
        }
    }
}

mod interp {
    use std::rc::Rc;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Error {
        pub msg: String,
        pub span: Span,
    }

    pub type Env<'src> = rpds::HashTrieMap<&'src str, Rc<Value<'src>>>;
    pub type Bindings<'src> = Vec<(&'src str, Value<'src>)>;
    pub type StructArgs<'src> = Rc<[(&'src str, Rc<Value<'src>>)]>;
}
