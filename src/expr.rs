use crate::token::Token;
use std::fmt;
#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(LiteralValue),
    Unary(Token, Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    Variable(Token),
    Assign(Token, Box<Expr>),
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Mutation {
        name: Token,
        operator: Token,
        value: Box<Expr>,
    },
    Ternary {
        condition: Box<Expr>,
        true_branch: Box<Expr>,
        false_branch: Box<Expr>,
    },
    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}
impl LiteralValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LiteralValue::Bool(b) => *b,
            LiteralValue::Nil => false,
            LiteralValue::Number(n) => *n != 0.0,
            LiteralValue::String(s) => !s.is_empty(),
        }
    }
}
impl std::fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Number(n) => write!(f, "{}", n),
            LiteralValue::String(s) => write!(f, "\"{}\"", s),
            LiteralValue::Bool(b) => write!(f, "{}", b),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}
