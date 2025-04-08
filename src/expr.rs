use crate::{token::Token, token_type::TokenType};
use anyhow::{Context, Result};
use std::fmt;
#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralValue),
    Unary(Token, Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    Variable(Token),
    Assign(Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    Error,
}
impl LiteralValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LiteralValue::Bool(b) => *b,
            LiteralValue::Nil => false,
            LiteralValue::Number(n) => *n != 0.0,
            LiteralValue::String(s) => !s.is_empty(),
            LiteralValue::Error => false,
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
            LiteralValue::Error => write!(f, "<error>"),
        }
    }
}
