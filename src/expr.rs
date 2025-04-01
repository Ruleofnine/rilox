use std::fmt;
use crate::{token::Token, token_type::TokenType};
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralValue),
    Unary(Token, Box<Expr>),
}


#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
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
fn evaluate(expr: &Expr) -> LiteralValue {
    match expr {
        Expr::Literal(v) => v.clone(),
        Expr::Grouping(inner) => evaluate(inner),
        Expr::Unary(op_token, right) => {
            let right_val = evaluate(right);
            match op_token.token_type {
                TokenType::Minus => {
                    if let LiteralValue::Number(n) = right_val {
                        LiteralValue::Number(-n)
                    } else {
                        panic!("Unary Minus applied to non-number")
                    }
                }
                TokenType::Bang => LiteralValue::Bool(!is_truthy(&right_val)),
                _ => panic!("Unknown Unary Operator"),
            }
        }
        Expr::Binary(left, op_token, right) => {
            let left_val = evaluate(left);
            let right_val = evaluate(right);
            match op_token.token_type {
                TokenType::Plus => match (left_val, right_val) {
                    (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                        LiteralValue::Number(a + b)
                    }
                    (LiteralValue::String(a), LiteralValue::String(b)) => {
                        LiteralValue::String(a + &b)
                    }
                    _ => panic!("Operands must be numbers or strings"),
                },
                TokenType::Minus => binary_numeric_op(left_val, right_val, |a, b| a - b),
                TokenType::Star => binary_numeric_op(left_val, right_val, |a, b| a * b),
                TokenType::Slash => binary_numeric_op(left_val, right_val, |a, b| a / b),
                TokenType::EqualEqual => LiteralValue::Bool(left_val == right_val),
                TokenType::BangEqual => LiteralValue::Bool(left_val != right_val),
                TokenType::Greater => binary_numeric_cmp(left_val, right_val, |a, b| a > b),
                TokenType::GreaterEqual => binary_numeric_cmp(left_val, right_val, |a, b| a >= b),
                TokenType::Less => binary_numeric_cmp(left_val, right_val, |a, b| a < b),
                TokenType::LessEqual => binary_numeric_cmp(left_val, right_val, |a, b| a <= b),
                _ => todo!("other binary ops"),
            }
        }
    }
}

fn is_truthy(value: &LiteralValue) -> bool {
    match value {
        LiteralValue::Bool(false) | LiteralValue::Nil => false,
        _ => true,
    }
}
fn binary_numeric_op<F>(left: LiteralValue, right: LiteralValue, op: F) -> LiteralValue
where
    F: FnOnce(f64, f64) -> f64,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => LiteralValue::Number(op(a, b)),
        _ => panic!("operands must be numbers"),
    }
}

fn binary_numeric_cmp<F>(left: LiteralValue, right: LiteralValue, cmp: F) -> LiteralValue
where
    F: FnOnce(f64, f64) -> bool,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => LiteralValue::Bool(cmp(a, b)),
        _ => panic!("Operands must be numbers for comparison"),
    }
}
