use crate::expr::{Expr, LiteralValue};
use crate::stmt::Stmt;
use crate::token::{Literal, Token};
use crate::token_type::TokenType;
use anyhow::{Context, Result};

pub struct Interpreter;
impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn execute(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                let _ = evaluate(expr); // discard result
            }
            Stmt::Print(expr) => match evaluate(expr) {
                Ok(value) => println!("{}", value),
                Err(_) => eprintln!("Runtime error during print."),
            },
        }
    }
}
pub fn evaluate(expr: &Expr) -> Result<LiteralValue> {
    match expr {
        Expr::Error => anyhow::bail!("Attempted to evaluate error node"),
        Expr::Literal(v) => Ok(v.clone()),
        Expr::Grouping(inner) => Ok(evaluate(inner)?),
        Expr::Assign(token, expr) => todo!(),
        Expr::Variable(token) => todo!(),
        Expr::Unary(op_token, right) => {
            let right_val = evaluate(right)?;
            match op_token.token_type {
                TokenType::Minus => {
                    if let LiteralValue::Number(n) = right_val {
                        Ok(LiteralValue::Number(-n))
                    } else {
                        panic!("Unary Minus applied to non-number")
                    }
                }
                TokenType::Bang => Ok(LiteralValue::Bool(!is_truthy(&right_val))),
                _ => panic!("Unknown Unary Operator"),
            }
        }
        Expr::Binary(left, op_token, right) => {
            let left_val = evaluate(left)?;
            let right_val = evaluate(right)?;
            match op_token.token_type {
                TokenType::Plus => match (left_val, right_val) {
                    (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                        Ok(LiteralValue::Number(a + b))
                    }
                    (LiteralValue::String(a), LiteralValue::String(b)) => {
                        Ok(LiteralValue::String(a + &b))
                    }
                    _ => panic!("Operands must be numbers or strings"),
                },
                TokenType::Minus => binary_numeric_op(left_val, right_val, |a, b| a - b),
                TokenType::Star => binary_numeric_op(left_val, right_val, |a, b| a * b),
                TokenType::Slash => binary_numeric_op(left_val, right_val, |a, b| a / b),
                TokenType::EqualEqual => Ok(LiteralValue::Bool(left_val == right_val)),
                TokenType::BangEqual => Ok(LiteralValue::Bool(left_val != right_val)),
                TokenType::Greater => binary_numeric_cmp(left_val, right_val, |a, b| a > b),
                TokenType::GreaterEqual => binary_numeric_cmp(left_val, right_val, |a, b| a >= b),
                TokenType::Less => binary_numeric_cmp(left_val, right_val, |a, b| a < b),
                TokenType::LessEqual => binary_numeric_cmp(left_val, right_val, |a, b| a <= b),
                _ => panic!("unknown op {}", op_token),
            }
        }
        Expr::Comma(left, right) => {
            let _ = evaluate(left).context("while evaluating left side of comma expression")?;
            let result =
                evaluate(right).context("while evaluating right side of comma expression")?;
            Ok(result)
        }
        Expr::Ternary(condition, true_left, false_right) => {
            let condition = evaluate(condition)?;
            match condition.is_truthy() {
                true => evaluate(true_left),
                false => evaluate(false_right),
            }
        }
    }
}

fn is_truthy(value: &LiteralValue) -> bool {
    !matches!(value, LiteralValue::Bool(false) | LiteralValue::Nil)
}
fn binary_numeric_op<F>(left: LiteralValue, right: LiteralValue, op: F) -> Result<LiteralValue>
where
    F: FnOnce(f64, f64) -> f64,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => Ok(LiteralValue::Number(op(a, b))),
        _ => panic!("operands must be numbers"),
    }
}

fn binary_numeric_cmp<F>(left: LiteralValue, right: LiteralValue, cmp: F) -> Result<LiteralValue>
where
    F: FnOnce(f64, f64) -> bool,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => Ok(LiteralValue::Bool(cmp(a, b))),
        _ => panic!("Operands must be numbers for comparison"),
    }
}
