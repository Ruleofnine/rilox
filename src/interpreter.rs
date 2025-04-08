use crate::error::{RuntimeError, RuntimeResult};
use crate::expr::{Expr, LiteralValue};
use crate::stmt::Stmt;
use crate::token_type::TokenType;

pub struct Interpreter;
impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression(expr) => {
                evaluate(expr)?; // discard result
            }
            Stmt::Print(expr) => {
                let value = evaluate(expr).map_err(|e| {
                    RuntimeError::with_source("while evaluating print expression", e)
                })?;
                println!("{}", value);
            }
        }
        Ok(())
    }
}
pub fn evaluate(expr: &Expr) -> Result<LiteralValue, RuntimeError> {
    match expr {
        Expr::Error => Err(RuntimeError::new("Cannot evaluate invalid expression")),
        Expr::Literal(v) => Ok(v.clone()),
        Expr::Grouping(inner) => Ok(evaluate(inner)?),
        Expr::Assign(_token, _expr) => todo!(),
        Expr::Variable(_token) => todo!(),
        Expr::Unary(op_token, right) => {
            let right_val = evaluate(right)?;
            match op_token.token_type {
                TokenType::Minus => {
                    if let LiteralValue::Number(n) = right_val {
                        Ok(LiteralValue::Number(-n))
                    } else {
                        Err(RuntimeError::new("Unary Minus applied to non-number"))
                    }
                }
                TokenType::Bang => Ok(LiteralValue::Bool(!right_val.is_truthy())),
                _ => Err(RuntimeError::new("Unknown Unary Operator")),
            }
        }
        Expr::Binary(left, op_token, right) => {
            let left_val =
                evaluate_with_context(left, "Evaluting left hand side of Binary Expression")?;
            let right_val =
                evaluate_with_context(right, "Evaluting right hand side of Binary Expression")?;
            match op_token.token_type {
                TokenType::Plus => match (left_val, right_val) {
                    (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                        Ok(LiteralValue::Number(a + b))
                    }
                    (LiteralValue::String(a), LiteralValue::String(b)) => {
                        Ok(LiteralValue::String(a + &b))
                    }
                    _ => Err(RuntimeError::new("Cannot add tokens")),
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
                TokenType::Caret => binary_numeric_op(left_val, right_val, |a, b| a.powf(b)),
                _ => Err(RuntimeError::new("Unknown Binary Operator")),
            }
        }
        Expr::Comma(left, right) => {
            let _ = evaluate(left);
            let result = evaluate(right)?;
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

fn binary_numeric_op<F>(
    left: LiteralValue,
    right: LiteralValue,
    op: F,
) -> RuntimeResult<LiteralValue>
where
    F: FnOnce(f64, f64) -> f64,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => Ok(LiteralValue::Number(op(a, b))),
        _ => Err(RuntimeError::new("Operands must be numbers")),
    }
}

fn binary_numeric_cmp<F>(
    left: LiteralValue,
    right: LiteralValue,
    cmp: F,
) -> RuntimeResult<LiteralValue>
where
    F: FnOnce(f64, f64) -> bool,
{
    match (left, right) {
        (LiteralValue::Number(a), LiteralValue::Number(b)) => Ok(LiteralValue::Bool(cmp(a, b))),
        _ => Err(RuntimeError::new("Operands must be numbers for comparison")),
    }
}

fn evaluate_with_context(expr: &Expr, context: &str) -> RuntimeResult<LiteralValue> {
    evaluate(expr).map_err(|e| RuntimeError::with_source(context, e))
}
