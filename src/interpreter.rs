use std::cell::RefCell;

use crate::environment::Environment;
use crate::error::{RuntimeError, RuntimeResult};
use crate::expr::{Expr, LiteralValue};
use crate::stmt::Stmt;
use crate::token_type::TokenType;
use std::rc::Rc;

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn statement(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate(expr).map_err(|e| {
                    RuntimeError::with_source("while evaluating expression statement", e)
                })?;
            }
            Stmt::Print(expr) => {
                let value = self.evaluate(expr).map_err(|e| {
                    RuntimeError::with_source("while evaluating print expression", e)
                })?;
                println!("{}", value);
            }
            Stmt::Var(name, value) => {
                let value = if let Some(expr) = value {
                    self.evaluate(expr)?
                } else {
                    LiteralValue::Nil
                };
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.to_string(), value);
            }
            Stmt::Block(stmt_list) => {
                let new_env = Rc::new(RefCell::new(Environment::new_enclosing_env(
                    self.environment.clone(),
                )));
                self.execute_block(stmt_list, new_env)?;
            }
            Stmt::If {
                condition,
                if_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate(condition)?;
                match cond_value.is_truthy() {
                    true => self.statement(if_branch)?,
                    false => {
                        if let Some(else_branch) = else_branch {
                            self.statement(else_branch)?
                        }
                    }
                }
            }
            Stmt::While(cond, statement) => {
                while self.evaluate(cond)?.is_truthy() {
                    self.statement(statement)?;
                }
            }
        }
        Ok(())
    }
    fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let previous_env = self.environment.clone();
        self.environment = environment;
        for stmt in stmts {
            self.statement(stmt)?;
        }
        self.environment = previous_env;
        Ok(())
    }
    fn evaluate_with_context(&mut self, expr: &Expr, context: &str) -> RuntimeResult<LiteralValue> {
        self.evaluate(expr)
            .map_err(|e| RuntimeError::with_source(context, e))
    }
    pub fn evaluate(&mut self, expr: &Expr) -> Result<LiteralValue, RuntimeError> {
        match expr {
            Expr::Error => Err(RuntimeError::new("Cannot evaluate invalid expression")),
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Grouping(inner) => Ok(self.evaluate(inner)?),
            Expr::Assign(name, expr) => {
                let expr = self.evaluate(expr)?;
                Environment::assign(self.environment.clone(), name, expr.clone())?;
                Ok(expr)
            }
            Expr::Mutation {
                name,
                operator,
                value,
            } => {
                let value = self.evaluate(value)?;
                let var_value = Environment::get(self.environment.clone(), name)?;
                let (var_num, value_num) = match (var_value, value) {
                    (LiteralValue::Number(a), LiteralValue::Number(b)) => (a, b),
                    other => {
                        return Err(RuntimeError::new(&format!(
                            "Expected to add or subtract two literal values found '{:?}'",
                            other,
                        )));
                    }
                };

                let result = match operator.token_type {
                    TokenType::PlusEqual | TokenType::PlusPlus => {
                        LiteralValue::Number(var_num + value_num)
                    }
                    TokenType::MinusEqual | TokenType::MinusMinus => {
                        LiteralValue::Number(var_num - value_num)
                    }
                    _ => {
                        return Err(RuntimeError::new(&format!(
                            "Expected '+'|'-'|'++'|'--' found '{}'",
                            operator.token_type
                        )));
                    }
                };
                Environment::assign(self.environment.clone(), name, result.clone())?;
                Ok(result)
            }
            Expr::Variable(name) => Environment::get(self.environment.clone(), name),
            Expr::Unary(op_token, right) => {
                let right_val = self.evaluate(right)?;
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
                let left_val = self
                    .evaluate_with_context(left, "Evaluting left hand side of Binary Expression")?;
                let right_val = self.evaluate_with_context(
                    right,
                    "Evaluting right hand side of Binary Expression",
                )?;
                match op_token.token_type {
                    TokenType::Plus | TokenType::PlusPlus => match (left_val, right_val) {
                        (LiteralValue::Number(a), LiteralValue::Number(b)) => {
                            Ok(LiteralValue::Number(a + b))
                        }
                        (LiteralValue::String(a), LiteralValue::String(b)) => {
                            Ok(LiteralValue::String(a + &b))
                        }
                        _ => Err(RuntimeError::new("Cannot add tokens")),
                    },
                    TokenType::Minus | TokenType::MinusMinus => {
                        binary_numeric_op(left_val, right_val, |a, b| a - b)
                    }
                    TokenType::Star => binary_numeric_op(left_val, right_val, |a, b| a * b),
                    TokenType::Slash => {
                        if right_val == LiteralValue::Number(0 as f64) {
                            return Err(RuntimeError::new("Divide by 0 error"));
                        }
                        binary_numeric_op(left_val, right_val, |a, b| a / b)
                    }
                    TokenType::EqualEqual => Ok(LiteralValue::Bool(left_val == right_val)),
                    TokenType::BangEqual => Ok(LiteralValue::Bool(left_val != right_val)),
                    TokenType::Greater => binary_numeric_cmp(left_val, right_val, |a, b| a > b),
                    TokenType::GreaterEqual => {
                        binary_numeric_cmp(left_val, right_val, |a, b| a >= b)
                    }
                    TokenType::Less => binary_numeric_cmp(left_val, right_val, |a, b| a < b),
                    TokenType::LessEqual => binary_numeric_cmp(left_val, right_val, |a, b| a <= b),
                    TokenType::Caret => binary_numeric_op(left_val, right_val, |a, b| a.powf(b)),
                    TokenType::Percent => binary_numeric_op(left_val, right_val, |a, b| a % b),
                    unknown => Err(RuntimeError::new(&format!(
                        "Unknown Binary Operator: {}",
                        unknown
                    ))),
                }
            }
            Expr::Comma(left, right) => {
                let _ = self.evaluate(left);
                let result = self.evaluate(right)?;
                Ok(result)
            }
            Expr::Ternary {
                condition,
                true_branch,
                false_branch,
            } => {
                let condition = self.evaluate(condition)?;
                match condition.is_truthy() {
                    true => self.evaluate(true_branch),
                    false => self.evaluate(false_branch),
                }
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
