use crate::expr::Expr;
use crate::token::Token;
#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>),
    If {
        condition: Expr,
        if_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While(Expr, Box<Stmt>),
    Break,
    Continue,
}
#[derive(PartialEq, Debug)]
pub enum ControlFlow {
    None,
    Break,
    Continue,
    Return(Expr),
}
