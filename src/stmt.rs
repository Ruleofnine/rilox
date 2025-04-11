use crate::expr::Expr;
use crate::token::Token;
#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>),
    Mutation {
        name: Token,
        operator: Token,
        value: Expr,
    },
    If {
        condition: Expr,
        if_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While(Expr, Box<Stmt>),
}
