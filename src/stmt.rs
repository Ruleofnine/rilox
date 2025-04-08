use crate::expr::Expr;
#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}
