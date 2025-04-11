use crate::{
    error::{RuntimeError, RuntimeResult},
    expr::LiteralValue,
    token::Token,
};
use log::debug;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default)]
pub struct Environment {
    values: HashMap<String, LiteralValue>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}
impl Environment {
    pub fn new_enclosing_env(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn define(&mut self, name: String, value: LiteralValue) {
        debug!("inserting : '{}' into env with vaule : {}", name, value);
        self.values.insert(name, value);
    }
    pub fn get(env: Rc<RefCell<Environment>>, name: &Token) -> RuntimeResult<LiteralValue> {
        let mut current = Some(env);
        while let Some(env_rc) = current {
            let env_ref = env_rc.borrow();
            if let Some(value) = env_ref.values.get(&name.lexeme) {
                return Ok(value.clone());
            }
            current = env_ref.enclosing.clone();
        }
        Err(RuntimeError::new(&format!(
            "Undefined variable '{}'.",
            name.lexeme
        )))
    }

    pub fn assign(
        env: Rc<RefCell<Environment>>,
        name: &Token,
        value: LiteralValue,
    ) -> RuntimeResult<()> {
        let mut current = Some(env);
        while let Some(env_rc) = current {
            let mut env_ref = env_rc.borrow_mut();
            if env_ref.values.contains_key(&name.lexeme) {
                env_ref.values.insert(name.lexeme.clone(), value);
                return Ok(());
            }
            current = env_ref.enclosing.clone();
        }
        Err(RuntimeError::new(&format!(
            "Undefined variable '{}'.",
            name.lexeme
        )))
    }
}
