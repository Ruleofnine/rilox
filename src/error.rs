use std::error::Error;
use std::fmt;
pub trait ErrorReporter {
    fn report(&mut self, line: usize, message: &str);
    fn had_error(&self) -> bool;
}
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    source: Option<Box<RuntimeError>>,
}

impl RuntimeError {
    pub fn new(message: &str) -> Self {
        RuntimeError {
            message: message.to_string(),
            source: None,
        }
    }
    pub fn with_source(message: &str, source: RuntimeError) -> RuntimeError {
        RuntimeError {
            message: message.to_string(),
            source: Some(Box::new(source)),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.message)?;
        if let Some(source) = &self.source {
            write!(f, "Caused by: {}", source)?;
        }
        Ok(())
    }
}

impl Error for RuntimeError {}
