pub trait ErrorReporter {
    fn report(&mut self, line: usize, message: &str);
    fn had_error(&self) -> bool;
}
