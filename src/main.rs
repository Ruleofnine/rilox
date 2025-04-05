use ansi_term::Color::Red;
use anyhow::Result;
use log::LevelFilter;
use rilox::lox::Lox;
use std::env::args;

fn main() -> Result<()> {
    let mut log_level = LevelFilter::Warn;
    let mut source_file = None;
    for arg in args().skip(1) {
        match arg.as_str() {
            "-d" | "--debug" => log_level = LevelFilter::Debug,
            "-i" | "--info" => log_level = LevelFilter::Info,
            "-h" | "--help" => {
                print_help();
                return Ok(());
            }
            _ => {
                if source_file.is_none() {
                    source_file = Some(arg);
                } else {
                    eprintln!(
                        "{}",
                        Red.paint(format!("Warning: Ignoring Extra argument: '{}'", arg))
                    )
                }
            }
        }
    }
    env_logger::builder().filter_level(log_level).init();
    let mut lox = Lox::new();
    match source_file {
        Some(path) => lox.run_file(path),
        None => lox.run_prompt(),
    }?;
    Ok(())
}
fn print_help() {
    println!(
        "Usage: rilox [script] [flags]\nUse -i/-d or --info --debug for log level adjustments\n-h or --help for this help message\n Use no script to run prompt"
    )
}
