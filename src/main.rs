use std::{error::Error, fs::read_to_string, path::PathBuf, rc::Rc};

use anstream::println;
use clap::{Args, Parser};
use ferralisp::{
    machine::{Machine, World},
    parser::{parse, parse_expression},
    value::Value,
};
use owo_colors::OwoColorize;
use rustyline::{error::ReadlineError, DefaultEditor};

#[derive(Debug, Parser)]
#[command(name = "ferralisp")]
struct Cli {
    #[command(flatten)]
    source: Option<Source>,

    #[arg(long)]
    parsetree: bool,
}

#[derive(Debug, Args)]
#[group(multiple = false)]
struct Source {
    #[arg(short = 'c', long = "code")]
    code: Option<String>,
    file: Option<PathBuf>,
}

impl Source {
    fn read(self) -> String {
        self.code.unwrap_or_else(|| {
            read_to_string(self.file.expect("neither code nor file is set"))
                .expect("failed to read file")
        })
    }
}

struct ConsoleWorld;
impl World for ConsoleWorld {
    fn disp(&self, value: &Value) {
        println!("{}", value);
    }
}

fn repl() -> Result<(), Box<dyn Error>> {
    let mut machine = Machine::with_default_loaders(ConsoleWorld);
    let mut rl = DefaultEditor::new()?;

    println!("(welcome to {})", "ferralisp".purple());
    loop {
        match rl.readline("tl> ") {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                match parse_expression(&line) {
                    Ok(expression) => match machine.eval(Rc::new(expression.into())) {
                        Ok(value) => {
                            println!("{}", value);
                        }
                        Err(error) => {
                            println!("{}: {}", "error!!".bold().red(), error);
                        }
                    },
                    Err(error) => {
                        println!("{}: {}", "parse error!!".bold().red(), error);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(_) => (),
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();

    match args.source {
        Some(source) => {
            let source = source.read();

            let parsed = parse(&source)?;
            if args.parsetree {
                println!("{:?}", parse(&source));
                return Ok(());
            }

            let mut machine = Machine::with_default_loaders(ConsoleWorld);
            for expression in parsed {
                match machine.eval(Rc::new(expression.into())) {
                    Ok(value) => println!("{}", value),
                    Err(err) => {
                        println!("error!! {}", err);
                        break;
                    }
                }
            }

            Ok(())
        }
        None => repl(),
    }
}
