use std::{error::Error, fs::read_to_string, path::PathBuf};

use anstream::println;
use clap::{Args, Parser};
use ferralisp::{
    machine::{Machine, OptimizationLevel, World},
    parser::{parse, parse_expression},
    value::Value,
};
use owo_colors::OwoColorize;
use rustyline::{error::ReadlineError, DefaultEditor};

#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[derive(Debug, Parser)]
#[command(name = "ferralisp")]
struct Cli {
    #[command(flatten)]
    source: Option<Source>,

    /// Instead of evaluating, output a parse tree.
    #[arg(long)]
    parsetree: bool,

    /// Configure the optimization level.
    #[arg(long = "opt", default_value_t, value_enum)]
    optimizations: OptimizationLevel,
}

#[derive(Debug, Args)]
#[group(multiple = false)]
struct Source {
    #[arg(short = 'c', long = "code")]
    /// Instead of using a file, execute code from the command line.
    code: Option<String>,
    /// The file to read the program from.
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
    fn disp(&self, machine: &Machine, value: &Value) {
        println!("{}", value.contextualize(machine));
    }
}

fn repl(args: Cli) -> Result<(), Box<dyn Error>> {
    let mut machine = Machine::with_default_loaders(ConsoleWorld, args.optimizations);
    let mut rl = DefaultEditor::new()?;

    println!("(welcome to {})", "ferralisp".purple());
    loop {
        match rl.readline("tl> ") {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                match parse_expression(&line) {
                    Ok(expression) => match machine.eval(&machine.hydrate(expression)) {
                        Ok(value) => {
                            println!("{}", value.contextualize(&machine));
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

            let mut machine = Machine::with_default_loaders(ConsoleWorld, args.optimizations);
            for expression in parsed {
                match machine.eval(&machine.hydrate(expression)) {
                    Ok(value) => println!("{}", value.contextualize(&machine)),
                    Err(err) => {
                        println!("error!! {}", err);
                        break;
                    }
                }
            }

            Ok(())
        }
        None => repl(args),
    }
}
