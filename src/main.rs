use std::{error::Error, fs::read_to_string, path::PathBuf, rc::Rc};

use clap::{Args, Parser};
use ferralisp::{machine::Machine, parser::parse};

#[derive(Debug, Parser)]
#[command(name = "ferralisp")]
struct Cli {
    #[command(flatten)]
    source: Source,

    #[arg(long)]
    parsetree: bool,
}

#[derive(Debug, Args)]
#[group(required = true, multiple = false)]
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

fn main() -> Result<(), Box<dyn Error>> {
    let args = Cli::parse();
    let source = args.source.read();
    
    let parsed = parse(&source)?;
    if args.parsetree {
        println!("{:?}", parse(&source));
        return Ok(());
    }

    let mut machine = Machine::new();
    for expression in parsed {
        println!("{:?}", machine.eval(Rc::new(expression.into()))?);
    }

    Ok(())
}
