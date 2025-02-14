use std::{collections::HashMap, ops::Deref, rc::Rc};

use thiserror::Error;

use crate::{builtins::BUILTINS, value::Value};

#[derive(Debug, Error)]
pub enum Error {
    #[error("attempt to look up an undefined name {0}")]
    UndefinedName(String),
    #[error("attempt to call an uncallable value {0}")]
    UncallableValue(Value),
    #[error("missing argument {name} of type {expected_type}")]
    MissingArgument {
        name: &'static str,
        expected_type: &'static str,
    },
    #[error("extra arguments supplied")]
    ExtraArguments(Vec<Value>),
    #[error("wrong argument type passed to argument {name}: expected a {expected_type} but got a value {value}")]
    WrongBuiltinArgumentType {
        name: &'static str,
        expected_type: &'static str,
        value: Value,
    },
    #[error("builtin execution failed")]
    BuiltinExecutionFailed(#[from] Box<dyn std::error::Error>),
}

pub type ValueResult = Result<Rc<Value>, Error>;
type Scope = HashMap<String, Rc<Value>>;

pub struct Machine {
    globals: Scope,
    locals: Vec<Scope>,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            globals: HashMap::from_iter(
                BUILTINS.iter().map(|(k, v)| ((*k).into(), Rc::new(Value::Builtin(*v))))
            ),
            locals: vec![],
        }
    }

    pub fn eval(&mut self, value: Rc<Value>) -> ValueResult {
        match Rc::deref(&value) {
            Value::List(contents) => {
                match contents.divide() {
                    // nil evaluates to itself
                    None => Ok(value),
                    // otherwise it's a function call
                    Some((target, args)) => {
                        match Rc::deref(&target) {
                            Value::List(list) => todo!(),
                            Value::Builtin(builtin) => {
                                let mut args: Vec<Rc<Value>> = args.into_iter().cloned().collect();
                                if builtin.is_macro {
                                    args = args.into_iter().map(|v| self.eval(v)).collect::<Result<_, _>>()?;
                                }
                                (builtin.body)(args, self)
                            },
                            other => Err(Error::UncallableValue(other.clone())),
                        }
                    }
                }
            },
            Value::Builtin(_) => Ok(value),
            Value::Integer(_) => Ok(value),
            Value::Name(name) => {
                self.locals.last()
                    .and_then(|scope| scope.get(name))
                    .or_else(|| self.globals.get(name))
                    .ok_or_else(|| Error::UndefinedName(name.to_string()))
                    .cloned()
            },
        }
    }
}