use std::{cell::RefCell, collections::{HashMap, VecDeque}, fmt::Display, rc::Rc};

use strum_macros::IntoStaticStr;
use thiserror::Error;

use crate::builtins::Builtin;

#[derive(Clone, Debug, IntoStaticStr)]
pub enum Value<'a> {
    List(Rc<RefCell<VecDeque<Value<'a>>>>),
    Builtin(&'a Builtin),
    Integer(i64),
    Name(&'a str),
}

impl<'a> Value<'a> {
    fn nil() -> Value<'a> {
        Value::List(Rc::new(RefCell::new(VecDeque::new())))
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(values) => {
                let values = values.borrow();
                write!(f, "[")?;
                for (index, value) in values.iter().enumerate() {
                    value.fmt(f)?;
                    if index != values.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            },
            Value::Builtin(builtin) => {
                write!(f, "<builtin {} \"{}\">", if builtin.is_macro { "macro" } else { "function" }, builtin.name)
            },
            Value::Integer(value) => write!(f, "{}", value),
            Value::Name(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Error)]
pub enum Error<'a> {
    #[error("attempt to look up an undefined name {0}")]
    UndefinedName(&'a str),
    #[error("attempt to call an uncallable value {0}")]
    UncallableValue(Value<'a>),
    #[error("missing argument {name} of type {expected_type}")]
    MissingArgument {
        name: &'static str,
        expected_type: &'static str,
    },
    #[error("extra arguments supplied")]
    ExtraArguments(Vec<Value<'a>>),
    #[error("wrong argument type passed to argument {name}: expected a {expected_type} but got a value {value}")]
    WrongBuiltinArgumentType {
        name: &'static str,
        expected_type: &'static str,
        value: Value<'a>,
    },
    #[error("builtin execution failed")]
    BuiltinExecutionFailed(#[from] Box<dyn std::error::Error>),
}

type ValueResult<'a> = Result<Value<'a>, Error<'a>>;
type Scope<'a> = HashMap<&'a str, Value<'a>>;

pub struct Machine<'a> {
    pub globals: Scope<'a>,
    pub locals: Vec<Scope<'a>>,
}

impl<'a> Machine<'a> {
    fn eval(&mut self, value: Value<'a>) -> ValueResult<'a> {
        match value {
            Value::List(contents) => {
                let mut contents = contents.borrow_mut();
                if contents.is_empty() {
                    // nil evaluates to itself
                    Ok(Value::nil())
                } else {
                    // otherwise it's a function call
                    let args = contents.split_off(1);
                    let target = contents.pop_front().unwrap();
                    match target {
                        Value::List(values) => todo!(),
                        Value::Builtin(builtin) => {
                            (builtin.body)(args, self)
                        },
                        _ => Err(Error::UncallableValue(target))
                    }
                }
            },
            Value::Builtin(_) => Ok(value),
            Value::Integer(_) => Ok(value),
            Value::Name(name) => {
                self.locals.last()
                    .and_then(|scope| scope.get(name))
                    .or_else(|| self.globals.get(name))
                    .ok_or_else(|| Error::UndefinedName(name))
                    .map(|value| value.clone())
            },
        }
    }

    fn call(value: Value<'a>) {

    }
}