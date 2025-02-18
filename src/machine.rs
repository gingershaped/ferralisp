use std::rc::Rc;

use itertools::{EitherOrBoth, Itertools};
use thiserror::Error;

use crate::{list::List, scope::GlobalScope, value::Value};

#[derive(Error, Debug)]
pub enum Error {
    #[error("attempt to look up an undefined name {0}")]
    UndefinedName(String),
    #[error("cannot call value {0} because it is not a list or a builtin")]
    UncallableValue(Value),
    #[error("list {value} is not a correctly structured function or macro because: {reason}")]
    MalformedFunction {
        value: List<Rc<Value>>,
        reason: &'static str,
    },
    #[error("missing argument {argument} while calling {call_target}", argument = match .expected_type {
        Some(expected_type) => format!("{} of type {}", .name, expected_type),
        None => .name.to_string(),
    })]
    MissingArgument {
        name: String,
        call_target: Value,
        expected_type: Option<String>,
    },
    #[error("extra arguments [{arguments}] supplied while calling {call_target}", arguments = .arguments.into_iter().join(", "))]
    ExtraArguments {
        call_target: Value,
        arguments: Vec<Value>,
    },
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

enum ArgumentNames {
    NAdic(Vec<String>),
    Variadic(String),
}

struct CallInformation {
    argument_names: ArgumentNames,
    arguments: Vec<Rc<Value>>,
    body: Rc<Value>,
}

pub struct Machine {
    pub scope: GlobalScope,
}

impl Machine {
    pub fn new() -> Self {
        Machine {
            scope: GlobalScope::new(),
        }
    }

    fn evaluate_args(&mut self, arguments: Vec<Rc<Value>>) -> Result<Vec<Rc<Value>>, Error> {
        arguments
            .into_iter()
            .map(|value| self.eval(value))
            .collect::<Result<_, _>>()
    }

    fn call_information(
        &mut self,
        target: &List<Rc<Value>>,
        arguments: List<&Rc<Value>>,
    ) -> Result<CallInformation, Error> {
        let (raw_argument_names, body, is_macro) = match target.len() {
            2 => (target[0].clone(), target[1].clone(), false),
            3 => (target[1].clone(), target[2].clone(), true),
            _ => {
                return Err(Error::MalformedFunction {
                    value: target.clone(),
                    reason: "it is not exactly 2 or 3 items long",
                })
            }
        };
        let argument_names = match raw_argument_names.as_ref() {
            Value::Name(name) => ArgumentNames::Variadic(name.to_owned()),
            Value::List(names) => ArgumentNames::NAdic(
                names
                    .into_iter()
                    .map(|value| {
                        if let Value::Name(name) = value.as_ref() {
                            Ok(name.to_owned())
                        } else {
                            Err(Error::MalformedFunction {
                                value: target.clone(),
                                reason: "an item in the name list is not a name",
                            })
                        }
                    })
                    .collect::<Result<_, _>>()?,
            ),
            _ => {
                return Err(Error::MalformedFunction {
                    value: target.clone(),
                    reason: "the name list is not a list",
                })
            }
        };
        let mut arguments: Vec<_> = arguments.into_iter().cloned().collect();
        if is_macro {
            arguments = self.evaluate_args(arguments)?;
        }
        Ok(CallInformation {
            argument_names,
            arguments,
            body,
        })
    }

    fn call(&mut self, function: &List<Rc<Value>>, raw_args: List<&Rc<Value>>) -> ValueResult {
        let mut scope = self.scope.local();
        let mut call_info = self.call_information(function, raw_args)?;
        let mut head: Option<Rc<Value>>;
        let mut body = call_info.body;
        let mut function = function;

        loop {
            match call_info.argument_names {
                ArgumentNames::NAdic(ref names) => {
                    for (index, pair) in names.iter().zip_longest(&call_info.arguments).enumerate()
                    {
                        match pair {
                            EitherOrBoth::Both(name, value) => {
                                scope.insert(name.to_string(), (*value).clone());
                            }
                            EitherOrBoth::Left(name) => {
                                return Err(Error::MissingArgument {
                                    call_target: Value::List(function.clone()),
                                    name: name.to_string(),
                                    expected_type: None,
                                });
                            }
                            EitherOrBoth::Right(_) => {
                                return Err(Error::ExtraArguments {
                                    call_target: Value::List(function.clone()),
                                    arguments: call_info
                                        .arguments
                                        .split_off(index)
                                        .iter()
                                        .map(|v| v.as_ref().clone())
                                        .collect(),
                                });
                            }
                        }
                    }
                }
                ArgumentNames::Variadic(ref name) => {
                    scope.insert(
                        name.to_string(),
                        Rc::new(Value::List(
                            (&call_info.arguments)
                                .iter()
                                .cloned()
                                .collect::<Vec<_>>()
                                .into(),
                        )),
                    );
                }
            }

            // tail-call elimination
            // first flatten builtins which are evaluated during TCE
            head = None;
            while let Value::List(body_contents) = body.as_ref() {
                if let Some((body_head, body_tail)) = body_contents.divide() {
                    head = Some(self.eval(body_head.clone())?);
                    if let Some(Value::Builtin(builtin)) = head.as_deref() {
                        if builtin.eval_during_tce {
                            body = (builtin.body)(body_tail.into_iter().cloned().collect(), self)?;
                            continue;
                        }
                    }
                }
                break;
            }

            if let Some(Value::List(head_contents)) = head.as_deref() {
                function = head_contents;
                if let Value::List(body) = body.as_ref() {
                    if let Some((_, raw_args)) = body.divide() {
                        call_info = self.call_information(head_contents, raw_args)?;
                        scope.clear();
                        continue;
                    }
                }
                return Err(Error::UncallableValue(body.as_ref().clone()));
            } else {
                return self.eval(body);
            }
        }
    }

    pub fn eval(&mut self, value: Rc<Value>) -> ValueResult {
        match value.as_ref() {
            Value::List(contents) => {
                match contents.divide() {
                    // nil evaluates to itself
                    None => Ok(value),
                    // otherwise it's a function call
                    Some((target, args)) => match target.as_ref() {
                        Value::List(function) => self.call(function, args),
                        Value::Builtin(builtin) => {
                            let mut args = args.into_iter().cloned().collect();
                            if builtin.is_macro {
                                args = self.evaluate_args(args)?;
                            }
                            (builtin.body)(args, self)
                        }
                        other => Err(Error::UncallableValue(other.clone())),
                    },
                }
            }
            Value::Builtin(_) => Ok(value),
            Value::Integer(_) => Ok(value),
            Value::Name(name) => self.scope.lookup(name),
        }
    }
}
