//! the machine, or the thing that actually executes tinylisp code.

use std::rc::Rc;

use itertools::{EitherOrBoth, Itertools};
use thiserror::Error;
use tracing::{error, instrument, trace};

use crate::{scope::GlobalScope, value::Value};

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("attempt to look up an undefined name {0}")]
    UndefinedName(String),
    #[error("name {0} may not be redefined, it is already defined as value {1}")]
    DefinedName(String, Value),
    #[error("cannot call value {0} because it is not a list or a builtin")]
    UncallableValue(Value),
    #[error("list {value} is not a correctly structured function or macro because: {reason}")]
    MalformedFunction { value: Value, reason: &'static str },
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
    #[error("wrong argument type passed to argument {name} of function {alias}: expected a {expected_type} but got a value {value}")]
    WrongBuiltinArgumentType {
        alias: &'static str,
        name: &'static str,
        expected_type: &'static str,
        value: Value,
    },
    #[error("builtin error: {0}")]
    BuiltinError(String),
}

pub type ValueResult = Result<Rc<Value>, Error>;

#[derive(Debug, PartialEq)]
enum ArgumentNames {
    NAdic(Vec<String>),
    Variadic(String),
}

#[derive(Debug, PartialEq)]
struct CallInformation {
    argument_names: ArgumentNames,
    arguments: Vec<Rc<Value>>,
    body: Rc<Value>,
}

#[derive(Debug)]
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

    /// inspect the structure of a user-defined function to determine how to call it,
    /// and evaluate the arguments if it's a macro
    fn call_information(
        &mut self,
        target: &Rc<Value>,
        arguments: &[Rc<Value>],
    ) -> Result<CallInformation, Error> {
        let Value::List(target) = target.as_ref() else {
            return Err(Error::MalformedFunction {
                value: target.as_ref().clone(),
                reason: "it is not a list",
            });
        };
        let (raw_argument_names, body, is_macro) = match target.len() {
            2 => (target[0].clone(), target[1].clone(), false),
            3 => (target[1].clone(), target[2].clone(), true),
            _ => {
                return Err(Error::MalformedFunction {
                    value: Value::List(target.clone()),
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
                                value: Value::List(target.clone()),
                                reason: "an item in the name list is not a name",
                            })
                        }
                    })
                    .collect::<Result<_, _>>()?,
            ),
            _ => {
                return Err(Error::MalformedFunction {
                    value: Value::List(target.clone()),
                    reason: "the name list is not a list",
                })
            }
        };
        let mut arguments: Vec<_> = arguments.into_iter().cloned().collect();
        if !is_macro {
            arguments = self.evaluate_args(arguments)?;
        }
        Ok(CallInformation {
            argument_names,
            arguments,
            body,
        })
    }

    /// call a user-defined "function" (i.e. a list with the correct structure).
    ///
    /// functions which recursively call themselves as their last operation (tail-recursive functions)
    /// will be optimized into a loop, allowing them to recurse infinitely without overflowing the Rust
    /// call stack. certain builtins (those marked as `tce` in `builtins.rs`) may also be used
    /// without disabling this optimization.
    #[instrument(ret)]
    fn call(&mut self, call_target: &Rc<Value>, raw_args: &Vec<Rc<Value>>) -> ValueResult {
        // all of this is mutable so TCE can update it
        let mut call_info = self.call_information(call_target, raw_args)?;
        let mut scope = self.scope.local();

        loop {
            trace!("current call information: {:?}", call_info);
            // bind argument values to their names in the local scope
            match call_info.argument_names {
                ArgumentNames::NAdic(ref names) => {
                    for (index, pair) in names.iter().zip_longest(&call_info.arguments).enumerate()
                    {
                        match pair {
                            EitherOrBoth::Both(name, value) => {
                                scope.insert(name.to_string(), (*value).clone());
                            }
                            // an argument is missing
                            EitherOrBoth::Left(name) => {
                                return Err(Error::MissingArgument {
                                    call_target: call_target.as_ref().clone(),
                                    name: name.to_string(),
                                    expected_type: None,
                                });
                            }
                            // an extra argument was supplied, split off the rest of the argument list
                            EitherOrBoth::Right(_) => {
                                return Err(Error::ExtraArguments {
                                    call_target: call_target.as_ref().clone(),
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
                    // summon a list from the ether to hold the arguments
                    scope.insert(
                        name.to_string(),
                        Rc::new(Value::List(
                            (&call_info.arguments).iter().cloned().collect(),
                        )),
                    );
                }
            }
            trace!("local scope: {}", scope);

            let mut body = call_info.body.clone();
            while let Value::List(body_inner) = body.as_ref() {
                if let Some((head, tail)) = body_inner.split_first() {
                    let head = self.eval(head.clone())?;
                    if let Value::Builtin(builtin) = head.as_ref() {
                        if builtin.eval_during_tce {
                            body = (builtin.body)(tail.to_vec(), self, true)?;
                            continue;
                        }
                    }
                }
                break;
            }

            if let Value::List(body) = body.as_ref() {
                if let Some((head, tail)) = body.split_first() {
                    let head = self.eval(head.clone())?;
                    if matches!(head.as_ref(), Value::List(_)) {
                        call_info = self.call_information(&head, tail)?;
                        scope.clear();
                        continue;
                    }
                }
            }

            return self.eval(body);

            // let mut call_target = None;
            // let mut function_body = call_info.body.clone();
            // trace!("initial function body: {}", &function_body);
            // while let Value::List(function_inner) = function_body.as_ref() {
            //     if let Some((head, raw_args)) = function_inner.split_first() {
            //         call_target = Some(self.eval(head.clone())?);
            //         trace!("new call target: {}", call_target.as_ref().unwrap());
            //         // SAFETY: we just set it to Some two lines earlier
            //         if let Value::Builtin(builtin) = unsafe { call_target.as_ref().unwrap_unchecked() }.as_ref() {
            //             if builtin.eval_during_tce {
            //                 trace!("evaluating builtin {} in TCE mode", &builtin.name);
            //                 function_body = (builtin.body)(raw_args.to_vec(), self, true)?;
            //                 trace!("builtin returned new function body: {}", &function_body);
            //                 continue;
            //             }
            //         }
            //     }
            //     break;
            // }

            // // is function_body a user-defined function or macro?
            // if let Some(Value::List(call_target_inner)) = call_target.as_deref() {
            //     if call_target_inner.len() != 0 {
            //         // if so, update call_info and go back to the start
            //         trace!("TCE recursing to new function: {}", &call_target.as_ref().unwrap());
            //         call_info = self.call_information(call_target_inner, raw_args)?;
            //         scope.clear();
            //         continue;
            //     }
            // }
            // // if not, we're done recursing, so eval and return it
            // trace!("final return value: {}", &function_body);
            // return self.eval(function_body);
        }
    }

    /// evaluate a value as described in the tinylisp spec
    #[instrument(skip(self), ret)]
    pub fn eval(&mut self, value: Rc<Value>) -> ValueResult {
        match value.as_ref() {
            Value::List(contents) => {
                match contents.split_first() {
                    // nil evaluates to itself
                    None => Ok(value),
                    // otherwise it's a function call
                    Some((target, args)) => {
                        let mut args: Vec<Rc<Value>> = args.iter().cloned().collect();
                        trace!("attempting to invoke {} with raw_args {:?}", target, &args);
                        let target = self.eval(target.clone())?;
                        match target.as_ref() {
                            Value::List(_) => self.call(&target, &args),
                            Value::Builtin(builtin) => {
                                if !builtin.is_macro {
                                    args = self.evaluate_args(args)?;
                                }
                                trace!(
                                    "invoking builtin {} with args {}",
                                    builtin.name,
                                    args.iter().join(", ")
                                );
                                (builtin.body)(args, self, false)
                            }
                            other => {
                                error!("uncallable value {}", other);
                                Err(Error::UncallableValue(other.clone()))
                            }
                        }
                    }
                }
            }
            // no need to increment the refcount here
            Value::Builtin(_) => Ok(value),
            Value::Integer(_) => Ok(value),
            Value::Name(name) => self.scope.lookup(name),
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::{
        machine::{ArgumentNames, CallInformation},
        parser::parse_expression,
        value::Value,
    };

    use super::Machine;

    macro_rules! list {
        ($code:literal) => {
            if let Value::List(list) = parse_expression($code).unwrap().into() {
                list
            } else {
                unreachable!()
            }
        };
    }

    #[test]
    fn call_info() {
        let mut machine = Machine::new();

        let nadic_function = Rc::new(parse_expression("((args) args)").unwrap().into());
        let variadic_function = Rc::new(parse_expression("(args args)").unwrap().into());
        let nadic_macro = Rc::new(parse_expression("(() (args) args)").unwrap().into());
        let variadic_macro = Rc::new(parse_expression("(() args args)").unwrap().into());
        let args = list!("((q 42))");

        assert_eq!(
            machine.call_information(&nadic_function, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::NAdic(vec!["args".to_string()]),
                arguments: vec![Value::of(42)],
                body: Value::of("args")
            })
        );
        assert_eq!(
            machine.call_information(&variadic_function, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::Variadic("args".to_string()),
                arguments: vec![Value::of(42)],
                body: Value::of("args")
            })
        );
        assert_eq!(
            machine.call_information(&nadic_macro, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::NAdic(vec!["args".to_string()]),
                arguments: vec![vec![Value::of("q"), Value::of(42)]
                    .into_iter()
                    .collect::<Value>()
                    .into()],
                body: Value::of("args")
            })
        );
        assert_eq!(
            machine.call_information(&variadic_macro, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::Variadic("args".to_string()),
                arguments: vec![vec![Value::of("q"), Value::of(42)]
                    .into_iter()
                    .collect::<Value>()
                    .into()],
                body: Value::of("args")
            })
        );
    }
}
