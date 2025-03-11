//! the machine, or the thing that actually executes tinylisp code.

use std::{borrow::Cow, cell::RefCell, collections::HashSet, env::current_dir, fmt::Debug, rc::Rc};

use itertools::{EitherOrBoth, Itertools};
use lasso::Rodeo;
use refpool::{Pool, PoolRef};
use thiserror::Error;
use tracing::{error, instrument, trace};

use crate::{
    loaders::{FileLoader, StdlibLoader},
    parser::Expression,
    scope::GlobalScope,
    util::or_fallback,
    value::{HashlessMiniSpur, List, Value},
};

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("attempt to look up an undefined name {name}", name = or_fallback(.0))]
    UndefinedName(Option<String>),
    #[error("name {name} may not be redefined to {new_value}, it is already defined as {current_value}", name = or_fallback(name))]
    DefinedName {
        name: Option<String>,
        current_value: Value,
        new_value: Value,
    },
    #[error("cannot call value {0} because it is not a list or a builtin")]
    UncallableValue(Value),
    #[error("list {value} is not a correctly structured function or macro because: {reason}")]
    MalformedFunction { value: Value, reason: &'static str },
    #[error("missing argument {argument} while calling {call_target}", argument = match .expected_type {
        Some(expected_type) => format!("{} of type {}", or_fallback(.name), expected_type),
        None => or_fallback(.name).to_owned(),
    })]
    MissingArgument {
        name: Option<String>,
        call_target: Value,
        expected_type: Option<String>,
    },
    #[error("extra arguments [{arguments}] supplied while calling {call_target}", arguments = .arguments.iter().join(", "))]
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
    #[error("unable to find module named {module}, tried loaders: {loaders}", loaders = .loaders.iter().join(", "))]
    ModuleNotFound {
        module: String,
        loaders: Vec<&'static str>,
    },
    #[error("{0}")]
    ModuleLoadError(#[from] ModuleLoadError),
    #[error("an error occured while evaluating module {module}: {source}")]
    ModuleEvaluationError { module: String, source: Box<Error> },
}

#[derive(Error, Debug)]
#[error("loader {loader} failed to load module {module}: {source}")]
pub struct ModuleLoadError {
    loader: &'static str,
    module: String,
    source: Box<dyn std::error::Error>,
}

impl PartialEq for ModuleLoadError {
    fn eq(&self, other: &Self) -> bool {
        self.loader == other.loader && self.module == other.module
    }
}

pub type ValueResult = Result<Value, Error>;

pub trait World {
    fn disp(&self, value: &Value);
}

impl Debug for dyn World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<world {:p}>", self)
    }
}

#[derive(Debug)]
pub enum LoadResult {
    NotFound,
    Ok { values: Vec<Value>, cache: bool },
    Err(Box<dyn std::error::Error>),
}

pub trait Loader {
    fn name(&self) -> &'static str;
    fn load(&self, path: &str, machine: &Machine, loads: &[ModuleLoad]) -> LoadResult;
}

impl Debug for dyn Loader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<loader {}>", self.name())
    }
}

#[derive(Debug, PartialEq)]
pub struct ModuleLoad {
    pub loader: &'static str,
    pub path: String,
}

#[derive(Debug, PartialEq)]
enum ArgumentNames {
    NAdic(Vec<HashlessMiniSpur>),
    Variadic(HashlessMiniSpur),
}

#[derive(Debug, PartialEq)]
struct CallInformation<'a> {
    argument_names: ArgumentNames,
    arguments: Cow<'a, List>,
    body: &'a Value,
}

pub type Interner = RefCell<Rodeo<HashlessMiniSpur>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
#[repr(u8)]
pub enum OptimizationLevel {
    /// Normal optimizations are applied, maintaining compatibility with the reference interpreter
    Normal,
    /// Allows ferralisp to perform arbitrary transformations to your program,
    /// possibly breaking compatibility with the reference interpreter
    Dangerous
}

impl Default for OptimizationLevel {
    fn default() -> Self {
        OptimizationLevel::Normal
    }
}


#[derive(Debug)]
pub struct Machine {
    pub scope: GlobalScope,
    pub(crate) world: Box<dyn World>,
    pub pool: Pool<List>,
    pub optimizations: OptimizationLevel,
    loaders: Vec<Box<dyn Loader>>,
    loaded_modules: HashSet<String>,
    module_origins: Vec<ModuleLoad>,
    interner: Rc<Interner>,
}

impl Machine {
    const POOL_SIZE: usize = 2_usize.pow(16);

    pub fn new(world: impl World + 'static, loaders: Vec<Box<dyn Loader>>, optimizations: OptimizationLevel) -> Self {
        let interner = Rc::new(RefCell::new(Rodeo::new()));
        Machine {
            scope: GlobalScope::new(interner.clone()),
            world: Box::new(world),
            pool: Pool::new(Self::POOL_SIZE),
            optimizations,
            loaders,
            loaded_modules: HashSet::new(),
            module_origins: vec![],
            interner,
        }
    }

    pub fn with_default_loaders(world: impl World + 'static, optimizations: OptimizationLevel) -> Self {
        Self::new(
            world,
            vec![
                Box::new(FileLoader::new(
                    current_dir().expect("current directory is invalid"),
                )),
                Box::new(StdlibLoader),
            ],
            optimizations
        )
    }

    /// Converts an Expression (AST) into a Value containing machine-specific context.
    pub fn hydrate(&self, expression: Expression) -> Value {
        // conversion process:
        // - integers are just copied
        // - names are interned, unless the name refers to a builtin
        //   and the optimization level is Dangerous, in which case the builtin is inlined
        // - lists are converted to linked lists and recursively hydrated
        match expression {
            Expression::Integer(value) => Value::Integer(value),
            Expression::Name(name) => {
                let name = self.create_name(name);
                if self.optimizations >= OptimizationLevel::Dangerous {
                    let Value::Name((spur, _)) = name else { unreachable!() };
                    match self.scope.global_lookup(&spur) {
                        Some(Value::Builtin(builtin)) => Value::Builtin(*builtin),
                        _ => name
                    }
                } else {
                    name
                }
            },
            Expression::List(expressions) => Value::List(PoolRef::new(
                &self.pool,
                List::from_iter(
                    &self.pool,
                    expressions
                        .into_iter()
                        .map(|expression| self.hydrate(expression)),
                ),
            )),
        }
    }

    /// Convert a string slice into a Value by interning it.
    pub fn create_name(&self, string: &str) -> Value {
        Value::Name((
            self.interner.borrow_mut().get_or_intern(string),
            Rc::downgrade(&self.interner),
        ))
    }

    /// Load a module from a path by running it through the machine's loaders.
    pub fn load(&mut self, path: String) -> ValueResult {
        if !self.loaded_modules.contains(&path) {
            for loader in &self.loaders {
                match (*loader).load(&path, self, &self.module_origins) {
                    LoadResult::NotFound => (),
                    LoadResult::Ok { values, cache } => {
                        self.module_origins.push(ModuleLoad {
                            loader: loader.name(),
                            path,
                        });
                        for value in values {
                            self.eval(&value).map_err(|source| {
                                let path = self
                                    .module_origins
                                    .pop()
                                    .expect("module origin stack underflow")
                                    .path;
                                Error::ModuleEvaluationError {
                                    module: path,
                                    source: Box::new(source),
                                }
                            })?;
                        }
                        let path = self
                            .module_origins
                            .pop()
                            .expect("module origin stack underflow")
                            .path;
                        if cache {
                            self.loaded_modules.insert(path);
                        }
                        return Ok(Value::nil(&self.pool));
                    }
                    LoadResult::Err(source) => {
                        return Err(Error::ModuleLoadError(ModuleLoadError {
                            loader: loader.name(),
                            module: path,
                            source,
                        }));
                    }
                }
            }
            return Err(Error::ModuleNotFound {
                module: path,
                loaders: self.loaders.iter().map(|loader| loader.name()).collect(),
            });
        }
        Ok(Value::nil(&self.pool))
    }

    fn evaluate_args(&mut self, arguments: &List) -> Result<List, Error> {
        List::try_from_iter(
            // cloning the pool doesn't actually clone its data
            &self.pool.clone(),
            arguments.into_iter().map(|value| self.eval(value)),
        )
    }

    /// inspect the structure of a user-defined function to determine how to call it,
    /// and evaluate the arguments if it's a macro
    fn call_information<'a>(
        &mut self,
        target: &'a Value,
        raw_arguments: &'a List,
    ) -> Result<CallInformation<'a>, Error> {
        let mut arguments = Cow::Borrowed(raw_arguments);
        let Value::List(target) = target else {
            return Err(Error::MalformedFunction {
                value: target.clone(),
                reason: "it is not a list",
            });
        };
        let (raw_argument_names, body, is_macro) = match target.as_ref() {
            List::Cons(first, tail) => match tail.as_ref() {
                List::Cons(second, tail) => match tail.as_ref() {
                    List::Cons(third, tail) => match tail.as_ref() {
                        List::Cons(..) => {
                            return Err(Error::MalformedFunction {
                                value: Value::List(target.clone()),
                                reason: "it is not exactly 2 or 3 items long",
                            })
                        }
                        List::Nil => (second, third, true),
                    },
                    List::Nil => (first, second, false),
                },
                List::Nil => {
                    return Err(Error::MalformedFunction {
                        value: Value::List(target.clone()),
                        reason: "it is not exactly 2 or 3 items long",
                    })
                }
            },
            List::Nil => {
                return Err(Error::MalformedFunction {
                    value: Value::List(target.clone()),
                    reason: "it is not exactly 2 or 3 items long",
                })
            }
        };
        let argument_names = match raw_argument_names {
            Value::Name((name, _)) => ArgumentNames::Variadic(name.to_owned()),
            Value::List(names) => ArgumentNames::NAdic(
                names
                    .iter()
                    .map(|value| {
                        if let Value::Name((name, _)) = value {
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
        if !is_macro {
            arguments = Cow::Owned(self.evaluate_args(raw_arguments)?);
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
    fn call(&mut self, call_target: &Value, raw_args: &List) -> ValueResult {
        // all of this is mutable so TCE can update it
        let mut call_info = self.call_information(call_target, raw_args)?;
        let mut scope = self.scope.local();
        let mut head;
        let mut body;

        loop {
            trace!("current call information: {:?}", call_info);
            // bind argument values to their names in the local scope
            match call_info.argument_names {
                ArgumentNames::NAdic(ref names) => {
                    for (index, pair) in names
                        .iter()
                        .zip_longest(Cow::as_ref(&call_info.arguments))
                        .enumerate()
                    {
                        match pair {
                            EitherOrBoth::Both(name, value) => {
                                scope.insert(*name, (*value).clone());
                            }
                            // an argument is missing
                            EitherOrBoth::Left(name) => {
                                return Err(Error::MissingArgument {
                                    call_target: call_target.clone(),
                                    name: self
                                        .interner
                                        .borrow()
                                        .try_resolve(name)
                                        .map(|v| v.to_owned()),
                                    expected_type: None,
                                });
                            }
                            // an extra argument was supplied, split off the rest of the argument list
                            EitherOrBoth::Right(_) => {
                                return Err(Error::ExtraArguments {
                                    call_target: call_target.clone(),
                                    arguments: call_info
                                        .arguments
                                        .into_iter()
                                        .cloned()
                                        .collect::<Vec<_>>()
                                        .split_off(index)
                                        .to_vec(),
                                });
                            }
                        }
                    }
                }
                ArgumentNames::Variadic(name) => {
                    // summon a list from the ether to hold the arguments
                    scope.insert(
                        name,
                        Value::List(PoolRef::new(&self.pool, call_info.arguments.into_owned())),
                    );
                }
            }
            trace!("local scope: {}", scope);

            body = call_info.body.clone();
            while let Value::List(ref body_inner) = body {
                if let List::Cons(head, tail) = body_inner.as_ref() {
                    let head = self.eval(head)?;
                    if let Value::Builtin(builtin) = head {
                        if builtin.eval_during_tce {
                            body = (builtin.body)(tail, self, true)?;
                            continue;
                        }
                    }
                }
                break;
            }

            if let Value::List(ref body) = body {
                if let List::Cons(raw_head, tail) = body.as_ref() {
                    head = self.eval(raw_head)?;
                    if matches!(head, Value::List(_)) {
                        call_info = self.call_information(&head, tail.as_ref())?;
                        scope.clear();
                        continue;
                    }
                }
            }

            return self.eval(&body);
        }
    }

    /// evaluate a value as described in the tinylisp spec
    #[instrument(skip(self), ret)]
    pub fn eval(&mut self, value: &Value) -> ValueResult {
        match value {
            Value::List(ref contents) => {
                match contents.as_ref() {
                    // nil evaluates to itself
                    List::Nil => Ok(value.clone()),
                    // otherwise it's a function call
                    List::Cons(target, args) => {
                        let mut args = Cow::Borrowed(args.as_ref());
                        trace!("attempting to invoke {} with raw_args {:?}", target, &args);
                        let target = self.eval(target)?;
                        match target {
                            Value::List(_) => self.call(&target, args.as_ref()),
                            Value::Builtin(builtin) => {
                                if !builtin.is_macro {
                                    args = Cow::Owned(self.evaluate_args(&args)?);
                                }
                                trace!(
                                    "invoking builtin {} with args {}",
                                    builtin.name,
                                    args.iter().join(", ")
                                );
                                (builtin.body)(&args, self, false)
                            }
                            other => {
                                error!("uncallable value {}", other);
                                Err(Error::UncallableValue(other.clone()))
                            }
                        }
                    }
                }
            }
            Value::Builtin(v) => Ok(Value::Builtin(*v)),
            Value::Integer(v) => Ok(Value::Integer(*v)),
            Value::Name((name, _)) => self.scope.lookup(name),
        }
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use crate::{
        machine::{ArgumentNames, CallInformation},
        parse_list, parse_value,
        util::dummy_machine,
        value::{List, Value},
    };

    #[test]
    fn call_info() {
        let mut machine = dummy_machine();

        let nadic_function = parse_value!(machine, "((args) args)");
        let variadic_function = parse_value!(machine, "(args args)");
        let nadic_macro = parse_value!(machine, "(() (args) args)");
        let variadic_macro = parse_value!(machine, "(() args args)");
        let args = parse_list!(machine, "((q 42))");

        let args_name = machine.interner.borrow_mut().get_or_intern_static("args");
        let q_name = machine.interner.borrow_mut().get_or_intern_static("q");

        assert_eq!(
            machine.call_information(&nadic_function, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::NAdic(vec![args_name]),
                arguments: Cow::Owned(List::from_iter(&machine.pool, vec![42.into()])),
                body: &args_name.into(),
            })
        );
        assert_eq!(
            machine.call_information(&variadic_function, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::Variadic(args_name),
                arguments: Cow::Owned(List::from_iter(&machine.pool, vec![42.into()])),
                body: &args_name.into(),
            })
        );
        assert_eq!(
            machine.call_information(&nadic_macro, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::NAdic(vec![args_name]),
                arguments: Cow::Owned(List::from_iter(
                    &machine.pool,
                    vec![Value::from_iter(
                        &machine.pool,
                        vec![q_name.into(), 42.into()]
                    )]
                )),
                body: &args_name.into(),
            })
        );
        assert_eq!(
            machine.call_information(&variadic_macro, &args),
            Ok(CallInformation {
                argument_names: ArgumentNames::Variadic(args_name),
                arguments: Cow::Owned(List::from_iter(
                    &machine.pool,
                    vec![Value::from_iter(
                        &machine.pool,
                        vec![q_name.into(), 42.into()].into_iter()
                    )]
                )),
                body: &args_name.into(),
            })
        );
    }
}
