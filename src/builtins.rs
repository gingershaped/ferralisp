use std::{collections::{HashMap, VecDeque}, error::Error, sync::LazyLock};

use crate::machine::{Machine, Value};


#[derive(Debug)]
pub enum BuiltinError<'a> {
    MissingArgument {
        name: &'static str,
        expected_type: &'static str,
    },
    ExtraArguments(Vec<Value<'a>>),
    WrongArgumentType {
        name: &'static str,
        expected_type: &'static str,
        value: Value<'a>,
    },
    ExecutionFailed(Box<dyn Error>),
}

#[derive(Debug)]
pub struct Builtin {
    pub is_macro: bool,
    pub body: for <'a> fn(Vec<Value<'a>>, Machine<'a>) -> Result<Value<'a>, BuiltinError<'a>>,
}

macro_rules! builtin {
    (
        fn $name:ident($($arg:tt)*) $body:block
    ) => {
        builtin_inner!($name, false, $($arg)*, $body)
    };
    (
        macro $name:ident($($arg:tt)*) $body:block
    ) => {
        builtin_inner!($name, true, $($arg)*, $body)
    };
}

macro_rules! builtin_inner {
    ($name:ident, $is_macro:literal, $($argname:ident: $argtype:tt),*, $body:block) => {
        (stringify!($name), Builtin {
            is_macro: $is_macro,
            #[allow(unused_variables)]
            body: |args, machine| {
                let mut args = VecDeque::from(args);
                $(builtin_argument!(args, $argname: $argtype);)*
                if (!args.is_empty()) {
                    Err(BuiltinError::ExtraArguments(args.into()))
                } else {
                    $body
                }
            }
        })
    }
}

macro_rules! builtin_argument {
    ($args:ident, $name:ident: any) => {
        #[allow(unused_mut)]
        let mut $name = $args.pop_front().ok_or_else(|| BuiltinError::MissingArgument {
            name: stringify!($name),
            expected_type: "any",
        })?;
    };
    ($args:ident, $name:ident: $argtype:path) => {
        use Value::*;
        #[allow(unused_mut)]
        let mut $name = match $args.pop_front() {
            Some(arg) => match arg {
                $argtype(arg) => Ok(arg),
                other => Err(BuiltinError::WrongArgumentType {
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other,
                }),
            },
            None => Err(BuiltinError::MissingArgument {
                name: stringify!($name),
                expected_type: stringify!($argtype),
            }),
        }?;
    };
}

pub static BUILTINS: LazyLock<HashMap<&'static str, Builtin>> = LazyLock::new(|| HashMap::from([
    builtin! {
        fn c(value: any, list: List) {
            list.push_front(value);
            Ok(Value::List(list))
        }
    },
    builtin! {
        macro q(thing: any) {
            Ok(thing)
        }
    },
]));