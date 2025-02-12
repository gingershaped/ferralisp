use std::{collections::{HashMap, VecDeque}, sync::LazyLock};

use crate::machine::{Machine, Value, Error};


#[derive(Debug)]
pub struct Builtin {
    pub name: &'static str,
    pub is_macro: bool,
    pub body: for <'a> fn(VecDeque<Value<'a>>, &mut Machine<'a>) -> Result<Value<'a>, Error<'a>>,
}

macro_rules! builtin {
    (
        fn $name:ident($($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, false, $($arg)*, $body)
    };
    (
        macro $name:ident($($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, true, $($arg)*, $body)
    };
}

macro_rules! builtin_inner {
    ($alias:ident, $name:ident, $is_macro:literal, $($argname:ident: $argtype:tt),*, $body:block) => {
        (stringify!($alias), Builtin {
            name: stringify!($name),
            is_macro: $is_macro,
            #[allow(unused_variables)]
            body: |mut args, machine| {
                $(builtin_argument!(args, $argname: $argtype);)*
                if !args.is_empty() {
                    Err(Error::ExtraArguments(args.into()))
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
        let mut $name = $args.pop_front().ok_or_else(|| Error::MissingArgument {
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
                other => Err(Error::WrongBuiltinArgumentType {
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other,
                }),
            },
            None => Err(Error::MissingArgument {
                name: stringify!($name),
                expected_type: stringify!($argtype),
            }),
        }?;
    };
}

pub static BUILTINS: LazyLock<HashMap<&'static str, Builtin>> = LazyLock::new(|| HashMap::from([
    builtin! {
        fn cons(value: any, list: List) as c {
            list.borrow_mut().push_front(value);
            Ok(Value::List(list))
        }
    },
    builtin! {
        macro quote(thing: any) as q {
            Ok(thing)
        }
    },
]));