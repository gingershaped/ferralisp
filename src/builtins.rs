//! all the tinylisp builtins. these will be injected into the global namespace
//! of newly created machines.

use std::{collections::HashMap, rc::Rc, sync::LazyLock};

use crate::{
    machine::{Error, Machine, ValueResult},
    value::Value,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Builtin {
    pub name: &'static str,
    pub is_macro: bool,
    pub eval_during_tce: bool,
    pub body: fn(Vec<Rc<Value>>, &mut Machine) -> ValueResult,
}

macro_rules! builtin {
    (
        fn $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, false, false, $machine, ($($arg)*), $body)
    };
    (
        macro $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, true, false, $machine, ($($arg)*), $body)
    };
    (
        tce fn $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, false, true, $machine, ($($arg)*), $body)
    };
    (
        tce macro $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, true, true, $machine, ($($arg)*), $body)
    };
}

macro_rules! builtin_inner {
    ($alias:ident, $name:ident, $is_macro:literal, $eval_during_tce:literal, $machine:ident, ($($argname:tt: $argtype:tt),*), $body:block) => {
        (stringify!($alias), Builtin {
            name: stringify!($name),
            is_macro: $is_macro,
            eval_during_tce: $eval_during_tce,
            #[allow(unused_variables)]
            body: |mut args, $machine| {
                args.reverse();
                $(builtin_argument!($alias, args, $machine, $argname: $argtype);)*
                if !args.is_empty() {
                    Err(Error::ExtraArguments {
                        call_target: Value::Builtin(BUILTINS[stringify!($alias)]),
                        arguments: args.into_iter().map(|value| value.as_ref().clone()).collect()
                    })
                } else {
                    $body
                }
            }
        })
    }
}

macro_rules! builtin_argument {
    ($alias:ident, $args:ident, $machine:ident, $name:ident: any) => {
        #[allow(unused_mut)]
        let mut $name = $args.pop().ok_or_else(|| Error::MissingArgument {
            name: stringify!($name).to_string(),
            call_target: Value::Builtin(BUILTINS[stringify!($alias)]),
            expected_type: Some("any".to_string()),
        })?;
    };
    ($alias:ident, $args:ident, $machine:ident, $name:ident: $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.pop();
        #[allow(unused_mut)]
        let mut $name = match arg {
            Some(ref arg) => match arg.as_ref() {
                $argtype(arg) => Ok(arg),
                other => Err(Error::WrongBuiltinArgumentType {
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: stringify!($name).to_owned(),
                call_target: Value::Builtin(BUILTINS[stringify!($alias)]),
                expected_type: Some(stringify!($argtype).to_owned()),
            }),
        }?;
    };
    ($alias:ident, $args:ident, $machine:ident, ($raw:ident -> $name:ident): $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.pop();
        #[allow(unused_mut)]
        let ($raw, mut $name) = match arg {
            Some(ref arg) => match arg.as_ref() {
                $argtype(value) => Ok((arg.clone(), value)),
                other => Err(Error::WrongBuiltinArgumentType {
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: stringify!($name).to_owned(),
                call_target: Value::Builtin(BUILTINS[stringify!($alias)]),
                expected_type: Some(stringify!($argtype).to_owned()),
            }),
        }?;
    };
}

pub static BUILTINS: LazyLock<HashMap<&'static str, Builtin>> = LazyLock::new(|| {
    HashMap::from([
        builtin! {
            fn cons(_machine, value: any, list: List) as c {
                Ok(Rc::new(Value::List(list.clone().cons(value))))
            }
        },
        builtin! {
            fn head(_machine, value: List) as h {
                Ok(value.head().cloned().unwrap_or(Value::nil()))
            }
        },
        builtin! {
            fn tail(_machine, value: List) as t {
                Ok(value.tail().map(|value| Rc::new(Value::List(value.into()))).unwrap_or(Value::nil()))
            }
        },
        builtin! {
            fn add(_machine, a: Integer, b: Integer) as a {
                Ok(Rc::new(Value::Integer(a + b)))
            }
        },
        builtin! {
            fn subtract(_machine, a: Integer, b: Integer) as s {
                Ok(Rc::new(Value::Integer(a - b)))
            }
        },
        builtin! {
            fn less_than(_machine, a: Integer, b: Integer) as l {
                Ok(Rc::new(Value::Integer((a < b).into())))
            }
        },
        builtin! {
            fn equal(_machine, a: any, b: any) as e {
                Ok(Rc::new(Value::Integer((a == b).into())))
            }
        },
        builtin! {
            tce fn eval(machine, value: any) as v {
                machine.eval(value)
            }
        },
        builtin! {
            fn string(_machine, codes: List) as string {
                Ok(Rc::new(Value::Name(String::from_iter(codes
                    .into_iter()
                    .map(|value| {
                        if let Value::Integer(value) = value.as_ref() {
                            char::from_u32(*value as u32).ok_or_else(|| Error::BuiltinError(format!("invalid character value {}", value)))
                        } else {
                            Err(Error::BuiltinError(format!("non-integer value {} supplied", value)))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
                ))))
            }
        },
        builtin! {
            fn chars(_machine, name: Name) as chars {
                Ok(Rc::new(Value::List(name.chars().map(|char| Rc::new(Value::Integer(char as i64))).collect())))
            }
        },
        builtin! {
            fn type(_machine, value: any) as type {
                Ok(Rc::new(Value::Name(match value.as_ref() {
                    Value::List(_) => "List",
                    Value::Builtin(_) => "Builtin",
                    Value::Integer(_) => "Integer",
                    Value::Name(_) => "Name",
                }.to_string())))
            }
        },
        builtin! {
            macro quote(_machine, thing: any) as q {
                Ok(thing)
            }
        },
        builtin! {
            tce macro if(machine, condition: any, if_truthy: any, if_falsy: any) as i {
                if machine.eval(condition)?.truthy() {
                    machine.eval(if_truthy)
                } else {
                    machine.eval(if_falsy)
                }
            }
        },
        builtin! {
            macro def(machine, (name_raw -> name): Name, value: any) as d {
                if machine.scope.global_lookup(name).is_none() {
                    let value = machine.eval(value)?;
                    machine.scope.insert(name.to_string(), value);
                    Ok(name_raw)
                } else {
                    Err(Error::DefinedName(name.to_string(), name_raw.as_ref().clone()))
                }
            }
        },
    ])
});
