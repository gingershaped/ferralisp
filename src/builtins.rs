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
    pub body: fn(Vec<Rc<Value>>, &mut Machine, bool) -> ValueResult,
}

macro_rules! builtin {
    (
        fn $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, false, $machine, ($($arg)*), $body)
    };
    (
        macro $name:ident($machine:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, true, $machine, ($($arg)*), $body)
    };
    (
        tce fn $name:ident($machine:ident, $tce_active:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, false, $machine, $tce_active, ($($arg)*), $body)
    };
    (
        tce macro $name:ident($machine:ident, $tce_active:ident, $($arg:tt)*) as $alias:ident $body:block
    ) => {
        builtin_inner!($alias, $name, true, $machine, $tce_active, ($($arg)*), $body)
    };
}

macro_rules! builtin_inner {
    ($alias:ident, $name:ident, $is_macro:literal, $machine:ident, ($($arg:tt)*), $body:block) => {
        (stringify!($alias), Builtin {
            name: stringify!($name),
            is_macro: $is_macro,
            eval_during_tce: false,
            #[allow(unused_variables, unused_mut)]
            body: |mut args, $machine, _| {
                builtin_arguments!($alias, args, ($($arg)*));
                $body
            }
        })
    };
    ($alias:ident, $name:ident, $is_macro:literal, $machine:ident, $tce_active:ident, ($($arg:tt)*), $body:block) => {
        (stringify!($alias), Builtin {
            name: stringify!($name),
            is_macro: $is_macro,
            eval_during_tce: true,
            #[allow(unused_variables)]
            body: |mut args, $machine, $tce_active| {
                builtin_arguments!($alias, args, ($($arg)*));
                $body
            }
        })
    };
}

macro_rules! builtin_arguments {
    ($alias:ident, $args:ident, (*$varargs:ident)) => {
        let $varargs = $args;
    };
    ($alias:ident, $args:ident, ($($argname:tt: $argtype:tt),*)) => {
        ($args).reverse();
        $(builtin_argument!($alias, $args, $argname: $argtype);)*
        if !$args.is_empty() {
            return Err(Error::ExtraArguments {
                call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
                arguments: $args.into_iter().map(|value| value.as_ref().clone()).collect()
            })
        }
    };
}

macro_rules! builtin_argument {
    ($alias:ident, $args:ident, $name:ident: any) => {
        #[allow(unused_mut)]
        let mut $name = $args.pop().ok_or_else(|| Error::MissingArgument {
            name: stringify!($name).to_string(),
            call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
            expected_type: Some("any".to_string()),
        })?;
    };
    ($alias:ident, $args:ident, $name:ident: $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.pop();
        #[allow(unused_mut)]
        let mut $name = match arg {
            Some(ref arg) => match arg.as_ref() {
                $argtype(arg) => Ok(arg),
                other => Err(Error::WrongBuiltinArgumentType {
                    alias: stringify!($alias),
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: stringify!($name).to_owned(),
                call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
                expected_type: Some(stringify!($argtype).to_owned()),
            }),
        }?;
    };
    ($alias:ident, $args:ident, ($raw:ident -> $name:ident): $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.pop();
        #[allow(unused_mut)]
        let ($raw, mut $name) = match arg {
            Some(ref arg) => match arg.as_ref() {
                $argtype(value) => Ok((arg.clone(), value)),
                other => Err(Error::WrongBuiltinArgumentType {
                    alias: stringify!($alias),
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: stringify!($name).to_owned(),
                call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
                expected_type: Some(stringify!($argtype).to_owned()),
            }),
        }?;
    };
}

pub static BUILTINS: LazyLock<HashMap<&'static str, Builtin>> = LazyLock::new(|| {
    HashMap::from([
        builtin! {
            fn cons(_machine, value: any, list: List) as c {
                let mut consed_list = vec![value];
                consed_list.extend(list.clone());
                Ok(Rc::new(Value::List(consed_list)))
            }
        },
        builtin! {
            fn head(_machine, value: List) as h {
                Ok(value.first().cloned().unwrap_or(Value::nil()))
            }
        },
        builtin! {
            fn tail(_machine, value: List) as t {
                Ok(value.split_first().map(|(_, value)| Rc::new(Value::List(value.to_vec()))).unwrap_or(Value::nil()))
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
            tce fn eval(machine, _tce_active, value: any) as v {
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
            fn disp(machine, value: any) as disp {
                machine.world.disp(value.as_ref());
                Ok(value)
            }
        },
        builtin! {
            macro quote(_machine, thing: any) as q {
                Ok(thing)
            }
        },
        builtin! {
            tce macro if(machine, tce_active, condition: any, if_truthy: any, if_falsy: any) as i {
                let selected_branch = if machine.eval(condition)?.truthy() {
                    if_truthy
                } else {
                    if_falsy
                };
                if tce_active {
                    Ok(selected_branch)
                } else {
                    machine.eval(selected_branch)
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
        builtin! {
            macro load(machine, path: Name) as load {
                machine.load(path.clone())
            }
        },
        builtin! {
            macro comment(_machine, *args) as comment {
                Ok(Value::nil())
            }
        }
    ])
});

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use test_log::test;

    use crate::{assert_eval, parser::parse_expression, util::dummy_machine, value::Value};

    #[test]
    fn cons() {
        assert_eval!("(c 1 (q (2 3)))", "(1 2 3)");
    }

    #[test]
    fn head() {
        assert_eval!("(h (q (1 2 3)))", "1");
    }

    #[test]
    fn tail() {
        assert_eval!("(t (q (1 2 3)))", "(2 3)");
    }

    #[test]
    fn add() {
        assert_eval!("(a 1 1)", "2");
    }

    #[test]
    fn subtract() {
        assert_eval!("(s 1 1)", "0");
    }

    #[test]
    fn less_than() {
        assert_eval!("(l 1 2)", "1");
        assert_eval!("(l 2 1)", "0");
    }

    #[test]
    fn equal() {
        assert_eval!("(e 1 1)", "1");
        assert_eval!("(e 1 0)", "0");
    }

    #[test]
    fn eval() {
        assert_eval!("(v (q 1))", "1");
    }

    #[test]
    fn string() {
        assert_eval!("(string (q (58 51)))", ":3");
    }

    #[test]
    fn chars() {
        assert_eval!("(chars (q :3))", "(58 51)");
    }

    #[test]
    fn type_() {
        assert_eval!("(type ())", "List");
        assert_eval!("(type type)", "Builtin");
        assert_eval!("(type 1)", "Integer");
        assert_eval!("(type (q type))", "Name");
    }

    #[test]
    fn quote() {
        assert_eval!("(q (1 2 3))", "(1 2 3)");
        assert_eval!("(q q)", "q");
        assert_eval!("(q 1)", "1");
        assert_eval!("(q (q q))", "(q q)");
    }

    #[test]
    fn if_() {
        assert_eval!("(i 1 1 2)", "1");
        assert_eval!("(i 0 1 2)", "2");
    }

    #[test]
    fn def() {
        let mut machine = dummy_machine();

        assert_eq!(
            machine.eval(Rc::new(parse_expression("(d the_answer 42)").unwrap().into())),
            Ok(Value::of("the_answer")),
        );
        assert_eq!(
            machine.eval(Value::of("the_answer")),
            Ok(Value::of(42)),
        );
    }
}
