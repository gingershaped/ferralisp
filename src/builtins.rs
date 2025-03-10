//! all the tinylisp builtins. these will be injected into the global namespace
//! of newly created machines.

use std::{collections::HashMap, rc::Rc, sync::LazyLock};

use crate::{
    machine::{Error, Machine, ValueResult},
    value::{List, Value},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Builtin {
    pub name: &'static str,
    pub is_macro: bool,
    pub eval_during_tce: bool,
    pub body: fn(&List, &mut Machine, bool) -> ValueResult,
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
            body: |args, $machine, _| {
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
            body: |args, $machine, $tce_active| {
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
        let mut $args = $args.into_iter();
        $(builtin_argument!($alias, $args, $argname: $argtype);)*
        // if arg_index != $args.len() {
        //     return Err(Error::ExtraArguments {
        //         call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
        //         arguments: $args.into_iter().map(|value| value.clone()).collect()
        //     })
        // }
    };
}

macro_rules! builtin_argument {
    ($alias:ident, $args:ident, $name:ident: any) => {
        #[allow(unused_mut)]
        let mut $name = $args.next().cloned().ok_or_else(|| Error::MissingArgument {
            name: Some(stringify!($name).to_string()),
            call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
            expected_type: Some("any".to_string()),
        })?;
    };
    ($alias:ident, $args:ident, $name:ident: $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.next();
        #[allow(unused_mut)]
        let mut $name = match arg {
            Some(arg) => match arg {
                $argtype(value) => Ok(value),
                other => Err(Error::WrongBuiltinArgumentType {
                    alias: stringify!($alias),
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: Some(stringify!($name).to_owned()),
                call_target: Value::Builtin(crate::builtins::BUILTINS[stringify!($alias)]),
                expected_type: Some(stringify!($argtype).to_owned()),
            }),
        }?;
    };
    ($alias:ident, $args:ident, ($raw:ident -> $name:ident): $argtype:path) => {
        #[allow(unused_imports)]
        use Value::*;
        let arg = $args.next().cloned();
        #[allow(unused_mut)]
        let ($raw, mut $name) = match &arg {
            Some(arg) => match arg {
                $argtype(value, ..) => Ok((arg, value)),
                other => Err(Error::WrongBuiltinArgumentType {
                    alias: stringify!($alias),
                    name: stringify!($name),
                    expected_type: stringify!($argtype),
                    value: other.clone(),
                }),
            },
            None => Err(Error::MissingArgument {
                name: Some(stringify!($name).to_owned()),
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
                Ok(List(Rc::new(crate::value::List::Cons(value, list.clone()))))
            }
        },
        builtin! {
            fn head(_machine, value: List) as h {
                Ok(value.head().cloned().unwrap_or(Value::nil()))
            }
        },
        builtin! {
            fn tail(_machine, value: List) as t {
                Ok(value.tail().map(|tail| List(tail)).unwrap_or(Value::nil()))
            }
        },
        builtin! {
            fn add(_machine, a: Integer, b: Integer) as a {
                Ok(Value::Integer(a + b))
            }
        },
        builtin! {
            fn subtract(_machine, a: Integer, b: Integer) as s {
                Ok(Value::Integer(a - b))
            }
        },
        builtin! {
            fn less_than(_machine, a: Integer, b: Integer) as l {
                Ok(Value::Integer((a < b).into()))
            }
        },
        builtin! {
            fn equal(_machine, a: any, b: any) as e {
                Ok(Value::Integer((a == b).into()))
            }
        },
        builtin! {
            tce fn eval(machine, _tce_active, value: any) as v {
                machine.eval(value.clone())
            }
        },
        builtin! {
            fn string(machine, codes: List) as string {
                let name = String::from_iter(codes
                    .iter()
                    .map(|value| {
                        if let Value::Integer(value) = value {
                            char::from_u32(*value as u32).ok_or_else(|| Error::BuiltinError(format!("invalid character value {}", value)))
                        } else {
                            Err(Error::BuiltinError(format!("non-integer value {} supplied", value)))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
                );
                Ok(machine.create_name(&name))
            }
        },
        builtin! {
            fn chars(_machine, name: Name) as chars {
                let interner = name.1.upgrade().expect("unbound name passed to (chars)");
                let interner = interner.borrow();
                let name = interner.resolve(&name.0);
                Ok(Value::List(Rc::new(name.chars().map(|char| Value::Integer(char as i64)).collect())))
            }
        },
        builtin! {
            fn type(machine, value: any) as type {
                Ok(machine.create_name(match value {
                    Value::List(_) => "List",
                    Value::Builtin(_) => "Builtin",
                    Value::Integer(_) => "Integer",
                    Value::Name(_) => "Name",
                }))
            }
        },
        builtin! {
            fn disp(machine, value: any) as disp {
                machine.world.disp(&value);
                Ok(value.clone())
            }
        },
        builtin! {
            macro quote(_machine, thing: any) as q {
                Ok(thing.clone())
            }
        },
        builtin! {
            tce macro if(machine, tce_active, condition: any, if_truthy: any, if_falsy: any) as i {
                let selected_branch = if machine.eval(condition.clone())?.truthy() {
                    if_truthy
                } else {
                    if_falsy
                };
                if tce_active {
                    Ok(selected_branch.clone())
                } else {
                    machine.eval(selected_branch.clone())
                }
            }
        },
        builtin! {
            macro def(machine, (name_raw -> name): Name, new_value: any) as d {
                let interner = name.1.upgrade().expect("unbound name passed to (d)");
                let name = name.0;
                match machine.scope.global_lookup(&name) {
                    None => {
                        let new_value = machine.eval(new_value.clone())?;
                        machine.scope.insert(name, new_value);
                        Ok(name_raw.clone())
                    }
                    Some(current_value) => {
                        Err(Error::DefinedName {
                            name: interner.borrow().try_resolve(&name).map(|v| v.to_owned()),
                            current_value: current_value.clone(),
                            new_value: new_value.clone()
                        })
                    }
                }
            }
        },
        builtin! {
            macro load(machine, path: Name) as load {
                let interner = path.1.upgrade().expect("unbound name passed to (chars)");
                let interner = interner.borrow();
                let path = interner.resolve(&path.0);
                machine.load(path.to_owned())
            }
        },
        builtin! {
            macro comment(_machine, *args) as comment {
                Ok(Value::nil())
            }
        },
    ])
});

#[cfg(test)]
mod test {
    use test_log::test;

    use crate::{assert_eval, parse_value, util::dummy_machine};

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
        let parsed = parse_value!(machine, "(d the_answer 42)");

        assert_eq!(machine.eval(parsed), Ok(machine.create_name("the_answer")),);
        assert_eq!(
            machine.eval(machine.create_name("the_answer")),
            Ok(42.into())
        );
    }
}
