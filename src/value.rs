use std::{fmt::Display, rc::Rc};

use strum_macros::IntoStaticStr;

use crate::{builtins::Builtin, list::List};

#[derive(Clone, Debug, IntoStaticStr, PartialEq, Eq)]
pub enum Value {
    List(Box<List<Rc<Value>>>),
    Builtin(Builtin),
    Integer(i64),
    Name(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(values) => {
                let values: Vec<&Rc<Value>> = values.as_ref().into_iter().collect();
                write!(f, "[")?;
                for (index, value) in values.iter().enumerate() {
                    value.fmt(f)?;
                    if index != values.len() {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")?;
                Ok(())
            }
            Value::Builtin(builtin) => {
                write!(
                    f,
                    "<builtin {} \"{}\">",
                    if builtin.is_macro {
                        "macro"
                    } else {
                        "function"
                    },
                    builtin.name
                )
            }
            Value::Integer(value) => write!(f, "{}", value),
            Value::Name(name) => write!(f, "{}", name),
        }
    }
}
