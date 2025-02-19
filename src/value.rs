//! values are the units of data which exist inside the machine. there are four kinds of them:
//! - List: a singly-linked list of values
//! - Builtin: a built-in function or macro
//! - Integer: a signed 64-bit integer
//! - Name: a name
//! values are usually wrapped in an Rc, but may be cloned for situations like displaying error messages.
//! all values are truthy for the purposes of builtins like `i`,
//! except for the number zero and the empty list (nil).

use std::{fmt::Display, rc::Rc};

use strum_macros::IntoStaticStr;

use crate::{builtins::Builtin, list::List, parser::Expression};

#[derive(Clone, Debug, IntoStaticStr, PartialEq, Eq)]
pub enum Value {
    List(List<Rc<Value>>),
    Builtin(Builtin),
    Integer(i64),
    Name(String),
}

impl Value {
    pub fn truthy(&self) -> bool {
        match self {
            Value::Integer(0) => false,
            Value::List(list) => list.len() > 0,
            _ => true,
        }
    }

    pub fn nil() -> Rc<Value> {
        Rc::new(Value::List(List::nil()))
    }

    pub fn of<T: Into<Value>>(value: T) -> Rc<Value> {
        Rc::new(value.into())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(values) => {
                let values: Vec<&Rc<Value>> = values.into_iter().collect();
                write!(f, "[")?;
                for (index, value) in values.iter().enumerate() {
                    value.fmt(f)?;
                    if index != values.len() - 1 {
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

impl From<Expression<'_>> for Value {
    fn from(expr: Expression) -> Self {
        match expr {
            Expression::Integer(value) => Value::Integer(value),
            Expression::Name(name) => Value::Name(name.to_owned()),
            Expression::List(expressions) => Value::List(
                expressions
                    .into_iter()
                    .map(|expr| Rc::new(expr.into()))
                    .collect(),
            ),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Name(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Name(value.to_string())
    }
}
