//! values are the units of data which exist inside the machine. there are four kinds of them:
//! - List: a singly-linked list of values
//! - Builtin: a built-in function or macro
//! - Integer: a signed 64-bit integer
//! - Name: a name
//!   all values are truthy for the purposes of builtins like `i`,
//!   except for the number zero and the empty list (nil).

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::{Rc, Weak},
};

use lasso::{Rodeo, Spur};
use strum_macros::IntoStaticStr;

use crate::builtins::Builtin;

#[derive(Clone, IntoStaticStr)]
pub enum Value {
    List(Rc<Vec<Value>>),
    Builtin(Builtin),
    Integer(i64),
    Name((Spur, Weak<RefCell<Rodeo>>)),
}

impl Value {
    pub fn truthy(&self) -> bool {
        match self {
            Value::Integer(0) => false,
            Value::List(list) => !list.is_empty(),
            _ => true,
        }
    }

    pub fn nil() -> Value {
        Value::List(Rc::new(vec![]))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Builtin(l0), Self::Builtin(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Name((l0, _)), Self::Name((r0, _))) => l0 == r0,
            _ => false,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(values) => {
                write!(f, "(")?;
                for (index, value) in values.iter().enumerate() {
                    Debug::fmt(value, f)?;
                    if index != values.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")?;
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
            Value::Name((name, interner)) => match interner.upgrade() {
                Some(interner) => match interner.borrow().try_resolve(name) {
                    Some(name) => write!(f, "{}", name),
                    None => write!(f, "<invalid key {}>", name.into_inner()),
                },
                None => write!(f, "<unbound key {}>", name.into_inner()),
            },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<Spur> for Value {
    fn from(value: Spur) -> Self {
        Value::Name((value, Weak::new()))
    }
}

impl FromIterator<Value> for Value {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Value::List(Rc::new(iter.into_iter().collect()))
    }
}