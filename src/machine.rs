use std::collections::{HashMap, VecDeque};

use strum_macros::IntoStaticStr;

use crate::builtins::Builtin;

#[derive(Debug, IntoStaticStr)]
pub enum Value<'a> {
    Integer(i64),
    Name(&'a str),
    List(VecDeque<Value<'a>>),
    Builtin(&'a Builtin),
}

pub struct Machine<'a> {
    pub globals: HashMap<&'a str, Value<'a>>,
}