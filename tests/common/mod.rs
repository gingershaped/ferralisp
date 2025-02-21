use std::{collections::HashMap, rc::Rc};

use ferralisp::{machine::Machine, parser::parse, scope::GlobalScope, value::Value};

pub fn test_machine() -> Machine {
    Machine {
        scope: GlobalScope::with_globals(HashMap::from_iter([(
            "foo".to_string(),
            Value::of(11111),
        )])),
    }
}

pub fn parse_single(input: &str) -> Rc<Value> {
    Rc::new(parse(input).unwrap().pop().unwrap().into())
}