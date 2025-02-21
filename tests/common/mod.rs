use std::collections::HashMap;

use ferralisp::{machine::Machine, scope::GlobalScope, value::Value};

pub fn test_machine() -> Machine {
    Machine {
        scope: GlobalScope::with_globals(HashMap::from_iter([(
            "foo".to_string(),
            Value::of(11111),
        )])),
    }
}