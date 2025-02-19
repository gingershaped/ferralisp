use std::collections::HashMap;

use ferralisp::{machine::Machine, scope::GlobalScope, value::Value};

#[test]
fn primitive_eval() {
    let mut machine = Machine {
        scope: GlobalScope::with_globals(HashMap::from_iter([(
            "foo".to_string(),
            Value::of(11111),
        )])),
    };

    assert_eq!(machine.eval(Value::of(12345)), Ok(Value::of(12345)));
    assert_eq!(machine.eval(Value::nil()), Ok(Value::nil()));
    assert_eq!(machine.eval(Value::of("foo")), Ok(Value::of(11111)))
}
