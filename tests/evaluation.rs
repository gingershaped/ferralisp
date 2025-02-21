mod common;

use std::rc::Rc;

use common::test_machine;
use ferralisp::{parser::parse, value::Value};
use test_log::test;

#[test]
fn primitive_eval() {
    let mut machine = test_machine();

    assert_eq!(machine.eval(Value::of(12345)), Ok(Value::of(12345)));
    assert_eq!(machine.eval(Value::nil()), Ok(Value::nil()));
    assert_eq!(machine.eval(Value::of("foo")), Ok(Value::of(11111)))
}

#[test]
fn functions_and_macros() {
    let mut machine = test_machine();

    let nadic_function: Rc<Value> =
        Rc::new(parse("(q ((frob) frob))").unwrap().pop().unwrap().into());

    assert_eq!(
        machine.eval(Rc::new(
            vec![nadic_function.clone(), Value::of(123)]
                .into_iter()
                .collect()
        )),
        Ok(Value::of(123))
    );
}
