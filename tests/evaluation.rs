mod common;

use std::rc::Rc;

use common::{parse_single, test_machine};
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
fn nadic_function() {
    let mut machine = test_machine();

    assert_eq!(
        machine.eval(parse_single("((q ((frob) frob)) 123)")),
        Ok(Value::of(123))
    );
}

#[test]
fn variadic_function() {
    let mut machine = test_machine();

    assert_eq!(
        machine.eval(parse_single("((q (frob (h frob))) 1 2 3)")),
        Ok(Value::of(1)),
    );
}

#[test]
fn macro_function() {
    let mut machine = test_machine();

    assert_eq!(
        machine.eval(parse_single("((q (() args (v (h args)))) (h (q (4 5 6))) 2 3)")),
        Ok(Value::of(4)),
    );
}