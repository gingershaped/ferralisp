use crate::{
    machine::{Machine, World},
    value::Value,
};

pub struct DummyWorld;
impl World for DummyWorld {
    fn disp(&self, _: &Value) {}
}

pub fn dummy_machine() -> Machine {
    Machine::new(DummyWorld, vec![])
}

#[macro_export]
macro_rules! parse_value {
    ($value:expr) => {
        Rc::new(
            $crate::parser::parse_expression($value)
                .expect("failed to parse value literal")
                .into(),
        )
    };
}

#[macro_export]
macro_rules! parse_list {
    ($code:expr) => {
        if let $crate::value::Value::List(list) =
            $crate::parser::parse_expression($code).unwrap().into()
        {
            list
        } else {
            unreachable!()
        }
    };
}

#[macro_export]
macro_rules! assert_eval {
    ($input:expr, $output:expr) => {
        let mut machine = $crate::util::dummy_machine();

        assert_eq!(
            machine.eval($crate::parse_value!($input)),
            Ok($crate::parse_value!($output)),
        )
    };
}
