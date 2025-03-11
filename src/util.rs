use crate::{
    machine::{Machine, OptimizationLevel, World},
    value::Value,
};

pub struct DummyWorld;
impl World for DummyWorld {
    fn disp(&self, _: &Value) {}
}

pub fn dummy_machine() -> Machine {
    Machine::new(DummyWorld, vec![], OptimizationLevel::Normal)
}

#[macro_export]
macro_rules! parse_value {
    ($machine:ident, $value:expr) => {
        $machine.hydrate(
            $crate::parser::parse_expression($value)
                .expect("failed to parse value literal")
                .into(),
        )
    };
}

#[macro_export]
macro_rules! parse_list {
    ($machine:ident, $code:expr) => {
        if let $crate::value::Value::List(list) =
            $machine.hydrate($crate::parser::parse_expression($code).unwrap())
        {
            refpool::PoolRef::try_unwrap(list).unwrap()
        } else {
            unreachable!()
        }
    };
}

#[macro_export]
macro_rules! assert_eval {
    ($input:expr, $output:expr) => {
        let mut machine = $crate::util::dummy_machine();
        let parsed = $crate::parse_value!(machine, $input);

        assert_eq!(
            machine.eval(&parsed),
            Ok($crate::parse_value!(machine, $output)),
        )
    };
}

pub fn or_fallback(string: &Option<String>) -> &str {
    match string {
        Some(string) => string,
        None => "???",
    }
}
