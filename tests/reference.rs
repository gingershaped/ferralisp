use std::rc::Rc;
use ferralisp::{parser::parse, value::Value, util::dummy_machine};
use itertools::{EitherOrBoth, Itertools};


macro_rules! tl_test {
    ($file:ident) => {
        #[test]
        fn $file() {
            let inputs = parse(include_str!(concat!(
                "reference/",
                stringify!($file),
                ".tl"
            )))
            .expect(concat!(
                "failed to parse input for test ",
                stringify!($file),
            ))
            .into_iter()
            .map(|v| Value::of(v))
            .collect::<Vec<Rc<Value>>>();
            let outputs = include_str!(concat!("reference/", stringify!($file), ".tl.out")).lines();

            let mut machine = dummy_machine();

            for item in inputs.into_iter().zip_longest(outputs) {
                let EitherOrBoth::Both(input, output_str) = item else {
                    panic!("input and output are not the same length");
                };
                let output_value = machine.eval(input).expect("evaluation failed");
                assert_eq!(format!("{}", output_value), output_str);
            }
        }
    };
}

tl_test!(primitives);
tl_test!(define);
tl_test!(weird_define);
tl_test!(conditionals);
tl_test!(functions);
tl_test!(length);
tl_test!(primes);
