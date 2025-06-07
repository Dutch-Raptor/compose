use compose_library::Value;
use crate::expression::test_utils::eval_code;
#[test]
fn fibonacci() {
    let input = r#"
        let mut a = 1;
        let mut b = 0;
        let mut i = 1;

        while i < 92 {
            println(i, a);
            a += b;
            b = a - b;
            i += 1;
        }

        a
    "#;
    
    assert_eq!(eval_code(input).unwrap(), Value::Int(7540113804746346429));
}