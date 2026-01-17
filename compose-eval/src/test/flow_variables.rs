#[cfg(test)]
use {
    crate::test::{assert_eval, eval_code},
    compose_error_codes::E0011_UNBOUND_VARIABLE
};

#[test]
fn is_expression() {
    assert_eval(
        r#"
    let two_is_even = 2 is Int x && x % 2 == 0;

    assert(two_is_even);
    "#,
    );
}

#[test]
fn is_expression_flow_scope_does_not_leak() {
    eval_code(
        r#"
        let two_is_even = 2 is Int x && x % 2 == 0;

        x; // should not be in scope here
    "#,
    )
    .assert_errors(&[E0011_UNBOUND_VARIABLE]);
}
