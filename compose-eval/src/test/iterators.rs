use crate::test::assert_eval;

#[test]
fn test_unbounded_range_iterator() {
    assert_eval(
        r#"
    let mut r = (0..).iter();
    assert::eq(r.next(), 0);
    assert::eq(r.next(), 1);

    assert::eq(r.nth(0), 2);
    assert::eq(r.nth(1), 4);
    assert::eq(r.nth(1), 6);
    assert::eq(r.nth(9), 16);
    "#,
    );
}

#[test]
fn test_bounded_range_iterator() {
    assert_eval(
        r#"
    let mut r = (0..3).iter();
    assert::eq(r.next(), 0);
    assert::eq(r.next(), 1);
    assert::eq(r.next(), 2);
    assert::eq(r.next(), ());
    "#,
    );
}

#[test]
fn test_empty_range_iterator() {
    assert_eval(
        r#"
    let mut r = (0..0).iter();
    assert::eq(r.next(), ());
    "#,
    );
}

#[test]
fn test_step_by_iterator() {
    assert_eval(
        r#"
        let mut r = (0..35).iter().step_by(2);
        assert::eq(r.next(), 0);
        assert::eq(r.next(), 2);
        assert::eq(r.next(), 4);

        assert::eq(r.nth(0), 6);
        assert::eq(r.nth(1), 10);
        assert::eq(r.nth(1), 14);
        assert::eq(r.nth(9), 34);
        assert::eq(r.next(), ());
        "#,
    );
}

#[test]
fn test_take_iter() {
    assert_eval(
        r#"
        let mut r = (0..10).iter().take(5);
        assert::eq(r.next(), 0);
        assert::eq(r.next(), 1);
        assert::eq(r.next(), 2);
        assert::eq(r.next(), 3);
        assert::eq(r.next(), 4);
        assert::eq(r.next(), ());
        "#,
    );
}

#[test]
fn test_skip_iter() {
    assert_eval(
        r#"
        let mut r = (0..10).iter().skip(5);
        assert::eq(r.next(), 5);
        assert::eq(r.next(), 6);
        assert::eq(r.next(), 7);
        assert::eq(r.next(), 8);
        assert::eq(r.next(), 9);
        "#,
    );
}

#[test]
fn test_map_iter() {
    assert_eval(
        r#"
        let mut r = (0..3).iter().map(x => x * 2);

        assert::eq(r.next(), 0);
        assert::eq(r.next(), 2);
        assert::eq(r.next(), 4);
        assert::eq(r.next(), ());
        "#,
    );
}

#[test]
fn test_take_while_iter() {
    assert_eval(
        r#"
        let mut r = (0..10).iter().take_while(x => x % 2 == 0 || x < 3);
        assert::eq(r.next(), 0);
        assert::eq(r.next(), 1);
        assert::eq(r.next(), 2);
        assert::eq(r.next(), ());
        "#,
    );
}
