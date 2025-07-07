use {crate::test::assert_eval, compose_library::Value};

#[test]
fn fibonacci() {
    let input = r#"
        let mut a = 1;
        let mut b = 0;
        let mut i = 1;

        while i < 92 {
            a += b;
            b = a - b;
            i += 1;
        };

        a;
    "#;

    assert_eq!(assert_eval(input), Value::Int(7540113804746346429));
}

#[test]
fn shared_state() {
    let input = r#"
        let mut i = box::new(0);
        
        let inc = (ref mut v) => { *v += 1; };
        
        inc(i);
        
        assert::eq(*i, 1);
        
        inc(i);
        
        assert::eq(*i, 2);
    "#;

    assert_eval(input);
}

#[test]
fn closure_recursion() {
    let input = r#"
        // Recursively clamps a value to <= 0;
        let no_pos = (v) => {
            if v > 0 {
                no_pos(v - 1);
            } else {
                v;
            };
        };

        assert::eq(no_pos(5), 0);
        assert::eq(no_pos(-2), -2);
    "#;

    assert_eval(input);
}

#[test]
fn closure_recursion_2() {
    let input = r#"
        let fact = (n) => {
            if n == 0 {
                1;
            } else {
                n * fact(n - 1);
            };
        } ;
        
        assert::eq(fact(5), 120);
    "#;

    assert_eval(input);
}

#[test]
fn closure_capturing() {
    let input = r#"
        let create_counter = (from = 0) => {
            let mut cur = box::new(from);
            |ref mut cur| () => {
                let ret = *cur;
                *cur += 1;
                ret;
            };
        };

        let counter = create_counter(from: 3);

        assert::eq(counter(), 3);
        assert::eq(counter(), 4);
        assert::eq(counter(), 5);
        assert::eq(counter(), 6);
        assert::eq(counter(), 7);
        assert::eq(counter(), 8);
    "#;

    assert_eval(input);
}
