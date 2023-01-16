class Main {
    main(): Object {{
        let add_const : Int <- 1 + 2,
            sub_const : Int <- 3 - 1,
            mul_const : Int <- 2 * 3,
            div_const : Int <- 4 / 2 in 0;

        let x : Int <- 42,
            y : Int <- ~37 + 1,
            add_var : Int <- x + y,
            sub_var : Int <- x - y,
            mul_var : Int <- x * y,
            div_var : Int <- x / y in 0;

        let compl : Int <- ~42 in 0;

        let cmp_lt : Int <- 3 < 6,
            cmp_le : Int <- 6 <= 6,
            cmp_eq : Int <- 6 = 6 in 0;
    }};
};
