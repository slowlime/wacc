class Main {
    int_min : Int <- ~2147483647; -- -2147483648

    io : IO <- new IO;

    println(n : Int): IO {
        io.out_int(n).out_string("\n")
    };

    neg(n : Int): Int {
        ~n + 1
    };

    test_add_sub(): Object {{
        io.out_string("test_add_sub:\n");
        println(0 + 0);
        println(0 - 0);
        println(2147483647 + 1);
        println(int_min - 1);
        println(int_min + int_min);
    }};

    test_mul_div(): Object {{
        io.out_string("\ntest_mul_div:\n");
        println(0 * 0);

        println(0 / int_min);
        println(1 / int_min);

        -- wasm leaves `int_min / -1` undefined; wacc extends it to equal int_min
        println(int_min / neg(1));
        println(int_min * neg(1));

        println(2 * 4 * 6 * 8 * 10);

        println(1 / neg(1));
        println(neg(1) / 1);

        -- most software would agree (5 / -2) is -3 in signed integer arithmetic
        -- wasm insists it's -2, so it's -2 for me as well
        println(5 / 2);
        println(5 / neg(2));
        println(neg(5) / 2);
        println(neg(5) / neg(2));
    }};

    main(): Object {{
        test_add_sub();
        test_mul_div();
    }};
};
