class Main {
    io : IO <- new IO;
    x : Int <- 0;

    main(): Object {
        test(x)
    };

    test(x : Int): Object {
        let x : Int <- x + 1,
            x : Int <- x + 2,
            x : Int <- x + 3
        in io.out_int(x).out_string("\n")
    };
};
