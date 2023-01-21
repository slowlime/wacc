class Main {
    int : Int;
    string : String;
    bool : Bool;
    object : Object;

    io : IO <- new IO;

    main(): Object {{
        print_is_void(int);
        print_is_void(string);
        print_is_void(bool);
        print_is_void(object);

        print_is_void(while false loop 0 pool);
    }};

    print_is_void(x : Object): Object {
        if isvoid x then
            io.out_string("null\n")
        else
            io.out_string("not null\n")
        fi
    };
};
