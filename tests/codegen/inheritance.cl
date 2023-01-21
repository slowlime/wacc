class Base {
    x : Int <- 42;

    x(): Int {
        x + 4
    };

    get_actual_x(): Int { x };
};

class Derived inherits Base {
    y : Int <- x + 3;
    z : Int <- y + y;

    x(): Int {
        self@Base.x() + 8
    };

    get_actual_y(): Int { y };
    get_actual_z(): Int { z };
};

class Main {
    base : Base <- new Base;
    derived : Derived <- new Derived;
    io : IO <- new IO;

    main(): Object {{
        io.out_int(base.get_actual_x()).out_string("\n");
        io.out_int(base.x()).out_string("\n");

        io.out_int(derived.get_actual_x()).out_string("\n");
        io.out_int(derived.x()).out_string("\n");
        io.out_int(derived.get_actual_y()).out_string("\n");
        io.out_int(derived.get_actual_z()).out_string("\n");
    }};
};
