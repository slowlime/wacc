class A {};
class B {};
class C {};
class D inherits A {};
class E inherits B {};
class F inherits E {};

class Main {
    io : IO <- new IO;

    main(): Object {
        let f : F <- new F in {
            test1(f);
            test2(f);
            test3(f);
            test4(f);
        }
    };

    test1(f : Object): Object {{
        io.out_string("test1: ");

        case f of
            f : F => io.out_string("f\n");
            e : E => io.out_string("e\n");
            d : D => io.out_string("d\n");
            c : C => io.out_string("c\n");
            b : B => io.out_string("b\n");
            a : A => io.out_string("a\n");
        esac;
    }};

    test2(f : Object): Object {{
        io.out_string("test2: ");

        case f of
            a : A => io.out_string("a\n");
            b : B => io.out_string("b\n");
            c : C => io.out_string("c\n");
            d : D => io.out_string("d\n");
            e : E => io.out_string("e\n");
            f : F => io.out_string("f\n");
        esac;
    }};

    test3(f : Object): Object {{
        io.out_string("test3: ");

        case f of
            a : A => io.out_string("a\n");
            b : B => io.out_string("b\n");
            c : C => io.out_string("c\n");
        esac;
    }};

    test4(f : F): Object {{
        io.out_string("test4: ");

        case f of
            b : B => io.out_string("b\n");
        esac;
    }};
};
