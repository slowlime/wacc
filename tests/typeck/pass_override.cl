class Base {
    test(x : Int, y : Base): String {
        "hello world"
    };
};

class Derived {
    test(y : Int, x : Base): String {
        "mic test"
    };
};

class Main inherits Derived {
    main(): Object {
        0
    };
};
