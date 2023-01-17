class Base {
    bad_ret(): Int {
        0
    };

    bad_param(x : Int): Object {
        self
    };

    too_many(x : Int): Object {
        self
    };

    too_few(x : Int, y : Int): Object {
        self
    };
};

class Derived inherits Base {
    bad_ret(): String {
        "hey"
    };

    bad_param(x : String): Object {
        self
    };

    too_many(x : Int, y : Int): Object {
        self
    };

    too_few(x : Int): Object {
        self
    };
};

class Main inherits Derived {
    main(): Object {
        0
    };
};
