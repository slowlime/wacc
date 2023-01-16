class Base {
    x : Int <- 42;
    y : Bool <- true;

    uninit : Object;
};

class Derived inherits Base {
    z : Int <- if y then x + 1 else 1 - x fi;
};
