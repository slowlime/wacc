class Base {
    same_ty : Int;
    different_ty : Int;
};

class Derived inherits Base {
    same_ty : Int;
    different_ty : String;
};

class Main inherits Derived {
    main(): Object {
        0
    };
};
