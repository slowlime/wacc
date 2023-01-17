class Base {
    x : SELF_TYPE <- self;
    x_id : SELF_TYPE <- id();

    id(): SELF_TYPE {
        self
    };
};

class Main inherits Base {
    main(): SELF_TYPE {{
        x <- self;
        x_id <- id();
    }};
};
