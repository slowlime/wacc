class Main {
    void : Object;
    obj1 : Object <- new Object;
    obj2 : Object <- new Object;
    int1 : Int <- 42;
    int2 : Int <- 34;
    int22 : Int <- 34;
    string1 : String <- "hello";
    string2 : String <- "world";
    bool1 : Bool <- true;
    bool2 : Bool <- false;

    io : IO <- new IO;

    println(x : Bool): Object {
        if x then
            io.out_string("true\n")
        else
            io.out_string("false\n")
        fi
    };

    main(): Object {{
        println(void = void);
        println(void = obj1);
        println(obj1 = void);

        io.out_string("\n");
        println(obj1 = obj1);
        println(obj1 = obj2);
        println(obj2 = obj2);

        io.out_string("\n");
        println(int1 = int2);
        println(int1 = int1);
        println(int1 = 42);
        println(int2 = 34);
        println(int2 = int22);

        io.out_string("\n");
        println(string1 = string2);
        println(string1 = string1);
        println(string1 = "hello");
        println(string2 = "world");

        io.out_string("\n");
        println(bool1 = bool2);
        println(bool1 = bool1);
        println(bool1 = true);
        println(bool2 = false);

        io.out_string("\n");
        case int2 of
            int_obj2 : Object => {
                case int22 of
                    int_obj22 : Object =>
                        -- false because the objects are distinct
                        -- I'm not sure whether Cool guarantees this, tho
                        -- the specification is REALLY imprecise
                        println(int_obj2 = int_obj22);
                esac;

                println(int_obj2 = obj1);
            };
        esac;
    }};
};
