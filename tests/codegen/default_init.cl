class Conversions {
    int_to_string(n : Int): String {
        if n = 0 then
            "0"
        else
            let result : String,
                neg : Bool <- n < 0
            in {
                if neg then
                    n <- ~n + 1
                else
                    0
                fi;

                while not (n = 0) loop
                    let div : Int <- n / 10,
                        rem : Int <- n - div * 10,
                        c : String <- digit_to_string(rem)
                    in {
                        result <- c.concat(result);
                        n <- div;
                    }
                pool;

                (if neg then "-" else "" fi).concat(result);
            }
        fi
    };

    digit_to_string(n : Int): String {
        if n < 8 then
            if n < 4 then
                if n < 2 then
                    if n < 1 then
                        "0"
                    else
                        "1"
                    fi
                else
                    if n = 2 then
                        "2"
                    else
                        "3"
                    fi
                fi
            else
                if n < 6 then
                    if n = 4 then
                        "4"
                    else
                        "5"
                    fi
                else
                    if n = 6 then
                        "6"
                    else
                        "7"
                    fi
                fi
            fi
        else
            if n = 8 then
                "8"
            else
                "9"
            fi
        fi
    };

    bool_to_string(value : Bool): String {
        if value then
            "true"
        else
            "false"
        fi
    };
};

class Main {
    f_string : String <- new String;
    f_int : Int <- new Int;
    f_bool : Bool <- new Bool;
    f_object : Object <- new Object;

    f_string0 : String;
    f_int0 : Int;
    f_bool0 : Bool;
    f_object0 : Object;

    io : IO <- new IO;
    conv : Conversions <- new Conversions;

    main(): Object {
        let l_string : String <- new String,
            l_int : Int <- new Int,
            l_bool : Bool <- new Bool,
            l_object : Object <- new Object,

            l_string0 : String,
            l_int0 : Int,
            l_bool0 : Bool,
            l_object0 : Object
        in {
            print(f_string);
            print(f_int);
            print(f_bool);
            print(f_object);

            print(f_string0);
            print(f_int0);
            print(f_bool0);
            print(f_object0);

            print(l_string);
            print(l_int);
            print(l_bool);
            print(l_object);

            print(l_string0);
            print(l_int0);
            print(l_bool0);
            print(l_object0);
        }
    };

    print(value : Object): Object {
        let s : String <-
                if isvoid value then
                    "null"
                else
                    case value of
                        x : String => escape_string(x);
                        x : Int => conv.int_to_string(x);
                        x : Bool => conv.bool_to_string(x);
                        x : Object => "[Object]";
                    esac
                fi
        in io.out_string(s).out_string("\n")
    };

    escape_string(s : String): String {
        let i : Int <- 0,
            result : String
        in {
            while i < s.length() loop
                let c : String <- s.substr(i, 1)
                in {
                    result <- result.concat(
                        if c = "\"" then
                            "\\\""
                        else
                            if c = "\\" then
                                "\\\\"
                            else
                                c
                            fi
                        fi
                    );
                    i <- i + 1;
                }
            pool;

            "\"".concat(result).concat("\"");
        }
    };
};
