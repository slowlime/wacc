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

class Printer inherits IO {
    conv : Conversions <- new Conversions;

    println(value : Object): SELF_TYPE {
        print(value).out_string("\n")
    };

    print(value : Object): SELF_TYPE {
        let s : String <-
                if isvoid value then
                    "null"
                else
                    case value of
                        x : String => escape_string(x);
                        x : Int => conv.int_to_string(x);
                        x : Bool => conv.bool_to_string(x);
                        x : Object => "[".concat(x.type_name()).concat("]");
                    esac
                fi
        in out_string(s)
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

class Main {
    printer : Printer <- new Printer;

    main(): Object {{
        print_string_params("hello,".concat(" world"));
        print_string_params("hell".concat(""));
        print_string_params("".concat("hell"));
        print_string_params("".concat(""));

        print_string_params("hello, world".substr(1, 0));
        print_string_params("hello, world".substr(4, 2));
        print_string_params("".substr(0, 0));
    }};

    print_string_params(s : String): Object {
        printer.print(s).out_string(", len ").println(s.length())
    };
};
