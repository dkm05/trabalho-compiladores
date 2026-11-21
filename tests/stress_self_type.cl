class Base inherits IO {
    clone() : SELF_TYPE {
        new SELF_TYPE
    };

    chain() : SELF_TYPE {
        let tmp : SELF_TYPE <- self in {
            out_string("ok\n");
            tmp;
        }
    };
};

class Sub inherits Base {
    extra() : Int { 42 };
};

class Main {
    main() : Int {
        let s : Sub <- (new Sub).clone().chain() in
            s.extra()
    };
};
