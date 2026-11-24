class A {
    bad() : SELF_TYPE {
        new A
    };
};

class Main {
    main() : Object { new A };
};
