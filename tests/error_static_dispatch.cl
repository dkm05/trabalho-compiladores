class A {
    foo() : Int { 1 };
};

class B inherits A {};

class C {
    foo() : Int { 2 };
};

class Main {
    main() : Int {
        (new B)@C.foo()
    };
};
