class A {
    x : Int <- 5;
    foo(y : Int) : Int {
        y + x
    };
};

class B inherits A {
    foo(y : Int) : Int {
        y * 2
    };
};

class Main {
    main() : Int {
        {
            let a : A <- new A in a.foo(10);
            let b : B <- new B in b.foo(10);
        }
    };
};
