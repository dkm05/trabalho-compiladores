class A {
    id() : Int { 1 };
    self_ref() : A { self };
};

class B inherits A {
    id() : Int { 2 };
};

class C inherits B {
    id() : Int { 3 };
    test_static() : Int {
        self@A.id() + self@B.id() + id()
    };
};

class Main {
    main() : A {
        let c : C <- new C,
            b : B <- new B,
            x : A <- if true then c else b fi in
        {
            while false loop c.test_static() pool;
            case x of
                n1 : C => n1;
                n2 : B => n2;
                n3 : A => n3;
            esac;
        }
    };
};
