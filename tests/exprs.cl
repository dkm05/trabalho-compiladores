class Main inherits IO {
    main() : Object {
        let x : Int <- 10 in
        if x < 20 then
            out_string("Menor\n")
        else
            out_string("Maior\n")
        fi
    };
};
