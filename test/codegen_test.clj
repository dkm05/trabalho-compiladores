(ns codegen-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]
            [codegen.core :as codegen]
            [codegen.types :as types])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- delete-recursively! [^File f]
  (when (.exists f)
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursively! child)))
    (.delete f)))

(defn- run-jvm-main [^File out-dir stdin-str]
  (let [pb   (ProcessBuilder. ^java.util.List ["java" "-cp" (.getAbsolutePath out-dir) "Main"])
        proc (.start pb)]
    (when stdin-str
      (with-open [os (.getOutputStream proc)]
        (.write os (.getBytes ^String stdin-str "UTF-8"))
        (.flush os)))
    (let [stdout-fut (future (slurp (.getInputStream proc)))
          stderr-fut (future (slurp (.getErrorStream proc)))
          exit-code  (.waitFor proc)]
      {:exit   exit-code
       :stdout @stdout-fut
       :stderr @stderr-fut})))

(defn- compile-and-run
  ([cool-src] (compile-and-run cool-src nil))
  ([cool-src stdin-str]
   (let [tmp-dir (.toFile (Files/createTempDirectory "cool-e2e-" (into-array FileAttribute [])))]
     (try
       (codegen/compile-source! cool-src tmp-dir "test.cl" {:verify? true})
       (run-jvm-main tmp-dir stdin-str)
       (finally
         (delete-recursively! tmp-dir))))))

(defn- compile-file-and-run [path]
  (let [tmp-dir (.toFile (Files/createTempDirectory "cool-file-e2e-" (into-array FileAttribute [])))]
    (try
      (codegen/compile-file! path tmp-dir {:verify? true})
      (run-jvm-main tmp-dir nil)
      (finally
        (delete-recursively! tmp-dir)))))

;; ---------------------------------------------------------------------------
;; Etapa 1: Literais, Escapes, IO e Call/Dispatch básico
;; ---------------------------------------------------------------------------

(deftest stage1-hello-literals-escapes-test
  (testing "Hello World e encadeamento IO"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              main() : Object {
                out_string(\"Hello, Cool!\\n\").out_string(\"x=\").out_int(42).out_string(\"\\n\")
              };
            };")]
      (is (= 0 exit))
      (is (= "Hello, Cool!\nx=42\n" stdout))))

  (testing "Unescape completo de StringLit (§10.2)"
    (is (= "a\nb\tc\bd\fe\\f\"g\nhi"
           (types/unescape-cool-string "\"a\\nb\\tc\\bd\\fe\\\\f\\\"g\\\nh\\i\"")))
    (let [{:keys [exit stdout]}
          (compile-and-run
           (str "class Main inherits IO {\n"
                "  main() : Object {\n"
                "    out_string(\"A\\tB\\n\\\"C\\\"\\\\D\\a\\\nE\")\n"
                "  };\n"
                "};"))]
      (is (= 0 exit))
      (is (= "A\tB\n\"C\"\\Da\nE" stdout))))

  (testing "BoolLit case-insensitive"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              main() : Object {
                {
                  if tRuE then out_string(\"T\") else out_string(\"F\") fi;
                  if fAlSe then out_string(\"T\") else out_string(\"F\") fi;
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "TF" stdout)))))

;; ---------------------------------------------------------------------------
;; Etapa 2: Operadores, If, While, Let (shadowing), Assign e Block
;; ---------------------------------------------------------------------------

(deftest stage2-ops-control-flow-scope-test
  (testing "Aritmética, negação aritmética ~ (ineg), comparações, not, isvoid"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              main() : Object {
                {
                  out_int(10 + 5 * 2 - 4 / 2); out_string(\",\");
                  out_int(~5); out_string(\",\");
                  out_int(~~42); out_string(\",\");
                  out_int(~7 / 2); out_string(\",\");
                  if 3 < 4 then out_string(\"1\") else out_string(\"0\") fi;
                  if 4 < 4 then out_string(\"1\") else out_string(\"0\") fi;
                  if 4 <= 4 then out_string(\"1\") else out_string(\"0\") fi;
                  if not false then out_string(\"1\") else out_string(\"0\") fi;
                  if isvoid (while false loop 1 pool) then out_string(\"1\") else out_string(\"0\") fi;
                  if isvoid self then out_string(\"1\") else out_string(\"0\") fi;
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "18,-5,42,-3,101110" stdout))))

  (testing "Fatorial iterativo (while com muitas iterações) e Fibonacci recursivo"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              fact(n : Int) : Int {
                let acc : Int <- 1, i : Int <- 1 in {
                  while i <= n loop {
                    acc <- acc * i;
                    i <- i + 1;
                  } pool;
                  acc;
                }
              };
              fib(n : Int) : Int {
                if n <= 1 then n else fib(n - 1) + fib(n - 2) fi
              };
              main() : Object {
                {
                  out_int(fact(10));
                  out_string(\":\");
                  out_int(fib(10));
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "3628800:55" stdout))))

  (testing "Let com defaults, shadowing aninhado mudando de tipo e Assign encadeado"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              x : Int <- 100;
              test(x : Int) : Object {
                {
                  out_int(x); out_string(\",\");
                  let x : String <- \"inner\", y : Int, s : String, b : Bool, o : Object in {
                    out_string(x); out_string(\",\");
                    out_int(y); out_string(\",\");
                    out_int(s.length()); out_string(\",\");
                    if b then out_string(\"T\") else out_string(\"F\") fi; out_string(\",\");
                    if isvoid o then out_string(\"V\") else out_string(\"N\") fi; out_string(\",\");
                    let a : Int <- 0, c : Int <- 0 in {
                      a <- c <- 77;
                      out_int(a + c); out_string(\",\");
                    };
                  };
                  out_int(x);
                }
              };
              main() : Object {
                {
                  test(50);
                  out_string(\",\");
                  out_int(x);
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "50,inner,0,0,F,V,154,50,100" stdout)))))

;; ---------------------------------------------------------------------------
;; Etapa 3: Objetos, Atributos, Herança em 2 Fases, copy() e Mangling
;; ---------------------------------------------------------------------------

(deftest stage3-lifecycle-inheritance-copy-test
  (testing "Ordem de inicialização de atributos (ancestral -> subclasse, ordem textual)"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A inherits IO {
              a1 : Int <- { out_string(\"A1,\"); 10; };
              a2 : Int <- { out_string(\"A2,\"); a1 + 5; };
            };
            class B inherits A {
              b1 : Int <- { out_string(\"B1,\"); a2 * 2; };
            };
            class C inherits B {
              c1 : Int <- { out_string(\"C1,\"); b1 + 1; };
              val() : Int { c1 };
            };
            class Main inherits IO {
              main() : Object {
                let c : C <- new C in out_int(c.val())
              };
            };")]
      (is (= 0 exit))
      (is (= "A1,A2,B1,C1,31" stdout))))

  (testing "Armadilha de 2 fases: superclasse chama método virtual que lê atributos básicos da subclasse"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A {
              seen_y : Int <- get_y();
              seen_s : String <- get_s();
              seen_b : Bool <- get_b();
              get_y() : Int { 999 };
              get_s() : String { \"A\" };
              get_b() : Bool { true };
              report(io : IO) : Object {
                {
                  io.out_int(seen_y).out_string(\",\");
                  io.out_int(seen_s.length()).out_string(\",\");
                  if seen_b then io.out_string(\"T\") else io.out_string(\"F\") fi;
                }
              };
            };
            class B inherits A {
              y : Int <- 42;
              s : String <- \"hello\";
              b : Bool <- true;
              get_y() : Int { y };
              get_s() : String { s };
              get_b() : Bool { b };
            };
            class Main inherits IO {
              main() : Object {
                let obj : B <- new B in {
                  obj.report(self);
                  out_string(\"|\");
                  out_int(obj.get_y()).out_string(\",\").out_string(obj.get_s());
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "0,0,F|42,hello" stdout))))

  (testing "Object.copy() raso (sem reexecutar cool$$init), new Int/String/Bool, type_name() em subclasse de IO"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Box {
              v : Int <- 1;
              set(x : Int) : Int { v <- x };
              get() : Int { v };
            };
            class Holder inherits IO {
              n : Int <- { out_string(\"INIT,\"); 10; };
              box : Box <- new Box;
              set_n(x : Int) : Int { n <- x };
              get_n() : Int { n };
              get_box() : Box { box };
            };
            class Main inherits IO {
              main() : Object {
                let h1 : Holder <- new Holder,
                    h2 : Holder <- h1.copy() in {
                  h2.set_n(99);
                  h2.get_box().set(777);
                  out_int(h1.get_n()); out_string(\",\");
                  out_int(h2.get_n()); out_string(\",\");
                  out_int(h1.get_box().get()); out_string(\",\");
                  out_string(h1.type_name()); out_string(\",\");
                  out_string((new Int).type_name()); out_string(\",\");
                  out_int(new Int); out_string(\",\");
                  out_int((new String).length()); out_string(\",\");
                  if new Bool then out_string(\"T\") else out_string(\"F\") fi;
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "INIT,10,99,777,Holder,Int,0,0,F" stdout))))

  (testing "Name mangling: métodos/atributos com nomes de java.lang.Object, keywords Java e mesmo nome attr/method"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Collide {
              foo : Int <- 7;
              final : Int <- 3;
              foo() : Int { foo + final };
              clone() : Int { 11 };
              wait() : Int { 22 };
              toString() : String { \"ok\" };
              getClass() : Int { 33 };
            };
            class Main inherits IO {
              main() : Object {
                let c : Collide <- new Collide in
                  out_int(c.foo() + c.clone() + c.wait() + c.getClass())
                    .out_string(c.toString())
              };
            };")]
      (is (= 0 exit))
      (is (= "76ok" stdout)))))

;; ---------------------------------------------------------------------------
;; Etapa 4: Polimorfismo, Despacho Estático (@Tipo), SELF_TYPE e Ordem de Avaliação
;; ---------------------------------------------------------------------------

(deftest stage4-dispatch-static-self-type-test
  (testing "Despacho estático a partir de classe não relacionada + método herdado + básico"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A {
              id() : Int { 1 };
              only_a() : Int { 100 };
            };
            class B inherits A {
              id() : Int { 2 };
            };
            class C inherits B {
              id() : Int { 3 };
            };
            class Main inherits IO {
              main() : Object {
                let c : C <- new C in {
                  out_int(c@A.id()); out_string(\",\");
                  out_int(c@B.id()); out_string(\",\");
                  out_int(c.id()); out_string(\",\");
                  out_int(c@B.only_a()); out_string(\",\");
                  out_string(\"hi\"@Object.type_name());
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "1,2,3,100,String" stdout))))

  (testing "SELF_TYPE em retorno (override polimórfico), atributo herdado (inclusive void) e new SELF_TYPE"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Base {
              link : SELF_TYPE;
              tag() : String { \"Base\" };
              make() : SELF_TYPE { new SELF_TYPE };
              me() : SELF_TYPE { self };
              init_link() : SELF_TYPE { { link <- new SELF_TYPE; self; } };
              get_link() : SELF_TYPE { link };
            };
            class Sub inherits Base {
              v : Int <- 55;
              tag() : String { \"Sub\" };
              me() : SELF_TYPE { { v <- v + 1; self; } };
              get_v() : Int { v };
              link_v() : Int { link.get_v() };
            };
            class Main inherits IO {
              main() : Object {
                let s1 : Sub <- new Sub,
                    was_void : Bool <- isvoid s1.get_link(),
                    s2 : Sub <- s1.make(),
                    poly : Base <- s1,
                    s3 : Base <- poly.me() in {
                  if was_void then out_string(\"V,\") else out_string(\"N,\") fi;
                  out_string(s2.type_name()); out_string(\",\");
                  out_int(s2.get_v()); out_string(\",\");
                  out_int(s1.get_v()); out_string(\",\");
                  s1.init_link();
                  out_int(s1.get_link().get_v()); out_string(\",\");
                  out_int(s1.link_v()); out_string(\",\");
                  out_string(s3.tag());
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "V,Sub,55,56,55,55,Sub" stdout))))

  (testing "Ordem de avaliação estrita no dispatch: e1..en ANTES de e0 (§13.3)"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Target {
              f(a : Int, b : Int) : Int { a + b };
            };
            class Main inherits IO {
              main() : Object {
                let res : Int <-
                  ({ out_string(\"0\"); new Target; })
                    .f({ out_string(\"1\"); 10; },
                       { out_string(\"2\"); 20; }) in
                {
                  out_string(\":\");
                  out_int(res);
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "120:30" stdout)))))

;; ---------------------------------------------------------------------------
;; Etapa 5: Case, Igualdade Dinâmica (=), IO stdin, String.substr e 5 Erros de Runtime
;; ---------------------------------------------------------------------------

(deftest stage5-case-equality-io-string-test
  (testing "Case selecionando ancestral mais próximo com ramos fora de ordem"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A {};
            class B inherits A {};
            class C inherits B {};
            class D inherits C { d_val() : Int { 42 }; };
            class E inherits A {};
            class Main inherits IO {
              classify(x : Object) : String {
                case x of
                  o : Object => \"Object\";
                  a : A => \"A\";
                  e : E => \"E\";
                  c : C => \"C\";
                  b : B => \"B\";
                  d : D => { out_int(d.d_val()); \"D\"; };
                  i : Int => \"Int\";
                  s : String => \"String\";
                  bl : Bool => \"Bool\";
                esac
              };
              main() : Object {
                {
                  out_string(classify(new D)); out_string(\",\");
                  out_string(classify(new C)); out_string(\",\");
                  out_string(classify(new B)); out_string(\",\");
                  out_string(classify(new A)); out_string(\",\");
                  out_string(classify(new E)); out_string(\",\");
                  out_string(classify(5)); out_string(\",\");
                  out_string(classify(\"x\")); out_string(\",\");
                  out_string(classify(true)); out_string(\",\");
                  out_string(classify(new IO));
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "42D,C,B,A,E,Int,String,Bool,Object" stdout))))

  (testing "Igualdade '=' dinâmica para Int/String/Bool via Object, void = void, e objetos"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A {};
            class Main inherits IO {
              bstr(b : Bool) : String { if b then \"1\" else \"0\" fi };
              main() : Object {
                let i1 : Object <- 5000,
                    i2 : Object <- 2500 + 2500,
                    s1 : Object <- \"ab\".concat(\"c\"),
                    s2 : Object <- \"abc\",
                    b1 : Object <- true,
                    b2 : Object <- not false,
                    v1 : A,
                    v2 : Object,
                    a1 : A <- new A,
                    a2 : A <- a1.copy() in
                {
                  out_string(bstr(i1 = i2));
                  out_string(bstr(s1 = s2));
                  out_string(bstr(b1 = b2));
                  out_string(bstr(i1 = s1));
                  out_string(bstr(v1 = v2));
                  out_string(bstr(v1 = a1));
                  out_string(bstr(a1 = a1));
                  out_string(bstr(a1 = a2));
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "11101010" stdout))))

  (testing "IO.in_int (§8.2), IO.in_string e String.substr/concat/length"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              main() : Object {
                let a : Int <- in_int(),
                    b : Int <- in_int(),
                    c : Int <- in_int(),
                    s : String <- in_string() in
                {
                  out_int(a + b + c);
                  out_string(\":\");
                  out_string(s.substr(5, 4));
                  out_string(\":\");
                  out_int(s.length());
                }
              };
            };"
           "  42abc\n  -7xyz\n  nope\nCool Lang!\n")]
      (is (= 0 exit))
      (is (= "35:Lang:10" stdout)))))

(deftest stage5-runtime-errors-test
  (testing "1a. Dispatch dinâmico em void (avalia argumentos antes de abortar!)"
    (let [{:keys [exit stdout stderr]}
          (compile-and-run
           "class A { f(x : Int) : Int { x }; };
            class Main inherits IO {
              main() : Object {
                let a : A in a.f({ out_string(\"ARG\"); 1; })
              };
            };")]
      (is (= 1 exit))
      (is (= "ARG" stdout))
      (is (str/includes? stderr "Dispatch to void"))))

  (testing "1b. Dispatch estático em void"
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class A { f() : Int { 1 }; };
            class Main {
              main() : Object {
                let a : A in a@A.f()
              };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "Dispatch to void"))))

  (testing "2. Case em void"
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class Main {
              main() : Object {
                let x : Object in
                  case x of o : Object => 1; esac
              };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "Match on void"))))

  (testing "3. Case sem ramo compatível"
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class A {};
            class B {};
            class Main {
              main() : Object {
                let x : Object <- new B in
                  case x of a : A => 1; esac
              };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "No match in case"))
      (is (str/includes? stderr "B"))))

  (testing "4. Divisão por zero"
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class Main {
              main() : Int { 10 / (3 - 3) };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "Division by zero"))))

  (testing "5. String.substr fora do intervalo (negativo e overflow)"
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class Main {
              main() : String { \"abc\".substr(2, 2) };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "substr")))
    (let [{:keys [exit stderr]}
          (compile-and-run
           "class Main {
              main() : String { \"abc\".substr(~1, 1) };
            };")]
      (is (= 1 exit))
      (is (str/includes? stderr "substr"))))

  (testing "6. Object.abort()"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main {
              main() : Object { abort() };
            };")]
      (is (= 1 exit))
      (is (str/includes? stdout "Abort called from class Main")))))

;; ---------------------------------------------------------------------------
;; Etapa 6: Programas Cool Completos Não-Triviais e Arquivos Reais de tests/
;; ---------------------------------------------------------------------------

(deftest stage6-complete-cool-programs-test
  (testing "Lista ligada polimórfica (List / Nil / Cons) com reverse, sum, length e case"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class List {
              isNil() : Bool { true };
              head()  : Int  { { abort(); 0; } };
              tail()  : List { { abort(); self; } };
              cons(i : Int) : List { (new Cons).init(i, self) };
              length() : Int { 0 };
              sum() : Int { 0 };
              reverse_acc(acc : List) : List { acc };
              reverse() : List { reverse_acc(new Nil) };
              print(io : IO) : Object { io.out_string(\"[]\") };
            };

            class Nil inherits List {};

            class Cons inherits List {
              car : Int;
              cdr : List;
              isNil() : Bool { false };
              head()  : Int  { car };
              tail()  : List { cdr };
              init(i : Int, rest : List) : List {
                {
                  car <- i;
                  cdr <- rest;
                  self;
                }
              };
              length() : Int { 1 + cdr.length() };
              sum() : Int { car + cdr.sum() };
              reverse_acc(acc : List) : List {
                cdr.reverse_acc(acc.cons(car))
              };
              print(io : IO) : Object {
                {
                  io.out_int(car);
                  case cdr of
                    n : Nil => io.out_string(\"!\");
                    c : Cons => { io.out_string(\"->\"); c.print(io); };
                  esac;
                }
              };
            };

            class Main inherits IO {
              main() : Object {
                let l : List <- (new Nil).cons(40).cons(30).cons(20).cons(10),
                    r : List <- l.reverse() in
                {
                  l.print(self);
                  out_string(\"|\");
                  r.print(self);
                  out_string(\"|len=\");
                  out_int(l.length());
                  out_string(\",sum=\");
                  out_int(l.sum());
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "10->20->30->40!|40->30->20->10!|len=4,sum=100" stdout))))

  (testing "Árvore Binária de Busca (Tree / Empty / Node) com inserção e travessia in-order"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Tree {
              insert(x : Int) : Tree { (new Node).init(x, new Empty, new Empty) };
              contains(x : Int) : Bool { false };
              inorder(io : IO) : Object { self };
            };

            class Empty inherits Tree {};

            class Node inherits Tree {
              val : Int;
              left : Tree;
              right : Tree;
              init(v : Int, l : Tree, r : Tree) : Node {
                { val <- v; left <- l; right <- r; self; }
              };
              insert(x : Int) : Tree {
                if x < val then
                  { left <- left.insert(x); self; }
                else if val < x then
                  { right <- right.insert(x); self; }
                else
                  self
                fi fi
              };
              contains(x : Int) : Bool {
                if x = val then true
                else if x < val then left.contains(x)
                else right.contains(x)
                fi fi
              };
              inorder(io : IO) : Object {
                {
                  left.inorder(io);
                  io.out_int(val).out_string(\" \");
                  right.inorder(io);
                }
              };
            };

            class Main inherits IO {
              main() : Object {
                let t : Tree <- (new Empty).insert(5).insert(2).insert(8).insert(1).insert(3).insert(7) in
                {
                  t.inorder(self);
                  if t.contains(3) then out_string(\"Y\") else out_string(\"N\") fi;
                  if t.contains(6) then out_string(\"Y\") else out_string(\"N\") fi;
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "1 2 3 5 7 8 YN" stdout))))

  (testing "Compilação e execução dos arquivos válidos existentes em test/fixtures/"
    (let [{:keys [exit stdout]} (compile-file-and-run "test/fixtures/exprs.cl")]
      (is (= 0 exit))
      (is (= "Menor\n" stdout)))
    (let [{:keys [exit stdout]} (compile-file-and-run "test/fixtures/stress_self_type.cl")]
      (is (= 0 exit))
      (is (= "ok\n" stdout)))
    (let [{:keys [exit]} (compile-file-and-run "test/fixtures/stress_inheritance_case.cl")]
      (is (= 0 exit)))
    (let [{:keys [exit]} (compile-file-and-run "test/fixtures/class-inherits.cl")]
      (is (= 0 exit)))))

(deftest audit-corner-cases-test
  (testing "Main.main() retornando SELF_TYPE e ~2147483648 (Integer.MIN_VALUE)"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class Main inherits IO {
              main() : SELF_TYPE {
                out_int(~2147483648).out_string(\",\").out_int(2147483647)
              };
            };")]
      (is (= 0 exit))
      (is (= "-2147483648,2147483647" stdout))))

  (testing "Frame merge com NULL, back-edge de while com irmãos B/C e static dispatch SELF_TYPE"
    (let [{:keys [exit stdout]}
          (compile-and-run
           "class A {
              f() : Int { 1 };
              me() : SELF_TYPE { self };
            };
            class B inherits A {
              f() : Int { 2 };
              extra() : Int { 99 };
              call_super_self() : Int { (self@A.me()).extra() };
            };
            class C inherits A {
              f() : Int { 3 };
            };
            class Main inherits IO {
              main() : Object {
                let uninit : A,
                    x : A <- new B,
                    i : Int <- 0,
                    b : B <- new B in
                {
                  out_int(if false then uninit.f() else 7 fi); out_string(\",\");
                  while i < 1 loop {
                    x <- new C;
                    i <- i + 1;
                  } pool;
                  out_int(x.f()); out_string(\",\");
                  out_int(b.call_super_self()); out_string(\",\");
                  out_int((b@A.me()).extra());
                }
              };
            };")]
      (is (= 0 exit))
      (is (= "7,3,99,99" stdout))))

  (testing "Recompilar dois programas diferentes no mesmo diretório de saída não falha na verificação"
    (let [tmp-dir (.toFile (Files/createTempDirectory "cool-reuse-" (into-array FileAttribute [])))]
      (try
        (codegen/compile-source!
         "class OldHelper {};
          class Main inherits IO { main() : Object { out_string(\"first\") }; };"
         tmp-dir "p1.cl" {:verify? true})
        (codegen/compile-source!
         "class Main inherits IO { main() : Object { out_string(\"second\") }; };"
         tmp-dir "p2.cl" {:verify? true})
        (let [{:keys [exit stdout]} (run-jvm-main tmp-dir nil)]
          (is (= 0 exit))
          (is (= "second" stdout)))
        (finally
          (delete-recursively! tmp-dir))))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'codegen-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
