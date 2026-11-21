(ns parser
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [ast :as ast]
            [lex :as lex])
  (:gen-class))

(declare parse-expr)

(defn peek-tag [tokens]
  (second (first tokens)))

(defn peek-pos [tokens]
  (let [[_ _ row col] (first tokens)]
    [row col]))

(defn accept
  [tokens expected-tag]
  (if (= (peek-tag tokens) expected-tag)
    [true (rest tokens)]
    [false tokens]))

(defn expect
  [tokens expected-tag]
  (let [[[val tag row col] & tail] tokens
        match? (if (set? expected-tag)
                 (expected-tag tag)
                 (= tag expected-tag))]
    (if match?
      [val tail]
      (throw (ex-info (str "Erro sintático na linha " row ", coluna " col
                           ": esperava " \" expected-tag \" " mas encontrou " \" val \")
                      {:expected expected-tag :found val :row row :col col})))))

(defn skip [tokens expected-tag]
  (let [[_ tail] (expect tokens expected-tag)]
    tail))

(defn parse-comma-separated [tokens parser-fn end-tag]
  (if (= (peek-tag tokens) end-tag)
    [[] tokens]
    (loop [curr tokens, acc []]
      (let [[item next-toks] (parser-fn curr)]
        (if (= (peek-tag next-toks) :comma)
          (recur (skip next-toks :comma) (conj acc item))
          [(conj acc item) next-toks])))))

(defn parse-semicolon-terminated [tokens parser-fn end-tag]
  (if (= (peek-tag tokens) end-tag)
    [[] tokens]
    (loop [curr tokens, acc []]
      (let [[item next-toks] (parser-fn curr)
            next-toks        (skip next-toks :semicolon)]
        (if (= (peek-tag next-toks) end-tag)
          [(conj acc item) next-toks]
          (recur next-toks (conj acc item)))))))

(defn parse-type-id [tokens]
  (let [[id curr]      (expect tokens :object-identifier)
        curr           (skip curr :colon)
        [type-id curr] (expect curr #{:type-identifier :self-type-token})]
    [id type-id curr]))

(defn parse-formal [tokens]
  (let [[row col] (peek-pos tokens)
        [id type-id tokens] (parse-type-id tokens)]
    [(ast/->Formal id type-id row col) tokens]))

(defn parse-feature [tokens]
  (let [[row col]     (peek-pos tokens)
        [name tokens] (expect tokens :object-identifier)
        next-tag      (peek-tag tokens)]
    (cond
      (= next-tag :leftparentheses)
      (let [tokens               (skip tokens :leftparentheses)
            [formals tokens]     (parse-comma-separated tokens parse-formal :rightparentheses)
            tokens               (skip tokens :rightparentheses)
            tokens               (skip tokens :colon)
            [return-type tokens] (expect tokens #{:type-identifier :self-type-token})
            tokens               (skip tokens :leftbrace)
            [body tokens]        (parse-expr tokens)
            tokens               (skip tokens :rightbrace)]
        [(ast/->Method name return-type formals body row col) tokens])
      (= next-tag :colon)
      (let [tokens               (skip tokens :colon)
            [attr-type tokens]   (expect tokens #{:type-identifier :self-type-token})
            [has-assign? tokens] (accept tokens :assign)
            [init tokens]        (if has-assign? (parse-expr tokens) [nil tokens])]
        [(ast/->Attribute name attr-type init row col) tokens])

      :else
      (throw (ex-info (str "Erro sintático dentro de parse-feature") {})))))

(defn parse-class [tokens]
  (let [[row col]              (peek-pos tokens)
        tokens                 (skip tokens :class)
        [class-name tokens]    (expect tokens :type-identifier)
        [has-inherits? tokens] (accept tokens :inherits)
        [parent tokens]        (if has-inherits?
                                 (expect tokens :type-identifier)
                                 ["Object" tokens])
        tokens                 (skip tokens :leftbrace)
        [features tokens]      (parse-semicolon-terminated tokens parse-feature :rightbrace)
        tokens                 (skip tokens :rightbrace)]

    [(ast/->ClassDef class-name parent features row col) tokens]))

; -------------------- expr --------------------

(defn parse-literal [tokens]
  (let [[val tag row col] (first tokens)
        node (case tag
               :integer (ast/->IntLit val row col)
               :string  (ast/->StringLit val row col)
               :boolean (ast/->BoolLit val row col))]
    [node (rest tokens)]))

(defn parse-if
  [tokens]
  (let [[row col]           (peek-pos tokens)
        tokens              (skip tokens :if)
        [condi tokens]      (parse-expr tokens)
        tokens              (skip tokens :then)
        [branch-t tokens]   (parse-expr tokens)
        tokens              (skip tokens :else)
        [branch-f tokens]   (parse-expr tokens)
        tokens              (skip tokens :fi)]
    [(ast/->If condi branch-t branch-f row col) tokens]))

(defn parse-while
  [tokens]
  (let [[row col]           (peek-pos tokens)
        tokens              (skip tokens :while)
        [condi tokens]       (parse-expr tokens)
        tokens              (skip tokens :loop)
        [loop-body tokens]   (parse-expr tokens)
        tokens              (skip tokens :pool)]
    [(ast/->While condi loop-body row col) tokens]))

(defn parse-block [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :leftbrace)
        [exprs tokens] (parse-semicolon-terminated tokens parse-expr :rightbrace)
        tokens    (skip tokens :rightbrace)]
    [(ast/->Block exprs row col) tokens]))

(defn parse-binding [tokens]
  (let [[row col]         (peek-pos tokens)
        [id type-id curr] (parse-type-id tokens)
        [has-init? curr]  (accept curr :assign)
        [init curr]       (if has-init? (parse-expr curr) [nil curr])]
    [(ast/->Binding id type-id init row col) curr]))

(defn parse-let [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :let)
        [bindings tokens] (parse-comma-separated tokens parse-binding :in)
        tokens    (skip tokens :in)
        [body tokens] (parse-expr tokens)]
    [(ast/->Let bindings body row col) tokens]))

(defn parse-case-branch [tokens]
  (let [[row col]          (peek-pos tokens)
        [id type-id curr]  (parse-type-id tokens)
        curr               (skip curr :to)
        [branch-expr curr] (parse-expr curr)]
    [(ast/->CaseBranch id type-id branch-expr row col) curr]))

(defn parse-case [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :case)
        [expr tokens] (parse-expr tokens)
        tokens    (skip tokens :of)
        [branches tokens] (parse-semicolon-terminated tokens parse-case-branch :esac)
        tokens    (skip tokens :esac)]
    [(ast/->Case expr branches row col) tokens]))

(defn parse-id-or-call [tokens]
  (let [[row col] (peek-pos tokens)
        [id tokens] (expect tokens :object-identifier)]
    (if (= (peek-tag tokens) :leftparentheses)
      (let [tokens (skip tokens :leftparentheses)
            [args tokens] (parse-comma-separated tokens parse-expr :rightparentheses)
            tokens (skip tokens :rightparentheses)]
        [(ast/->Call id args row col) tokens])
      [(ast/->Id id row col) tokens])))

(defn parse-primary [tokens]
  (let [tag (peek-tag tokens)]
    (cond
      (#{:integer :string :boolean} tag) (parse-literal tokens)
      (= tag :if)         (parse-if tokens)
      (= tag :while)      (parse-while tokens)
      (= tag :leftbrace)  (parse-block tokens)
      (= tag :let)        (parse-let tokens)
      (= tag :case)       (parse-case tokens)
      (= tag :self-token)
      (let [[row col] (peek-pos tokens)
            tokens (skip tokens :self-token)]
        [(ast/->Id "self" row col) tokens])
      (= tag :object-identifier) (parse-id-or-call tokens)
      (= tag :new)
      (let [[row col] (peek-pos tokens)
            tokens (skip tokens :new)
            [type-id tokens] (expect tokens #{:type-identifier :self-type-token})]
        [(ast/->New type-id row col) tokens])
      (= tag :leftparentheses)
      (let [tokens (skip tokens :leftparentheses)
            [expr tokens] (parse-expr tokens)
            tokens (skip tokens :rightparentheses)]
        [expr tokens])
      :else
      (let [[val _ row col] (first tokens)]
        (throw (ex-info (str "Token inesperado na linha " row ": " val) {}))))))

(defn parse-dispatch [tokens]
  (let [[left tokens] (parse-primary tokens)]
    (loop [curr-left left, curr-toks tokens]
      (let [tag (peek-tag curr-toks)]
        (if (#{:dot :at} tag)
          (let [[has-at? curr-toks]   (accept curr-toks :at)
                [type-name curr-toks] (if has-at? (expect curr-toks :type-identifier) [nil curr-toks])
                curr-toks             (skip curr-toks :dot)
                [method-id curr-toks] (expect curr-toks :object-identifier)
                curr-toks             (skip curr-toks :leftparentheses)
                [args curr-toks]      (parse-comma-separated curr-toks parse-expr :rightparentheses)
                curr-toks             (skip curr-toks :rightparentheses)
                dispatch-node         (ast/->Dispatch curr-left
                                                      type-name
                                                      method-id
                                                      args
                                                      (:row curr-left)
                                                      (:col curr-left))]
            (recur dispatch-node curr-toks))
          [curr-left curr-toks])))))

(defn parse-binary-left [tokens operator-set next-parser-fn]
  (let [[left tokens] (next-parser-fn tokens)]
    (loop [curr-left left, curr-toks tokens]
      (let [tag (peek-tag curr-toks)]
        (if (operator-set tag)
          (let [op tag
                curr-toks (rest curr-toks)
                [right curr-toks] (next-parser-fn curr-toks)]
            (recur (ast/->BinaryOp op curr-left right (:row curr-left) (:col curr-left)) curr-toks))
          [curr-left curr-toks])))))

(defn parse-unary [tokens expected-tag next-layer-fn current-layer-fn]
  (if (= (peek-tag tokens) expected-tag)
    (let [[row col] (peek-pos tokens)
          tokens        (skip tokens expected-tag)
          [expr tokens] (current-layer-fn tokens)]
      [(ast/->UnaryOp expected-tag expr row col) tokens])
    (next-layer-fn tokens)))

(defn parse-tilde [tokens]
  (parse-unary tokens :intcomplement parse-dispatch parse-tilde))

(defn parse-isvoid [tokens]
  (parse-unary tokens :isvoid parse-tilde parse-isvoid))

(defn parse-mult-div [tokens]
  (parse-binary-left tokens #{:mult :div} parse-isvoid))

(defn parse-add-sub [tokens]
  (parse-binary-left tokens #{:sum :sub} parse-mult-div))

(defn parse-comp [tokens]
  (let [[left tokens] (parse-add-sub tokens)
        tag (peek-tag tokens)]
    (if (#{:lt :leq :eq} tag)
      (let [op tag
            tokens (rest tokens)
            [right tokens] (parse-add-sub tokens)]
        [(ast/->BinaryOp op left right (:row left) (:col left)) tokens])
      [left tokens])))

(defn parse-not [tokens]
  (parse-unary tokens :not parse-comp parse-not))

(defn parse-expr [tokens]
  (let [tag1 (peek-tag tokens)
        tag2 (peek-tag (rest tokens))]
    (if (and (= tag1 :object-identifier) (= tag2 :assign))
      (let [[row col] (peek-pos tokens)
            [id tokens] (expect tokens :object-identifier)
            tokens (skip tokens :assign)
            [expr tokens] (parse-expr tokens)]
        [(ast/->Assign id expr row col) tokens])
      (parse-not tokens))))

(defn parse-program [tokens]
  (let [[classes next-toks] (parse-semicolon-terminated tokens parse-class :eof)]
    [(ast/->Program classes) next-toks]))

(defn lex-all
  [filename]
  (loop [state (lex/init-state (slurp filename))
         tokens []]
    (let [result (lex/lex state)]
      (if result
        (let [[next-state token] result]
          (recur next-state (conj tokens token)))
        (conj tokens ["EOF" :eof -1 -1])))))

(defn get-ast
  [filename]
  (let [tokens  (lex-all filename)
        [ast _] (parse-program tokens)]
    ast))
