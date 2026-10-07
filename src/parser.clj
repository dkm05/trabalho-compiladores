(ns parser
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
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
    [{:type :formal 
      :name id 
      :declared-type type-id 
      :row row 
      :col col} tokens]))

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
        [{:type :method 
          :name name 
          :return-type return-type 
          :formals formals 
          :body body 
          :row row 
          :col col} tokens])
      (= next-tag :colon)
      (let [tokens               (skip tokens :colon)
            [attr-type tokens]   (expect tokens #{:type-identifier :self-type-token})
            [has-assign? tokens] (accept tokens :assign)
            [init tokens]        (if has-assign? (parse-expr tokens) [nil tokens])]
        [{:type :attribute 
          :name name 
          :declared-type attr-type 
          :init init 
          :row row 
          :col col} tokens])

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

    [{:type :class
      :name class-name
      :inherits parent
      :features features
      :row row
      :col col}
     tokens]))

; -------------------- expr --------------------

(defn parse-literal
  [tokens]
  (let [[val tag row col] (first tokens)]
    [{:type tag :value val :row row :col col} (rest tokens)]))

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
    [{:type :if
      :cond condi
      :then branch-t
      :else branch-f
      :row row
      :col col}
     tokens]))

(defn parse-while
  [tokens]
  (let [[row col]           (peek-pos tokens)
        tokens              (skip tokens :while)
        [condi tokens]       (parse-expr tokens)
        tokens              (skip tokens :loop)
        [loop-body tokens]   (parse-expr tokens)
        tokens              (skip tokens :pool)]
    [{:type :while
      :cond condi
      :body loop-body
      :row row
      :col col}
     tokens]))

(defn parse-block [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :leftbrace)
        [exprs tokens] (parse-semicolon-terminated tokens parse-expr :rightbrace)
        tokens    (skip tokens :rightbrace)]
    [{:type :block 
      :body exprs 
      :row row 
      :col col} tokens]))

(defn parse-binding [tokens]
  (let [[id type-id curr] (parse-type-id tokens)
        [has-init? curr]  (accept curr :assign)
        [init curr]       (if has-init? (parse-expr curr) [nil curr])]
    [{:type type-id 
      :id id 
      :init init} curr]))

(defn parse-let [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :let)
        [bindings tokens] (parse-comma-separated tokens parse-binding :in)
        tokens    (skip tokens :in)
        [body tokens] (parse-expr tokens)]
    [{:type :let 
      :bindings bindings 
      :body body 
      :row row 
      :col col} tokens]))

(defn parse-case-branch [tokens]
  (let [[id type-id curr]  (parse-type-id tokens)
        curr               (skip curr :to)
        [branch-expr curr] (parse-expr curr)]
    [{:type type-id 
      :id id 
      :expr branch-expr} curr]))

(defn parse-case [tokens]
  (let [[row col] (peek-pos tokens)
        tokens    (skip tokens :case)
        [expr tokens] (parse-expr tokens)
        tokens    (skip tokens :of)
        [branches tokens] (parse-semicolon-terminated tokens parse-case-branch :esac)
        tokens    (skip tokens :esac)]
    [{:type :case 
      :expr expr 
      :cases branches 
      :row row 
      :col col} tokens]))

(defn parse-id-or-call [tokens]
  (let [[row col] (peek-pos tokens)
        [id tokens] (expect tokens :object-identifier)]
    (if (= (peek-tag tokens) :leftparentheses)
      (let [tokens (skip tokens :leftparentheses)
            [args tokens] (parse-comma-separated tokens parse-expr :rightparentheses)
            tokens (skip tokens :rightparentheses)]
        [{:type :call :method id :args args :row row :col col} tokens])
      [{:type :id 
        :name id 
        :row row 
        :col col} tokens])))

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
        [{:type :id 
          :name "self" 
          :row row 
          :col col} tokens])
      (= tag :object-identifier) (parse-id-or-call tokens)
      (= tag :new)
      (let [[row col] (peek-pos tokens)
            tokens (skip tokens :new)
            [type-id tokens] (expect tokens :type-identifier)]
        [{:type :new 
          :type-id type-id 
          :row row 
          :col col} tokens])
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
                base-node             {:type :dispatch 
                                       :obj curr-left 
                                       :method method-id 
                                       :args args 
                                       :row (:row curr-left) 
                                       :col (:col curr-left)}
                dispatch-node         (if has-at? (assoc base-node :static-type type-name) base-node)]
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
            (recur {:type :binary-op 
                    :op op 
                    :left curr-left 
                    :right right 
                    :row (:row curr-left) 
                    :col (:col curr-left)} curr-toks))
          [curr-left curr-toks])))))

(defn parse-unary [tokens expected-tag next-layer-fn current-layer-fn]
  (if (= (peek-tag tokens) expected-tag)
    (let [[row col] (peek-pos tokens)
          tokens        (skip tokens expected-tag)
          [expr tokens] (current-layer-fn tokens)]
      [{:type expected-tag 
        :expr expr 
        :row row 
        :col col} tokens])
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
        [{:type :binary-op 
          :op op 
          :left left 
          :right right 
          :row (:row left) 
          :col (:col left)} tokens])
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
        [{:type :assign 
          :name id 
          :expr expr 
          :row row 
          :col col} tokens])
      (parse-not tokens))))

(defn parse-program [tokens]
  (let [[classes next-toks] (parse-semicolon-terminated tokens parse-class :eof)]
    [{:type :program 
      :classes classes} next-toks]))

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
