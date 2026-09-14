(ns parser
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [lex :as lex])
  (:gen-class))

(defn accept 
  [tokens expected-tag]
  (let [[[_ tag] & resto] tokens]
    (if (= tag expected-tag)
      [true resto]
      [false tokens])))

(defn expect 
  [tokens expected]
  (let [[[val tag row col] & resto] tokens
        match? (if (set? expected) 
                 (expected tag) 
                 (= tag expected))]
    (if match?
      [val resto]
      (throw (ex-info (str "Erro sintático na linha " row ", coluna " col 
                           ": esperava " \" expected \" " mas encontrou " \" val \")
                      {:expected expected :found val :row row :col col})))))

(defn skip [tokens expected-tag]
  (let [[_ tail] (expect tokens expected-tag)]
    tail))

(defn skip-expr-temporario [tokens stop-tag]
  (loop [toks tokens]
    (let [[_ tag] (first toks)]
      (cond
        (= tag stop-tag) toks
        :else (recur (rest toks))))))

(defn parse-formal [tokens] 
  (let [[id tokens]         (expect tokens :object-identifier)
        tokens              (skip tokens :colon) 
        [type-decl tokens]  (expect tokens #{:type-identifier :self-type-token})]
    [{:type :formal
      :name id
      :declared-type type-decl}
     tokens]))

(defn parse-feature [tokens]
  (let [[name tokens] (expect tokens :object-identifier)
        [_ next-tag]  (first tokens)]
    (cond
      (= next-tag :leftparentheses)
      (let [tokens               (skip tokens :leftparentheses)
            tokens               (skip-expr-temporario tokens :rightparentheses)
            tokens               (skip tokens :rightparentheses)
            tokens               (skip tokens :colon)
           [return-type tokens]  (expect tokens #{:type-identifier :self-type-token}) 
            tokens               (skip tokens :leftbracket)
            tokens               (skip-expr-temporario tokens :rightbracket)
            tokens               (skip tokens :rightbracket)
            tokens               (skip tokens :semicolon)]
        [{:type :method :name name :return-type return-type :formals [] :body []} tokens])
      (= next-tag :colon)
      (let [tokens             (skip tokens :colon)
            [attr-type tokens] (expect tokens #{:type-identifier :self-type-token})
            tokens             (skip-expr-temporario tokens :semicolon)
            tokens             (skip tokens :semicolon)]
        [{:type :attribute :name name :declared-type attr-type} tokens])

      :else
      (throw (ex-info (str "Erro sintático dentro de parse-feature") {})))))

(defn parse-class [tokens]
  (let [tokens                 (skip tokens :class)
        [class-name tokens]    (expect tokens :type-identifier)
        
        [has-inherits? tokens] (accept tokens :inherits)
        [parent tokens]        (if has-inherits?
                                 (expect tokens :type-identifier)
                                 ["Object" tokens])
                                 
        tokens                 (skip tokens :leftbracket)
        
        [features tokens]      (loop [curr-tokens tokens
                                      acc-features []]
                                 (let [[is-end? _] (accept curr-tokens :rightbracket)]
                                   (if is-end?
                                     [acc-features curr-tokens]
                                     (let [[feat next-tokens] (parse-feature curr-tokens)]
                                       (recur next-tokens (conj acc-features feat))))))
                                       
        tokens                 (skip tokens :rightbracket)
        tokens                 (skip tokens :semicolon)]
    
    [{:type :class
      :name class-name
      :inherits parent
      :features features}
     tokens]))

(defn lex-all
  [filename] 
  (loop [state (lex/init-state (slurp filename))
         tokens []]
      (let [result (lex/lex state)]
          (if result 
            (let [[next-state token] result]
              (recur next-state (conj tokens token)))
            tokens))))

(defn -main [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: java -jar ./parser <file.cl>")
      (let [tokens (lex-all filename)
            [ast _] (parse-class tokens)]
        (pprint/pprint ast)))))
