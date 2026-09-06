(ns lex
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def keyword-table
  #{"class" "else" "fi" "if" "in" "inherits" "isvoid"
    "let" "loop" "pool" "then" "while" "case" "esac"
    "new" "of" "not"})

(def single-char-ops
  {\+ :sum
   \- :sub
   \~ :intcomplement
   \* :mult
   \< :lt
   \= :eq
   \/ :div
   \. :dot
   \@ :at
   \: :colon
   \; :semicolon
   \{ :leftbracket
   \} :rightbracket
   \( :leftparentheses
   \) :rightparentheses
   \, :comma})

(def whitespace-chars
  #{\space \newline \formfeed \return \tab \v})

(defn quote? [c] (= c \"))

(defn whitespace?
  [c]
  (boolean (whitespace-chars c)))

(defn valid-first-char?
  [c]
  (and c (or (Character/isLetter c) (= c \_))))

(defn valid-body-char?
  [c]
  (and c (or (valid-first-char? c) (Character/isDigit c))))

(defn operator?
  [c]
  (contains? single-char-ops c))

(defn multiline-comment?
  [[c1 c2]]
  (and (= c1 \() (= c2 \*)))

(defn singleline-comment?
  [[c1 c2]]
  (and (= c1 \-) (= c2 \-)))

(defn is-keyword?
  [word]
  (contains? keyword-table word))

(defn die
  [message]
  (println message))

(defn next-char
  [state] 
  (let [[buf row col] (vals state)]
    (if (= (first buf) \newline)
      [(first buf) {:buf (rest buf) :row (+ row 1) :col 1}]
      [(first buf) {:buf (rest buf) :row row :col (+ col 1)}])))

(defn peek-char [state] (first (:buf state)))

(defn classify-word
  [word]
  (let [low-word (str/lower-case word)]
    (cond
      (= word "self")                                :self-token
      (= word "SELF_TYPE")                           :self-type-token
      (keyword-table low-word)                       :keyword
      (and (= (first word) \t) (= low-word "true"))  :boolean
      (and (= (first word) \f) (= low-word "false")) :boolean
      (Character/isUpperCase (first word))           :type-identifier
      :else                                          :object-identifier)))

(defn get-token
  ([state]
   (get-token state ""))
  ([state word]
   (if (valid-body-char? (peek-char state))
     (let [[c new-state] (next-char state)]
       (recur new-state (str word c)))
     (let [tag (classify-word word)]
       [state [word tag]]))))

(defn ignore-singleline-comment
  [state]
  (let [c (peek-char state)]
    (if (or (= c \newline) (nil? c))
      (second (next-char state))
      (recur (second (next-char state))))))

(defn ignore-multiline-comment
  ([state]
   (ignore-multiline-comment state '()))
  ([state stack]
   (let [[c1 new-state] (next-char state)
          c2            (peek-char state)]
     (cond
       (nil? c1)
       (die "erro: comentário multilinha não foi fechado")
       (= (str c1 c2) "(*")
       (recur (second (next-char state)) (cons \( stack))
       (= (str c1 c2) "*)")
       (if (empty? (rest stack))
         new-state
         (recur (second (next-char state)) (rest stack)))
       :else
       (recur new-state stack)))))

; para testes:
; (get-string '(\" \o \l \a \backspace \m \u \n \d \o))
; (get-string '(\" \tab \o \l \a \backspace \\ \newline \m \u \n \d \o \"))
; (get-string '(\" \o \l \a \newline \m \u \n \d \o \"))
; (get-string '(\" \o \l \a \backspace \\ \newline \m \u \n \d \o))
; TODO: verificar se a string tem EOF e \0 (de acordo com o manual)
(defn get-string
  ([state]
   (get-string (second (next-char state)) "\""))
  ([state string]
   (let [[c1 new-state] (next-char state)
          c2            (peek-char new-state)] ; gambiarra?
   (cond
     (nil? c1)        (die "Falta fechar a string.")
     (= \u0000 c1)    (die "Caractere nulo '\\0' encontrado na string.")
     (= \newline c1)  (die "Faltou escapar o '\\n'")
     (= \\ c1)        (recur (second (next-char state)) (str string c1 c2))
     (quote? c1)      [new-state [(str string \") :string]]
     :else            (recur new-state (str string c1))))))

; TODO: descobrir como melhorar isso
(defn get-operator
  [state]
  (let [[c1 new-state] (next-char state)
         c2            (peek-char state)]
    (cond
      ; provavelmente é melhor comparar char a char ao invés
      ; de construir uma string, porem, no momento,
      ; o objetivo não é ter performance maxima.
      (= (str c1 c2) "<-")   [(second (next-char new-state)) ["<-" :assign]]
      (= (str c1 c2) "<=")   [(second (next-char new-state)) ["<=" :leq]]
      (= (str c1 c2) "=>")   [(second (next-char new-state)) ["=>" :to]]
      :else                  [new-state [(str c1) (get single-char-ops c1)]])))

(defn get-integer
  ([state]
   (get-integer state ""))
  ([state string]
   (let [[c new-state] (next-char state)]
     (if (Character/isDigit c)
       (recur new-state (str string c))
       [new-state [string :integer]]))))

(defn ignore-whitespace [state] (second (next-char state)))

; pode ser interessante mudar a ordem dos testes, para 
; diminuir os testes e melhorar a performance
; TODO: descobrir como fazer a recursão. dessa forma NAO FUNCIONA, pois não é
; possível pegar o erro.
(defn lex
  ([buf]
   (lex {:buf buf :row 1 :col 1}))
  ([state]
   (let [c (peek-char state)
     fun (cond
         (nil? c)                  nil
         (Character/isDigit c)     get-integer
         (valid-first-char? c)     get-token 
         (quote? c)                get-string 
         ; operator precisa ser depois de testar se é comentário, pois
         ; comentários multilinha começam com '('
         (operator? c)             get-operator
         :else                     get-erro?)]

    (cond
      (singleline-comment? state) (recur ignore-singleline-comment)
      (whitespace? c)             (recur ignore-whitespace)
      (multiline-comment? state)  (recur ignore-multiline-comment)
      :else (fun state)))))
