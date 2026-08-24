(ns lex
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
)

(def keyword-table
  #{"class" "else" "fi" "if" "in" "inherits" "isvoid"
    "let" "loop" "pool" "then" "while" "case" "esac"
    "new" "of" "not"})

(def single-char-ops
  { \+ :sum
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
    \, :comma })

(defn valid-first-char? [c] 
  (and c (or (Character/isLetter c) (= c \_))))
(defn valid-body-char? [c] 
  (and c (or (valid-first-char? c) (Character/isDigit c))))
(defn quote? [c] (= c \"))
(defn operator? [c] (contains? single-char-ops c))
; tem que ver EOF aqui
(defn multiline-comment? [[c1 c2]]
  (and (= c1 \() (= c2 \*)))
(defn singleline-comment? [[c1 c2]]
  (and (= c1 \-) (= c2 \-)))
(defn is-keyword? [word] (contains? keyword-table word))
(defn die [message] (println message))
; isso é apenas pra verificar se começa, de acordo com o manual:
; "Comments may also be written by enclosing
;  text in (∗ . . . ∗). The latter form of comment may be nested. "
; provavelmente usar uma stack/PDA


(defn classify-word 
  [word]
  (let [low-word (str/lower-case word)]
  (cond
    (= word "self")                                :self-token
    (= word "SELF_TYPE")                           :self-type-token
    (keyword-table low-word)                       :keyword
    (and (= low-word "true")  (= (first word) \t)) :boolean
    (and (= low-word "false") (= (first word) \f)) :boolean
    (Character/isUpperCase (first word))           :type-identifier
    :else                                          :object-identifier
  ))
)

(defn get-token
  ([buf]
    (get-token buf ""))
  ([buf word]
  (if (valid-body-char? (first buf))
      (recur (rest buf) (str word (first buf)))
      (let [tag (classify-word word)]
        [buf [word tag]])))
)

; descobrir como parar em caso de erro
; provavelmente no parser, ja que o lexer vai apenas retornar o token,
; while (true)
;   tok = lex(buf)
;   if (erro)...

(def special-char-table 
  {
    \t \tab
    \b \backspace
    \n \newline
    \f \formfeed
    \newline \newline
  }
)

(defn ignore-singleline-comment
  [buf]
  (let [c (first buf)]
  (if (or (= c \newline) (nil? c))
    (rest buf)
    (recur (rest buf)))
  )
)

(defn ignore-multiline-comment
  ([buf]
    (ignore-multiline-comment buf '()))
  ([buf stack]
    (let [[c1 c2 & tail] buf]
    (cond
      (nil? c1)
        (die "erro: comentário multilinha não foi fechado")

      (and (= c1 \*) (= c2 \)))
        (if (empty? (rest stack))
          tail
          (recur tail (rest stack)) 
        )

      (and (= c1 \() (= c2 \*))
        (recur tail (cons \( stack))

      :else
        (recur (rest buf) stack)
    )
  ))
)

; para testes:
; (get-string '(\" \o \l \a \backspace \m \u \n \d \o))
; (get-string '(\" \tab \o \l \a \backspace \\ \newline \m \u \n \d \o \"))
; (get-string '(\" \o \l \a \newline \m \u \n \d \o \"))
; (get-string '(\" \o \l \a \backspace \\ \newline \m \u \n \d \o))
; TODO: verificar se a string tem EOF e \0 (de acordo com o manual)
(defn get-string
  ([buf]
    (get-string (rest buf) "\""))
  ([buf string]
    (cond
      ;; tem que ver a situacao em que o buffer fica vazio e é necessario recarregar.
      ;; no momento, a ideia é usar algo parecido em C, com um vetor circular de tamanho
      ;; 2 * BUFFER_SIZE, e fazer fread com tamanho BUFFER_SIZE (no caso, esta usando 2 buffers
      ;; na mesma posição da memoria: [buf1][buf2]; é necessario verificar se
      ;; é possivel fazer isso em clojure)
      (nil? (first buf))        (die "Falta fechar a string.")
      (= \u0000 (first buf))    (die "Caractere nulo '\\0' encontrado na string.")
      (= \newline (first buf))  (die "Faltou escapar o '\\n'")
      (= (first buf) \\)        (let [[_ c & tail] buf] (recur tail (str string (get special-char-table c c))))
      (quote? (first buf))      [(rest buf) [(str string \") :string]]
      :else                     (recur (rest buf) (str string (first buf)))
    )
  )
)

(defn get-operator
  [buf]
  (let [[c1 c2 & tail] buf]
    (cond
      ; provavelmente é melhor comparar char a char ao invés
      ; de construir uma string, porem, no momento,
      ; o objetivo não é ter performance maxima.
      (= (str c1 c2) "<-")   [tail ["<-" :assign]]
      (= (str c1 c2) "<=")   [tail ["<=" :leq]]
      (= (str c1 c2) "=>")   [tail ["=>" :to]]
      :else                  [(rest buf) [(str c1) (get single-char-ops c1)]]
    ) 
  )
)

(defn get-integer
  [buf]
  (let [[int-chars new-buf] (split-with #(Character/isDigit %) buf)
         integer            (apply str int-chars)]
         [new-buf [integer :integer]])   
)

(def whitespace-chars
  #{\space \newline \formfeed \return \tab \v})

(defn whitespace?
  [c]
  (boolean (whitespace-chars c)))

; pode ser interessante mudar a ordem dos testes, para 
; diminuir os testes e melhorar a performance
(defn lex
  [buf]
  (let [[c & tail] buf]
    (cond
      (nil? c)
        nil
      (Character/isDigit c)
        (get-integer buf)
      (valid-first-char? c)
        (get-token buf)
      (quote? c) 
        (get-string buf)
      (singleline-comment? buf)
        (recur (ignore-singleline-comment buf))
      (multiline-comment? buf)
        (recur (ignore-multiline-comment buf))
      (whitespace? c)
        (recur (rest buf))
      ; operator precisa ser depois de testar se é comentário, pois
      ; comentários multilinha começam com '('
      (operator? c)
        (get-operator buf)
      :else
        [tail [c :error (str "Caractere inválido: " c)]]
    )
  )
)
