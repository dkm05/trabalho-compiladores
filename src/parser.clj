(ns parser
  (:require [lex :as lex])
  (:gen-class))

(defn -main
  [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: java -jar ./parser <file.cl>")
      (loop [[token buf row col] (lex/lex (seq (slurp filename)))]
        (when token
          (prn token row col) 
          (recur (lex/lex buf row col))
        )
      )
    )
  )
)
