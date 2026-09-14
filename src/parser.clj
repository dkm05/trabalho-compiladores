(ns parser
  (:require [lex :as lex])
  (:gen-class))

(defn lex-all
  [filename] 
  (loop [state (lex/init-state (slurp filename))
         tokens []]
      (let [result (lex/lex state)]
          (if result 
            (let [[next-state token] result]
              (recur next-state (conj tokens token)))
            tokens))))

(defn -main
  [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: java -jar ./parser <file.cl>")
        (println (first (lex-all filename))))))
