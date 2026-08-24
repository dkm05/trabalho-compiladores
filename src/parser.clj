(ns parser
  (:require [lex :as lex])
  (:gen-class))

(defn next-token
  [buf] 
  (lex/lex buf)
  )

(defn -main
  [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: java -jar ./parser <file.cl>")
      (loop [[buf token] (next-token (seq (slurp filename)))]
        (when token
          (prn token) 
          (recur (lex/lex buf))
        )
      )
    )
  )
)
