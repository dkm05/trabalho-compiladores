(ns semantic
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [parser :as parser])
  (:gen-class))

(defn -main [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: clj -M -m parser <file.cl>")
      (pprint/pprint (parser/get-ast filename)))))
