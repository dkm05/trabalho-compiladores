(ns semantic
  (:require [parser :as parser]
            [semantic.hierarchy :as hierarchy]
            [semantic.typecheck :as typecheck])
  (:gen-class))

(defn run-semantic [ast]
  (hierarchy/build-hierarchy ast))

(defn verify-program-types [class-map]
  (typecheck/verify-program-types class-map))

(defn -main [& args]
  (let [filename (first args)]
    (if (nil? filename)
      (println "usage: clj -M -m semantic <file.cl>")
      (try
        (let [ast       (parser/get-ast filename)
              class-map (run-semantic ast)]
          (verify-program-types class-map)
          (println "Verificação semântica concluida.")
          (System/exit 0))
        (catch Exception e
          (println (.getMessage e))
          (System/exit 1))))))
