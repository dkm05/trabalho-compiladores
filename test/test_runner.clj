(ns test-runner
  (:require [semantic]
            [parser]
            [clojure.java.io :as io]))

(defn run-test [file should-pass?]
  (let [filename (.getPath file)]
    (try
      (let [ast (parser/get-ast filename)
            class-map (semantic/run-semantic ast)]
        (semantic/verify-program-types class-map)
        (if should-pass?
          (println "[PASS] Sucesso esperado e obtido em:" filename)
          (println "[FAIL] Falha:" filename "- Esperava um erro semântico, mas o código passou.")))
      (catch Exception e
        (if should-pass?
          (println "[FAIL] Falha:" filename "- Erro inesperado:" (.getMessage e))
          (println "[PASS] Erro capturado corretamente em" filename ":" (.getMessage e)))))))

(defn -main [& args]
  (let [tests-dir (io/file "test/fixtures")]
    (if (.exists tests-dir)
      (let [files (file-seq tests-dir)
            cl-files (filter #(and (.isFile %) (.endsWith (.getName %) ".cl")) files)]
        (println "A correr testes na pasta 'test/fixtures/'...\n")
        (doseq [f cl-files]
          (let [filename (.getName f)
                should-pass? (not (.startsWith filename "error_"))]
            (run-test f should-pass?))))
      (println "A pasta 'test/fixtures/' não foi encontrada!"))))
