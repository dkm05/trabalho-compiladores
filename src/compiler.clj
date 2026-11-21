(ns compiler
  (:require [codegen.core :as codegen])
  (:gen-class))

(defn- parse-args [args]
  (loop [rem args
         opts {:out-dir "out" :verify? true :file nil}]
    (if (empty? rem)
      opts
      (let [[a & more] rem]
        (cond
          (or (= a "-o") (= a "--out"))
          (if (seq more)
            (recur (rest more) (assoc opts :out-dir (first more)))
            (throw (ex-info (str "Opção '" a "' requer um diretório de saída.") {})))

          (= a "--no-verify")
          (recur more (assoc opts :verify? false))

          (.startsWith ^String a "-")
          (throw (ex-info (str "Opção desconhecida: " a) {}))

          :else
          (recur more (assoc opts :file a)))))))

(defn -main [& args]
  (try
    (let [{:keys [file out-dir verify?]} (parse-args args)]
      (if (nil? file)
        (do
          (println "Uso: clojure -M -m compiler <arquivo.cl> [-o <dir-saida>] [--no-verify]")
          (System/exit 1))
        (let [{:keys [classes]} (codegen/compile-file! file out-dir {:verify? verify?})]
          (println (str "Compilação concluída: " (count classes) " classe(s) gerada(s) em '" out-dir "'."))
          (System/exit 0))))
    (catch Exception e
      (binding [*out* *err*]
        (println (str "Erro de compilação: " (.getMessage e))))
      (System/exit 1))))
