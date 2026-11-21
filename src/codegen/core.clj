(ns codegen.core
  (:require [basic-classes :as basic-classes]
            [clojure.java.io :as io]
            [codegen.asm :as asm]
            [codegen.class-gen :as class-gen]
            [lex :as lex]
            [parser :as parser]
            [semantic :as semantic]))

(def ^:private runtime-class-resources
  ["cool/runtime/CoolObject.class"
   "cool/runtime/CoolIO.class"
   "cool/runtime/CoolInt.class"
   "cool/runtime/CoolString.class"
   "cool/runtime/CoolBool.class"
   "cool/runtime/CoolRuntime.class"])

(defn- copy-runtime-classes! [out-dir]
  (doseq [res-path runtime-class-resources]
    (let [res  (or (io/resource res-path)
                   (throw (ex-info (str "Runtime class resource não encontrado no classpath: " res-path)
                                   {:resource res-path})))
          dest (io/file out-dir res-path)]
      (.mkdirs (.getParentFile dest))
      (with-open [in  (io/input-stream res)
                  out (io/output-stream dest)]
        (io/copy in out)))))

(defn- parse-source-string [source]
  (let [tokens (loop [state  (lex/init-state source)
                      tokens []]
                 (if-let [[next-state token] (lex/lex state)]
                   (recur next-state (conj tokens token))
                   (conj tokens ["EOF" :eof -1 -1])))
        [ast _] (parser/parse-program tokens)]
    ast))

(defn emit-program!
  "Emits .class files for all user-defined classes in a verified/annotated class-map,
   copies the cool.runtime classes into out-dir, and optionally verifies all .class files
   via CheckClassAdapter.verify."
  ([class-map out-dir source-file]
   (emit-program! class-map out-dir source-file {:verify? true}))
  ([class-map out-dir source-file {:keys [verify?] :or {verify? true}}]
   (let [out-file (io/file out-dir)]
     (.mkdirs out-file)
     (copy-runtime-classes! out-file)
     (let [user-classes  (->> (vals class-map)
                              (remove #(contains? basic-classes/basic-classes (:name %)))
                              (sort-by :name))
           emitted-files (mapv (fn [cls]
                                 (let [bytes  (class-gen/generate-class-bytes cls class-map source-file)
                                       target (io/file out-file (str (:name cls) ".class"))]
                                   (with-open [os (io/output-stream target)]
                                     (.write os bytes))
                                   target))
                               user-classes)]
       (when verify?
         (asm/verify-output-dir! out-file emitted-files))
       {:out-dir (.getPath out-file)
        :classes (mapv :name user-classes)}))))

(defn compile-ast!
  ([ast out-dir source-file]
   (compile-ast! ast out-dir source-file {:verify? true}))
  ([ast out-dir source-file opts]
   (let [typed-class-map (-> ast
                             (semantic/run-semantic)
                             (semantic/verify-program-types))]
     (emit-program! typed-class-map out-dir source-file opts))))

(defn compile-file!
  ([filename out-dir]
   (compile-file! filename out-dir {:verify? true}))
  ([filename out-dir opts]
   (let [ast         (parser/get-ast filename)
         source-name (.getName (io/file filename))]
     (compile-ast! ast out-dir source-name opts))))

(defn compile-source!
  ([source out-dir]
   (compile-source! source out-dir "input.cl" {:verify? true}))
  ([source out-dir source-name opts]
   (let [ast (parse-source-string source)]
     (compile-ast! ast out-dir source-name opts))))
