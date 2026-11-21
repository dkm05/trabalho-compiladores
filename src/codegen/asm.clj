(ns codegen.asm
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [codegen.types :as types]
            [semantic.hierarchy :as hierarchy])
  (:import [java.io PrintWriter StringWriter]
           [java.net URL URLClassLoader]
           [org.objectweb.asm ClassReader ClassVisitor ClassWriter MethodVisitor Opcodes]
           [org.objectweb.asm.util CheckClassAdapter]))

(def BYTECODE-VERSION Opcodes/V1_8)

(defn make-class-writer
  "Creates an ASM ClassWriter with COMPUTE_FRAMES | COMPUTE_MAXS whose
   getCommonSuperClass resolves Cool classes directly from class-map + lca."
  [class-map]
  (proxy [ClassWriter] [(bit-or ClassWriter/COMPUTE_FRAMES ClassWriter/COMPUTE_MAXS)]
    (getCommonSuperClass [^String t1 ^String t2]
      (cond
        (= t1 t2) t1
        (or (= t1 "java/lang/Object") (= t2 "java/lang/Object")) "java/lang/Object"
        :else
        (let [c1 (types/jvm-internal->cool-name t1)
              c2 (types/jvm-internal->cool-name t2)]
          (if (and (contains? class-map c1) (contains? class-map c2))
            (types/cool-type->internal (hierarchy/lca c1 c2 class-map nil))
            (try
              (proxy-super getCommonSuperClass t1 t2)
              (catch Throwable _
                "java/lang/Object"))))))))

(defn wrap-structural-check
  "Wraps a ClassWriter with CheckClassAdapter in structural-only mode (no dataflow classloading)."
  ^ClassVisitor [^ClassWriter cw]
  (CheckClassAdapter. cw false))

(defn verify-output-dir!
  "Runs ASM CheckClassAdapter.verify over generated .class files in out-dir
   using a URLClassLoader pointing to out-dir."
  ([out-dir]
   (let [dir-file (io/file out-dir)
         classes  (->> (file-seq dir-file)
                       (filter #(and (.isFile ^java.io.File %)
                                     (.endsWith (.getName ^java.io.File %) ".class")))
                       (sort-by #(.getPath ^java.io.File %)))]
     (verify-output-dir! dir-file classes)))
  ([out-dir class-files]
   (let [dir-file (io/file out-dir)
         url      (.toURL (.toURI dir-file))
         loader   (URLClassLoader. (into-array URL [url])
                                   (.getContextClassLoader (Thread/currentThread)))]
     (try
       (doseq [^java.io.File f (map io/file class-files)]
         (let [bytes (java.nio.file.Files/readAllBytes (.toPath f))
               cr    (ClassReader. bytes)
               sw    (StringWriter.)
               pw    (PrintWriter. sw)]
           (CheckClassAdapter/verify cr loader false pw)
           (.flush pw)
           (let [out (str/trim (.toString sw))]
             (when-not (str/blank? out)
               (throw (ex-info (str "Bytecode verification failed for " (.getPath f) ":\n" out)
                               {:file (.getPath f) :details out}))))))
       (finally
         (.close loader))))))

(defn emit-push-int [^MethodVisitor mv ^long n]
  (cond
    (= n -1) (.visitInsn mv Opcodes/ICONST_M1)
    (= n 0)  (.visitInsn mv Opcodes/ICONST_0)
    (= n 1)  (.visitInsn mv Opcodes/ICONST_1)
    (= n 2)  (.visitInsn mv Opcodes/ICONST_2)
    (= n 3)  (.visitInsn mv Opcodes/ICONST_3)
    (= n 4)  (.visitInsn mv Opcodes/ICONST_4)
    (= n 5)  (.visitInsn mv Opcodes/ICONST_5)
    (and (>= n Byte/MIN_VALUE) (<= n Byte/MAX_VALUE))
    (.visitIntInsn mv Opcodes/BIPUSH (int n))
    (and (>= n Short/MIN_VALUE) (<= n Short/MAX_VALUE))
    (.visitIntInsn mv Opcodes/SIPUSH (int n))
    :else
    (.visitLdcInsn mv (Integer/valueOf (int n)))))

(defn emit-default-value [^MethodVisitor mv cool-type]
  (case cool-type
    "Int"    (.visitFieldInsn mv Opcodes/GETSTATIC types/COOL-INT "ZERO" types/DESC-INT)
    "String" (.visitFieldInsn mv Opcodes/GETSTATIC types/COOL-STRING "EMPTY" types/DESC-STRING)
    "Bool"   (.visitFieldInsn mv Opcodes/GETSTATIC types/COOL-BOOL "FALSE" types/DESC-BOOL)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defn emit-box-int [^MethodVisitor mv]
  (.visitMethodInsn mv Opcodes/INVOKESTATIC
                    types/COOL-INT "of" (str "(I)" types/DESC-INT) false))

(defn emit-unbox-int [^MethodVisitor mv]
  (.visitFieldInsn mv Opcodes/GETFIELD types/COOL-INT "value" "I"))

(defn emit-unbox-bool [^MethodVisitor mv]
  (.visitFieldInsn mv Opcodes/GETFIELD types/COOL-BOOL "value" "Z"))

(defn emit-checkcast-if-needed [^MethodVisitor mv target-internal]
  (when (not= target-internal types/COOL-OBJECT)
    (.visitTypeInsn mv Opcodes/CHECKCAST target-internal)))
