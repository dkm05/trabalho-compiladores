(ns codegen.types
  (:require [clojure.string :as str]))

(def COOL-OBJECT  "cool/runtime/CoolObject")
(def COOL-IO      "cool/runtime/CoolIO")
(def COOL-INT     "cool/runtime/CoolInt")
(def COOL-STRING  "cool/runtime/CoolString")
(def COOL-BOOL    "cool/runtime/CoolBool")
(def COOL-RUNTIME "cool/runtime/CoolRuntime")

(def DESC-OBJECT  (str "L" COOL-OBJECT ";"))
(def DESC-IO      (str "L" COOL-IO ";"))
(def DESC-INT     (str "L" COOL-INT ";"))
(def DESC-STRING  (str "L" COOL-STRING ";"))
(def DESC-BOOL    (str "L" COOL-BOOL ";"))

(def basic-cool->jvm
  {"Object" COOL-OBJECT
   "IO"     COOL-IO
   "Int"    COOL-INT
   "String" COOL-STRING
   "Bool"   COOL-BOOL})

(def basic-jvm->cool
  (into {} (map (fn [[k v]] [v k]) basic-cool->jvm)))

(defn cool-type->internal
  "Maps a Cool type name to its JVM internal class name.
   Erases SELF_TYPE to cool/runtime/CoolObject for invariant method/field descriptors."
  [cool-type]
  (cond
    (= cool-type "SELF_TYPE") COOL-OBJECT
    (contains? basic-cool->jvm cool-type) (get basic-cool->jvm cool-type)
    :else cool-type))

(defn resolve-cool-type
  "Resolves SELF_TYPE to current-class for concrete stack types and checkcast targets."
  [cool-type current-class]
  (if (= cool-type "SELF_TYPE")
    current-class
    cool-type))

(defn jvm-internal->cool-name
  "Maps a JVM internal class name back to a Cool class name (if applicable)."
  [internal-name]
  (get basic-jvm->cool internal-name internal-name))

(defn internal->desc [internal-name]
  (str "L" internal-name ";"))

(defn cool-type->desc [cool-type]
  (internal->desc (cool-type->internal cool-type)))

(defn virtual-method-name [mname]
  (str "cool$" mname))

(defn static-method-name [mname]
  (str "cool$static$" mname))

(defn field-name [aname]
  (str "attr$" aname))

(defn method-virtual-desc
  "JVM descriptor for virtual method cool$<name>(formals) -> return-type."
  [meth]
  (let [params (apply str (map #(cool-type->desc (:declared-type %)) (:formals meth)))
        ret    (cool-type->desc (:return-type meth))]
    (str "(" params ")" ret)))

(defn method-static-desc
  "JVM descriptor for static implementation cool$static$<name>(self, formals) -> return-type."
  [defining-class meth]
  (let [self-desc (cool-type->desc defining-class)
        params    (apply str (map #(cool-type->desc (:declared-type %)) (:formals meth)))
        ret       (cool-type->desc (:return-type meth))]
    (str "(" self-desc params ")" ret)))

(defn unescape-cool-string
  "Strips surrounding quotes (if present) and decodes Cool string escapes (§10.2):
   \\b, \\t, \\n, \\f, \\\\, \\\", \\<newline> -> \\n, and \\c -> c."
  [^String raw]
  (let [inner (if (and (>= (.length raw) 2)
                       (= \" (.charAt raw 0))
                       (= \" (.charAt raw (dec (.length raw)))))
                (.substring raw 1 (dec (.length raw)))
                raw)
        n     (.length inner)
        sb    (StringBuilder. n)]
    (loop [i 0]
      (if (>= i n)
        (.toString sb)
        (let [ch (.charAt inner i)]
          (if (and (= ch \\) (< (inc i) n))
            (let [nxt (.charAt inner (inc i))]
              (.append sb (case nxt
                            \b \backspace
                            \t \tab
                            \n \newline
                            \f \formfeed
                            nxt))
              (recur (+ i 2)))
            (do
              (.append sb ch)
              (recur (inc i)))))))))

(defn parse-cool-int
  "Parses a Cool integer literal string into a 32-bit signed int (supports 2147483648 for ~2147483648)."
  [^String s]
  (let [v (Long/parseLong (str/trim s))]
    (unchecked-int v)))

(defn parse-cool-bool
  "Parses a Cool boolean literal string (case-insensitive after first char: t... -> true)."
  [^String s]
  (= \t (.charAt s 0)))
