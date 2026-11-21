(ns runtime-test
  (:require [clojure.test :refer [deftest is testing run-tests]])
  (:import [cool.runtime CoolObject CoolIO CoolInt CoolString CoolBool CoolRuntime]
           [java.io ByteArrayInputStream ByteArrayOutputStream PrintStream]))

(defmacro with-throw-on-error [& body]
  `(do
     (set! CoolRuntime/throwOnError true)
     (try
       ~@body
       (finally
         (set! CoolRuntime/throwOnError false)))))

(defmacro with-stdin [input-str & body]
  `(let [orig-in# System/in]
     (try
       (System/setIn (ByteArrayInputStream. (.getBytes ~input-str "UTF-8")))
       ~@body
       (finally
         (System/setIn orig-in#)))))

(defmacro with-captured-stdout [& body]
  `(let [orig-out# System/out
         baos#     (ByteArrayOutputStream.)
         ps#       (PrintStream. baos# true "UTF-8")]
     (try
       (System/setOut ps#)
       (let [res# (do ~@body)]
         [res# (.toString baos# "UTF-8")])
       (finally
         (System/setOut orig-out#)))))

(deftest basic-type-names-test
  (is (= "Object" (.-value (.cool$type_name (CoolObject.)))))
  (is (= "IO"     (.-value (.cool$type_name (CoolIO.)))))
  (is (= "Int"    (.-value (.cool$type_name (CoolInt/of 7)))))
  (is (= "String" (.-value (.cool$type_name (CoolString/of "abc")))))
  (is (= "Bool"   (.-value (.cool$type_name CoolBool/TRUE)))))

(deftest object-copy-and-abort-test
  (let [o (CoolObject.)
        c (.cool$copy o)]
    (is (not (identical? o c)))
    (is (= CoolObject (class c))))
  (with-throw-on-error
    (is (thrown-with-msg? RuntimeException #"Abort called from class Object"
                          (.cool$abort (CoolObject.))))
    (is (thrown-with-msg? RuntimeException #"Abort called from class Int"
                          (.cool$abort (CoolInt/of 1))))))

(deftest int-and-bool-test
  (is (= 0 (.-value CoolInt/ZERO)))
  (is (= 42 (.-value (CoolInt/of 42))))
  (is (= 100000 (.-value (CoolInt/of 100000))))
  (is (identical? CoolInt/ZERO (.cool$$newInstance (CoolInt/of 9))))
  (is (true? (.-value (CoolBool/of true))))
  (is (false? (.-value (CoolBool/of false))))
  (is (identical? CoolBool/FALSE (.cool$$newInstance CoolBool/TRUE))))

(deftest string-methods-test
  (let [s (CoolString/of "hello world")]
    (is (= 11 (.-value (.cool$length s))))
    (is (= "hello world!" (.-value (.cool$concat s (CoolString/of "!")))))
    (is (= "world" (.-value (.cool$substr s (CoolInt/of 6) (CoolInt/of 5)))))
    (is (= "" (.-value (.cool$substr s (CoolInt/of 11) (CoolInt/of 0)))))
    (is (identical? CoolString/EMPTY (.cool$$newInstance s))))
  (with-throw-on-error
    (let [s (CoolString/of "abc")]
      (is (thrown? RuntimeException (.cool$substr s (CoolInt/of -1) (CoolInt/of 1))))
      (is (thrown? RuntimeException (.cool$substr s (CoolInt/of 0) (CoolInt/of -1))))
      (is (thrown? RuntimeException (.cool$substr s (CoolInt/of 2) (CoolInt/of 2))))
      (is (thrown? RuntimeException (.cool$substr s (CoolInt/of Integer/MAX_VALUE) (CoolInt/of 2)))))))

(deftest io-methods-test
  (let [io (CoolIO.)]
    (let [[ret out] (with-captured-stdout
                      (-> io
                          (.cool$out_string (CoolString/of "v="))
                          (#(.cool$out_int ^CoolIO % (CoolInt/of 42)))))]
      (is (identical? io ret))
      (is (= "v=42" out)))
    (testing "in_int strict §8.2 rules and shared reader with in_string"
      (with-stdin "  42abc\n  -17xyz\n  +99\n   abc\n  -abc\n\nhello cool\n"
        (is (= 42  (.-value (.cool$in_int io))))
        (is (= -17 (.-value (.cool$in_int io))))
        (is (= 99  (.-value (.cool$in_int io))))
        (is (= 0   (.-value (.cool$in_int io))))
        (is (= 0   (.-value (.cool$in_int io))))
        (is (= 0   (.-value (.cool$in_int io))))
        (is (= "hello cool" (.-value (.cool$in_string io))))
        ;; EOF returns 0 and ""
        (is (= 0   (.-value (.cool$in_int io))))
        (is (= ""  (.-value (.cool$in_string io))))))))

(deftest runtime-helpers-test
  (testing "coolEquals"
    (is (true?  (.-value (CoolRuntime/coolEquals nil nil))))
    (is (false? (.-value (CoolRuntime/coolEquals nil (CoolObject.)))))
    (is (false? (.-value (CoolRuntime/coolEquals (CoolObject.) nil))))
    (let [o (CoolObject.)]
      (is (true?  (.-value (CoolRuntime/coolEquals o o))))
      (is (false? (.-value (CoolRuntime/coolEquals o (CoolObject.))))))
    ;; Non-cached ints compared by value
    (is (true?  (.-value (CoolRuntime/coolEquals (CoolInt/of 10000) (CoolInt/of 10000)))))
    (is (false? (.-value (CoolRuntime/coolEquals (CoolInt/of 10000) (CoolInt/of 10001)))))
    ;; Strings compared by value (distinct String instances)
    (is (true?  (.-value (CoolRuntime/coolEquals (CoolString/of (String. "abc")) (CoolString/of (String. "abc"))))))
    (is (false? (.-value (CoolRuntime/coolEquals (CoolString/of "abc") (CoolString/of "abd")))))
    ;; Bools
    (is (true?  (.-value (CoolRuntime/coolEquals CoolBool/TRUE CoolBool/TRUE))))
    (is (false? (.-value (CoolRuntime/coolEquals CoolBool/TRUE CoolBool/FALSE))))
    ;; Mixed basic types (when typed as Object)
    (is (false? (.-value (CoolRuntime/coolEquals (CoolInt/of 1) (CoolString/of "1"))))))

  (testing "operators and void checks"
    (is (= 3 (.-value (CoolRuntime/div 7 2 "f.cl" 1))))
    (is (true?  (.-value (CoolRuntime/lt 1 2))))
    (is (false? (.-value (CoolRuntime/lt 2 2))))
    (is (true?  (.-value (CoolRuntime/leq 2 2))))
    (is (false? (.-value (CoolRuntime/not CoolBool/TRUE))))
    (is (true?  (.-value (CoolRuntime/isVoid nil))))
    (is (false? (.-value (CoolRuntime/isVoid (CoolObject.)))))
    (is (nil? (CoolRuntime/checkDispatchNotVoid (CoolObject.) "f.cl" 1)))
    (is (nil? (CoolRuntime/checkCaseNotVoid (CoolObject.) "f.cl" 1))))

  (testing "runtime errors"
    (with-throw-on-error
      (is (thrown-with-msg? RuntimeException #"Division by zero"
                            (CoolRuntime/div 5 0 "f.cl" 10)))
      (is (thrown-with-msg? RuntimeException #"Dispatch to void"
                            (CoolRuntime/checkDispatchNotVoid nil "f.cl" 11)))
      (is (thrown-with-msg? RuntimeException #"Match on void in case"
                            (CoolRuntime/checkCaseNotVoid nil "f.cl" 12)))
      (is (thrown-with-msg? RuntimeException #"No match in case.*Int"
                            (CoolRuntime/caseNoMatch (CoolInt/of 5) "f.cl" 13))))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'runtime-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
