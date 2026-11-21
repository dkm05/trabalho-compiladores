(ns basic-classes
  (:require [ast :as ast]))

(def basic-classes
  {"Object"
   (ast/->ClassDef
    "Object" nil
    [(ast/->Method "abort"     "Object"    [] nil 0 0)
     (ast/->Method "type_name" "String"    [] nil 0 0)
     (ast/->Method "copy"      "SELF_TYPE" [] nil 0 0)]
    0 0)

   "IO"
   (ast/->ClassDef
    "IO" "Object"
    [(ast/->Method "out_string" "SELF_TYPE" [(ast/->Formal "x" "String" 0 0)] nil 0 0)
     (ast/->Method "out_int"    "SELF_TYPE" [(ast/->Formal "x" "Int" 0 0)]    nil 0 0)
     (ast/->Method "in_string"  "String"    []                                nil 0 0)
     (ast/->Method "in_int"     "Int"       []                                nil 0 0)]
    0 0)

   "Int"
   (ast/->ClassDef "Int" "Object" [] 0 0)

   "String"
   (ast/->ClassDef
    "String" "Object"
    [(ast/->Method "length" "Int"    []                                                              nil 0 0)
     (ast/->Method "concat" "String" [(ast/->Formal "s" "String" 0 0)]                               nil 0 0)
     (ast/->Method "substr" "String" [(ast/->Formal "i" "Int" 0 0) (ast/->Formal "l" "Int" 0 0)]     nil 0 0)]
    0 0)

   "Bool"
   (ast/->ClassDef "Bool" "Object" [] 0 0)})
