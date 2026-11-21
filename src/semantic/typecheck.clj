(ns semantic.typecheck
  (:require [ast]
            [basic-classes :as basic-classes]
            [semantic.hierarchy :refer [err! check! conforms? lca]])
  (:import [ast Method Attribute
                IntLit StringLit BoolLit Id Assign If While Block
                Let Case New Call Dispatch BinaryOp UnaryOp]))

(defn- valid-type? [class-map t]
  (or (= t "SELF_TYPE") (contains? class-map t)))

(defn- lookup-var [class-map current-class local-env var-name]
  (or (get local-env var-name)
      (get-in class-map [current-class :attribute var-name :declared-type])))

(defmulti type-of (fn [expr class-map current-class local-env] (type expr)))

(defmethod type-of :default [node _ _ _]
  (err! node (str "Nó AST não tratado: " (type node))))

(defmethod type-of IntLit [node _ _ _]
  (assoc node :inferred-type "Int"))

(defmethod type-of StringLit [node _ _ _]
  (assoc node :inferred-type "String"))

(defmethod type-of BoolLit [node _ _ _]
  (assoc node :inferred-type "Bool"))

(defmethod type-of New [node class-map _ _]
  (let [class-name (:name node)]
    (check! (valid-type? class-map class-name) node
            (str "A classe '" class-name "' não foi definida."))
    (assoc node :inferred-type class-name)))

(defmethod type-of Id [node class-map current-class local-env]
  (let [var-name (:name node)
        t        (if (= var-name "self")
                   "SELF_TYPE"
                   (or (lookup-var class-map current-class local-env var-name)
                       (err! node (str "Erro: Variável '" var-name "' não declarada."))))]
    (assoc node :inferred-type t)))

(defmethod type-of Assign [node class-map current-class local-env]
  (let [var-name  (:name node)
        expr'     (type-of (:expr node) class-map current-class local-env)
        expr-type (:inferred-type expr')
        decl-type (lookup-var class-map current-class local-env var-name)]
    (check! decl-type node (str "Variável '" var-name "' não declarada."))
    (check! (conforms? expr-type decl-type class-map current-class) node
            (str "Tipo '" expr-type "' não conforma com '" decl-type "'."))
    (assoc node :expr expr' :inferred-type expr-type)))

(defn- eval-let-binding [class-map current-class [bindings-acc env] b]
  (let [bname (:name b)
        btype (:declared-type b)
        binit (:init b)]
    (check! (valid-type? class-map btype) b
            (str "O tipo '" btype "' não está definido."))
    (let [b' (if binit
               (let [init'     (type-of binit class-map current-class env)
                     init-type (:inferred-type init')]
                 (check! (conforms? init-type btype class-map current-class) b
                         (str "Inicialização '" init-type "' não conforma com '" btype "'."))
                 (assoc b :init init'))
               b)]
      [(conj bindings-acc b') (assoc env bname btype)])))

(defmethod type-of Let [node class-map current-class local-env]
  (let [[bindings' final-env] (reduce #(eval-let-binding class-map current-class %1 %2)
                                      [[] local-env]
                                      (:bindings node))
        body'                 (type-of (:body node) class-map current-class final-env)]
    (assoc node
           :bindings bindings'
           :body body'
           :inferred-type (:inferred-type body'))))

(defmethod type-of UnaryOp [node class-map current-class local-env]
  (let [op        (:op node)
        expr'     (type-of (:expr node) class-map current-class local-env)
        expr-type (:inferred-type expr')
        res-type  (case op
                    :not           (do (check! (= expr-type "Bool") node "Operador 'not' exige tipo Bool.") "Bool")
                    :intcomplement (do (check! (= expr-type "Int") node "Operador '~' exige tipo Int.") "Int")
                    :isvoid        "Bool"
                    (err! node (str "Operador unário desconhecido: " op)))]
    (assoc node :expr expr' :inferred-type res-type)))

(defmethod type-of BinaryOp [node class-map current-class local-env]
  (let [left'      (type-of (:left node) class-map current-class local-env)
        right'     (type-of (:right node) class-map current-class local-env)
        left-type  (:inferred-type left')
        right-type (:inferred-type right')
        op         (:op node)
        res-type   (cond
                     (contains? #{:sum :sub :mult :div} op)
                     (do
                       (check! (and (= left-type "Int") (= right-type "Int"))
                               node "Operação aritmética exige operandos do tipo Int.")
                       "Int")

                     (contains? #{:lt :leq} op)
                     (do
                       (check! (and (= left-type "Int") (= right-type "Int"))
                               node (str "Operador " op " exige Int."))
                       "Bool")

                     (= op :eq)
                     (let [basic-types #{"Int" "String" "Bool"}]
                       (when (or (contains? basic-types left-type) (contains? basic-types right-type))
                         (check! (= left-type right-type) node "Comparação '=' inválida entre tipos diferentes."))
                       "Bool")

                     :else (err! node (str "Operador binário desconhecido: " op)))]
    (assoc node :left left' :right right' :inferred-type res-type)))

(defmethod type-of If [node class-map current-class local-env]
  (let [cond' (type-of (:cond node) class-map current-class local-env)]
    (check! (= "Bool" (:inferred-type cond')) node "A condição do 'if' deve ser Bool.")
    (let [then' (type-of (:then node) class-map current-class local-env)
          else' (type-of (:else node) class-map current-class local-env)
          t     (lca (:inferred-type then') (:inferred-type else') class-map current-class)]
      (assoc node :cond cond' :then then' :else else' :inferred-type t))))

(defmethod type-of While [node class-map current-class local-env]
  (let [cond' (type-of (:cond node) class-map current-class local-env)]
    (check! (= "Bool" (:inferred-type cond')) node "A condição do 'while' deve ser Bool.")
    (let [body' (type-of (:body node) class-map current-class local-env)]
      (assoc node :cond cond' :body body' :inferred-type "Object"))))

(defmethod type-of Block [node class-map current-class local-env]
  (let [body' (mapv #(type-of % class-map current-class local-env) (:body node))
        t     (if (seq body') (:inferred-type (peek body')) "Object")]
    (assoc node :body body' :inferred-type t)))

(defn- eval-case-branch [class-map current-class local-env b]
  (let [bname (:name b)
        btype (:declared-type b)]
    (check! (contains? class-map btype) b (str "O tipo '" btype "' não está definido."))
    (let [body' (type-of (:body b) class-map current-class (assoc local-env bname btype))]
      (assoc b :body body' :inferred-type (:inferred-type body')))))

(defmethod type-of Case [node class-map current-class local-env]
  (let [expr'          (type-of (:expr node) class-map current-class local-env)
        cases          (:cases node)
        declared-types (map :declared-type cases)]
    (check! (= (count declared-types) (count (set declared-types)))
            node "Erro semântico: Tipos duplicados nas ramificações do case.")
    (let [cases'       (mapv #(eval-case-branch class-map current-class local-env %) cases)
          branch-types (map :inferred-type cases')
          res-type     (reduce #(lca %1 %2 class-map current-class) branch-types)]
      (assoc node :expr expr' :cases cases' :inferred-type res-type))))

(defn verify-args! [args formals class-map current-class local-env node]
  (check! (= (count args) (count formals)) node "Número incorreto de argumentos.")
  (mapv (fn [arg formal]
          (let [arg'     (type-of arg class-map current-class local-env)
                arg-type (:inferred-type arg')]
            (check! (conforms? arg-type (:declared-type formal) class-map current-class) arg
                    (str "Argumento '" arg-type "' não conforma com '" (:declared-type formal) "'."))
            arg'))
        args
        formals))

(defn handle-dispatch! [node target-type expr-type class-map current-class local-env]
  (let [method-name (:method node)
        method      (get-in class-map [target-type :method method-name])]
    (check! method node (str "Método '" method-name "' não encontrado em '" target-type "'."))
    (let [args'    (verify-args! (:args node) (:formals method) class-map current-class local-env node)
          ret-type (if (= (:return-type method) "SELF_TYPE")
                     expr-type
                     (:return-type method))]
      (assoc node :args args' :inferred-type ret-type))))

(defmethod type-of Call [node class-map current-class local-env]
  (handle-dispatch! node current-class "SELF_TYPE" class-map current-class local-env))

(defmethod type-of Dispatch [node class-map current-class local-env]
  (let [obj'        (type-of (:obj node) class-map current-class local-env)
        expr-type   (:inferred-type obj')
        static-type (:static-type node)
        target-type (or static-type
                        (if (= expr-type "SELF_TYPE") current-class expr-type))]
    (when static-type
      (check! (contains? class-map static-type) node
              (str "Tipo estático '" static-type "' não definido."))
      (check! (conforms? expr-type static-type class-map current-class) node
              (str "O tipo '" expr-type "' não conforma com o tipo estático '" static-type "'.")))
    (-> node
        (assoc :obj obj')
        (handle-dispatch! target-type expr-type class-map current-class local-env))))

(defn- verify-attribute! [class-map cname attr]
  (let [decl-type (:declared-type attr)]
    (check! (valid-type? class-map decl-type) attr
            (str "O tipo '" decl-type "' do atributo '" (:name attr) "' não foi definido."))
    (if-let [init-expr (:init attr)]
      (let [init'     (type-of init-expr class-map cname {})
            init-type (:inferred-type init')]
        (check! (conforms? init-type decl-type class-map cname) attr
                (str "A inicialização do atributo '" (:name attr) "' não conforma com o tipo '" decl-type "'."))
        (assoc attr :init init'))
      attr)))

(defn- build-method-env [class-map meth]
  (reduce (fn [env f]
            (let [fname (:name f)
                  ftype (:declared-type f)]
              (check! (not (contains? env fname)) f (str "Parâmetro '" fname "' duplicado no método '" (:name meth) "'."))
              (check! (contains? class-map ftype) f (str "O tipo '" ftype "' do parâmetro '" fname "' não foi definido."))
              (assoc env fname ftype)))
          {}
          (:formals meth)))

(defn- verify-method! [class-map cname meth]
  (let [ret-type (:return-type meth)]
    (check! (valid-type? class-map ret-type) meth
            (str "O tipo de retorno '" ret-type "' do método '" (:name meth) "' não foi definido."))
    (let [initial-env (build-method-env class-map meth)
          body'       (type-of (:body meth) class-map cname initial-env)
          body-type   (:inferred-type body')]
      (check! (conforms? body-type ret-type class-map cname) meth
              (str "O método '" (:name meth) "' retorna '" body-type "', mas declarou '" ret-type "'."))
      (assoc meth :body body'))))

(defn- verify-class! [class-map cls]
  (let [cname (:name cls)]
    (if (contains? basic-classes/basic-classes cname)
      cls
      (let [own-attrs' (mapv #(verify-attribute! class-map cname %) (:own-attributes cls))
            own-meths' (mapv #(verify-method! class-map cname %) (:own-methods cls))
            attr-map'  (reduce (fn [m a] (assoc m (:name a) a)) (:attribute cls) own-attrs')
            meth-map'  (reduce (fn [m f] (assoc m (:name f) f)) (:method cls) own-meths')]
        (assoc cls
               :own-attributes own-attrs'
               :own-methods    own-meths'
               :attribute      attr-map'
               :method         meth-map')))))

(defn verify-program-types [class-map]
  (reduce-kv (fn [acc cname cls]
               (assoc acc cname (verify-class! class-map cls)))
             {}
             class-map))
