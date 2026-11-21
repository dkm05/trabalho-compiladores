(ns semantic.hierarchy
  (:require [ast]
            [basic-classes :as basic-classes])
  (:import [ast Method Attribute]))

(defn err! [node msg]
  (throw (ex-info msg {:row (:row node "N/A") :col (:col node "N/A")})))

(defmacro check!
  [condition node msg]
  `(when-not ~condition
     (err! ~node ~msg)))

(defn build-class-map [classes]
  (reduce (fn [acc cls]
            (check! (not (contains? acc (:name cls))) cls (str "Classe duplicada ou protegida: " (:name cls)))
            (assoc acc (:name cls) cls))
          basic-classes/basic-classes
          classes))

(defn validate-inheritance [class-map]
  (doseq [cls (vals class-map)]
    (let [parent (:inherits cls)
          cname  (:name cls)]
      (when parent
        (check! (not (#{"Int" "String" "Bool"} parent)) cls (str "A classe '" cname "' não pode herdar de '" parent "'."))
        (check! (contains? class-map parent) cls (str "Classe pai '" parent "' não definida.")))))
  class-map)

(defn check-cycles [class-map]
  (doseq [start-cls (vals class-map)]
    (loop [curr-name (:name start-cls)
           visited   #{}]
      (when curr-name
        (check! (not (contains? visited curr-name)) start-cls (str "Ciclo de herança detectado envolvendo a classe '" curr-name "'."))
        (recur (:inherits (get class-map curr-name)) (conj visited curr-name)))))
  class-map)

(defn ensure-main-class [class-map]
  (let [main-cls (get class-map "Main")]
    (check! main-cls {} "O programa deve definir uma classe 'Main'.")
    (let [main-method (first (filter #(and (instance? Method %) (= (:name %) "main"))
                                     (:features main-cls)))]
      (check! main-method main-cls "A classe 'Main' deve conter um método chamado 'main'.")
      (check! (empty? (:formals main-method)) main-method "O método 'main' da classe 'Main' não pode receber parâmetros.")))
  class-map)

(defn- feature-kind [feat]
  (cond
    (instance? Method feat)    :method
    (instance? Attribute feat) :attribute
    :else                      (err! feat (str "Feature inválida: " (type feat)))))

(defn separate-class-features [cls]
  (let [cname (:name cls)]
    (reduce (fn [acc feat]
              (let [ftype  (feature-kind feat)
                    fname  (:name feat)
                    feat'  (assoc feat :defining-class cname)
                    own-k  (if (= ftype :method) :own-methods :own-attributes)]
                (check! (not (contains? (get acc ftype) fname)) feat (str "O " (name ftype) " '" fname "' foi redefinido."))
                (-> acc
                    (assoc-in [ftype fname] feat')
                    (update own-k conj feat'))))
            (assoc cls :method {} :attribute {} :own-methods [] :own-attributes [])
            (:features cls))))

(defn build-all-features [class-map]
  (reduce (fn [m [k v]]
            (assoc m k (separate-class-features v)))
          {}
          class-map))

(defn collect-ancestors [class-map class-name]
  (loop [curr-name (:inherits (get class-map class-name))
         ancestors []]
    (if (nil? curr-name)
      ancestors
      (recur (:inherits (get class-map curr-name)) (conj ancestors curr-name)))))

(defn- check-method-override! [m-child m-parent]
  (let [k (:name m-child)]
    (check! (= (:return-type m-child) (:return-type m-parent))
            m-child (str "Assinatura do método '" k "' incompatível (retorno)."))
    (check! (= (map :declared-type (:formals m-child))
               (map :declared-type (:formals m-parent)))
            m-child (str "Assinatura do método '" k "' incompatível (parâmetros)."))))

(defn- merge-ancestor [class-map acc anc-name]
  (let [anc-cls  (get class-map anc-name)
        anc-meth (:method anc-cls)
        anc-attr (:attribute anc-cls)]
    (doseq [[k m-parent] anc-meth]
      (when-let [m-child (get (:method acc) k)]
        (check-method-override! m-child m-parent)))
    (doseq [[k attr-child] (:attribute acc)]
      (check! (not (contains? anc-attr k))
              attr-child
              (str "Erro semântico: O atributo '" k "' não pode redefinir um atributo herdado da superclasse '" anc-name "'.")))
    {:attribute (merge anc-attr (:attribute acc))
     :method    (merge anc-meth (:method acc))}))

(defn- flatten-class [class-map cls]
  (let [ancestors (collect-ancestors class-map (:name cls))
        initial   {:attribute (:attribute cls) :method (:method cls)}
        merged    (reduce #(merge-ancestor class-map %1 %2) initial ancestors)]
    (merge cls merged)))

(defn flatten-inheritance [class-map]
  (reduce (fn [acc [cname cls]]
            (assoc acc cname (flatten-class class-map cls)))
          {}
          class-map))

(defn conforms?
  [tipo-a tipo-b class-map current-class]
  (cond
    (= tipo-a tipo-b)      true
    (= tipo-b "Object")    true
    (= tipo-b "SELF_TYPE") false
    (= tipo-a "SELF_TYPE") (recur current-class tipo-b class-map current-class)
    (nil? tipo-a)          false
    :else                  (recur (:inherits (get class-map tipo-a)) tipo-b class-map current-class)))

(defn lca
  [tipo-a tipo-b class-map current-class]
  (if (= tipo-a tipo-b)
    tipo-a
    (let [ta     (if (= tipo-a "SELF_TYPE") current-class tipo-a)
          tb     (if (= tipo-b "SELF_TYPE") current-class tipo-b)
          ancs-a (set (conj (collect-ancestors class-map ta) ta))]
      (loop [curr tb]
        (cond
          (nil? curr)             "Object"
          (contains? ancs-a curr) curr
          :else                   (recur (:inherits (get class-map curr))))))))

(defn class-depth
  [class-map class-name]
  (count (collect-ancestors class-map class-name)))

(defn build-hierarchy [ast]
  (-> (:classes ast)
      (build-class-map)
      (validate-inheritance)
      (check-cycles)
      (ensure-main-class)
      (build-all-features)
      (flatten-inheritance)))
