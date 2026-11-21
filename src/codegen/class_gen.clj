(ns codegen.class-gen
  (:require [codegen.asm :as asm]
            [codegen.expr :as expr]
            [codegen.types :as types])
  (:import [org.objectweb.asm ClassVisitor Opcodes]))

(defn- emit-fields! [^ClassVisitor cv cls]
  (doseq [attr (:own-attributes cls)]
    (doto (.visitField cv
                       Opcodes/ACC_PUBLIC
                       (types/field-name (:name attr))
                       (types/cool-type->desc (:declared-type attr))
                       nil
                       nil)
      (.visitEnd))))

(defn- emit-constructor!
  "Phase 1: calls super.<init>() and assigns Cool default values to own Int/String/Bool attributes."
  [^ClassVisitor cv cname parent-internal cls]
  (let [mv (.visitMethod cv Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL parent-internal "<init>" "()V" false)
    (doseq [attr (:own-attributes cls)]
      (let [dtype (:declared-type attr)]
        (when (contains? #{"Int" "String" "Bool"} dtype)
          (.visitVarInsn mv Opcodes/ALOAD 0)
          (asm/emit-default-value mv dtype)
          (.visitFieldInsn mv Opcodes/PUTFIELD
                           cname
                           (types/field-name (:name attr))
                           (types/cool-type->desc dtype)))))
    (.visitInsn mv Opcodes/RETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-cool-init!
  "Phase 2: calls super.cool$$init() then evaluates own attribute :init expressions in textual order."
  [^ClassVisitor cv cname parent-internal cls class-map source-file]
  (let [mv  (.visitMethod cv Opcodes/ACC_PUBLIC "cool$$init" "()V" nil nil)
        ctx {:mv            mv
             :class-map     class-map
             :current-class cname
             :source-file   source-file
             :locals        {}
             :next-slot     (atom 1)}]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL parent-internal "cool$$init" "()V" false)
    (doseq [attr (:own-attributes cls)]
      (when-let [init-expr (:init attr)]
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (expr/emit-expr init-expr ctx)
        (.visitFieldInsn mv Opcodes/PUTFIELD
                         cname
                         (types/field-name (:name attr))
                         (types/cool-type->desc (:declared-type attr)))))
    (.visitInsn mv Opcodes/RETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-new-instance-factory!
  "Virtual factory cool$$newInstance() used to implement `new SELF_TYPE` without reflection."
  [^ClassVisitor cv cname]
  (let [mv (.visitMethod cv Opcodes/ACC_PUBLIC "cool$$newInstance" (str "()" types/DESC-OBJECT) nil nil)]
    (.visitCode mv)
    (.visitTypeInsn mv Opcodes/NEW cname)
    (.visitInsn mv Opcodes/DUP)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL cname "<init>" "()V" false)
    (.visitInsn mv Opcodes/DUP)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL cname "cool$$init" "()V" false)
    (.visitInsn mv Opcodes/ARETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-method-pair!
  "Emits both:
   1. public static cool$static$<name>(C self, formals...) -> ret  (holds method body; used by static dispatch)
   2. public        cool$<name>(formals...) -> ret                (virtual delegator; used by dynamic dispatch)"
  [^ClassVisitor cv cname meth class-map source-file]
  (let [mname       (:name meth)
        formals     (:formals meth)
        static-name (types/static-method-name mname)
        static-desc (types/method-static-desc cname meth)
        virt-name   (types/virtual-method-name mname)
        virt-desc   (types/method-virtual-desc meth)
        locals      (into {} (map-indexed (fn [idx f] [(:name f) (inc idx)]) formals))]
    ;; 1. Static implementation
    (let [mv  (.visitMethod cv (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) static-name static-desc nil nil)
          ctx {:mv            mv
               :class-map     class-map
               :current-class cname
               :source-file   source-file
               :locals        locals
               :next-slot     (atom (inc (count formals)))}]
      (.visitCode mv)
      (expr/emit-expr (:body meth) ctx)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    ;; 2. Virtual delegator
    (let [mv (.visitMethod cv Opcodes/ACC_PUBLIC virt-name virt-desc nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (dotimes [i (count formals)]
        (.visitVarInsn mv Opcodes/ALOAD (inc i)))
      (.visitMethodInsn mv Opcodes/INVOKESTATIC cname static-name static-desc false)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))))

(defn- emit-jvm-main!
  "Emits `public static void main(String[] args)` on class Main using Main.main's actual return descriptor."
  [^ClassVisitor cv class-map]
  (let [main-meth (get-in class-map ["Main" :method "main"])
        main-desc (types/method-virtual-desc main-meth)
        mv        (.visitMethod cv (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "main" "([Ljava/lang/String;)V" nil nil)]
    (.visitCode mv)
    (.visitTypeInsn mv Opcodes/NEW "Main")
    (.visitInsn mv Opcodes/DUP)
    (.visitMethodInsn mv Opcodes/INVOKESPECIAL "Main" "<init>" "()V" false)
    (.visitInsn mv Opcodes/DUP)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "Main" "cool$$init" "()V" false)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "Main" (types/virtual-method-name "main") main-desc false)
    (.visitInsn mv Opcodes/POP)
    (.visitInsn mv Opcodes/RETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn generate-class-bytes
  "Generates the JVM .class bytecode byte[] for a user-defined Cool class."
  ^bytes [cls class-map source-file]
  (let [cname           (:name cls)
        parent-internal (types/cool-type->internal (:inherits cls))
        cw              (asm/make-class-writer class-map)
        cv              (asm/wrap-structural-check cw)]
    (.visit cv asm/BYTECODE-VERSION (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER) cname nil parent-internal nil)
    (.visitSource cv source-file nil)
    (emit-fields! cv cls)
    (emit-constructor! cv cname parent-internal cls)
    (emit-cool-init! cv cname parent-internal cls class-map source-file)
    (emit-new-instance-factory! cv cname)
    (doseq [meth (:own-methods cls)]
      (emit-method-pair! cv cname meth class-map source-file))
    (when (= cname "Main")
      (emit-jvm-main! cv class-map))
    (.visitEnd cv)
    (.toByteArray cw)))
