(ns codegen.expr
  (:require [ast]
            [codegen.asm :as asm]
            [codegen.types :as types]
            [semantic.hierarchy :as hierarchy])
  (:import [ast IntLit StringLit BoolLit Id Assign If While Block
                Let Case New Call Dispatch BinaryOp UnaryOp]
           [org.objectweb.asm Label MethodVisitor Opcodes]))

(defn- alloc-slot! [ctx]
  (let [slot @(:next-slot ctx)]
    (swap! (:next-slot ctx) inc)
    slot))

(defmulti emit-expr
  "Emits JVM bytecode for an annotated Cool AST expression node.
   Invariant: every expression leaves exactly 1 CoolObject (or subtype) reference on the operand stack."
  (fn [node _ctx] (type node)))

(defmethod emit-expr :default [node _ctx]
  (throw (ex-info (str "Nó AST não suportado no codegen: " (type node)) {:node node})))

(defmethod emit-expr IntLit [node {:keys [^MethodVisitor mv]}]
  (asm/emit-push-int mv (types/parse-cool-int (:value node)))
  (asm/emit-box-int mv))

(defmethod emit-expr StringLit [node {:keys [^MethodVisitor mv]}]
  (.visitLdcInsn mv (types/unescape-cool-string (:value node)))
  (.visitMethodInsn mv Opcodes/INVOKESTATIC
                    types/COOL-STRING "of" (str "(Ljava/lang/String;)" types/DESC-STRING) false))

(defmethod emit-expr BoolLit [node {:keys [^MethodVisitor mv]}]
  (if (types/parse-cool-bool (:value node))
    (.visitFieldInsn mv Opcodes/GETSTATIC types/COOL-BOOL "TRUE" types/DESC-BOOL)
    (.visitFieldInsn mv Opcodes/GETSTATIC types/COOL-BOOL "FALSE" types/DESC-BOOL)))

(defmethod emit-expr Id [node {:keys [^MethodVisitor mv class-map current-class locals]}]
  (let [var-name (:name node)]
    (cond
      (= var-name "self")
      (.visitVarInsn mv Opcodes/ALOAD 0)

      (contains? locals var-name)
      (.visitVarInsn mv Opcodes/ALOAD (int (get locals var-name)))

      :else
      (let [attr  (get-in class-map [current-class :attribute var-name])
            owner (types/cool-type->internal (:defining-class attr))
            fname (types/field-name var-name)
            fdesc (types/cool-type->desc (:declared-type attr))]
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (.visitFieldInsn mv Opcodes/GETFIELD owner fname fdesc)
        (when (= (:declared-type attr) "SELF_TYPE")
          (asm/emit-checkcast-if-needed mv (types/cool-type->internal current-class)))))))

(defmethod emit-expr Assign [node {:keys [^MethodVisitor mv class-map current-class locals] :as ctx}]
  (let [var-name (:name node)]
    (if (contains? locals var-name)
      (do
        (emit-expr (:expr node) ctx)
        (.visitInsn mv Opcodes/DUP)
        (.visitVarInsn mv Opcodes/ASTORE (int (get locals var-name))))
      (let [attr  (get-in class-map [current-class :attribute var-name])
            owner (types/cool-type->internal (:defining-class attr))
            fname (types/field-name var-name)
            fdesc (types/cool-type->desc (:declared-type attr))]
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (emit-expr (:expr node) ctx)
        (.visitInsn mv Opcodes/DUP_X1)
        (.visitFieldInsn mv Opcodes/PUTFIELD owner fname fdesc)))))

(defmethod emit-expr Block [node {:keys [^MethodVisitor mv] :as ctx}]
  (let [exprs (:body node)]
    (if (empty? exprs)
      (.visitInsn mv Opcodes/ACONST_NULL)
      (do
        (doseq [e (butlast exprs)]
          (emit-expr e ctx)
          (.visitInsn mv Opcodes/POP))
        (emit-expr (last exprs) ctx)))))

(defmethod emit-expr If [node {:keys [^MethodVisitor mv] :as ctx}]
  (let [l-else (Label.)
        l-end  (Label.)]
    (emit-expr (:cond node) ctx)
    (asm/emit-unbox-bool mv)
    (.visitJumpInsn mv Opcodes/IFEQ l-else)
    (emit-expr (:then node) ctx)
    (.visitJumpInsn mv Opcodes/GOTO l-end)
    (.visitLabel mv l-else)
    (emit-expr (:else node) ctx)
    (.visitLabel mv l-end)))

(defmethod emit-expr While [node {:keys [^MethodVisitor mv] :as ctx}]
  (let [l-cond (Label.)
        l-end  (Label.)]
    (.visitLabel mv l-cond)
    (emit-expr (:cond node) ctx)
    (asm/emit-unbox-bool mv)
    (.visitJumpInsn mv Opcodes/IFEQ l-end)
    (emit-expr (:body node) ctx)
    (.visitInsn mv Opcodes/POP)
    (.visitJumpInsn mv Opcodes/GOTO l-cond)
    (.visitLabel mv l-end)
    (.visitInsn mv Opcodes/ACONST_NULL)))

(defmethod emit-expr Let [node {:keys [^MethodVisitor mv] :as ctx}]
  (let [final-ctx (reduce (fn [cur-ctx binding]
                            (if-let [init-expr (:init binding)]
                              (emit-expr init-expr cur-ctx)
                              (asm/emit-default-value mv (:declared-type binding)))
                            (let [slot (alloc-slot! cur-ctx)]
                              (.visitVarInsn mv Opcodes/ASTORE (int slot))
                              (assoc-in cur-ctx [:locals (:name binding)] slot)))
                          ctx
                          (:bindings node))]
    (emit-expr (:body node) final-ctx)))

(defmethod emit-expr New [node {:keys [^MethodVisitor mv current-class]}]
  (let [t (:name node)]
    (cond
      (= t "SELF_TYPE")
      (do
        (.visitVarInsn mv Opcodes/ALOAD 0)
        (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL
                          types/COOL-OBJECT "cool$$newInstance" (str "()" types/DESC-OBJECT) false)
        (asm/emit-checkcast-if-needed mv (types/cool-type->internal current-class)))

      (contains? #{"Int" "String" "Bool"} t)
      (asm/emit-default-value mv t)

      :else
      (let [internal (types/cool-type->internal t)]
        (.visitTypeInsn mv Opcodes/NEW internal)
        (.visitInsn mv Opcodes/DUP)
        (.visitMethodInsn mv Opcodes/INVOKESPECIAL internal "<init>" "()V" false)
        (.visitInsn mv Opcodes/DUP)
        (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL internal "cool$$init" "()V" false)))))

(defmethod emit-expr UnaryOp [node {:keys [^MethodVisitor mv] :as ctx}]
  (emit-expr (:expr node) ctx)
  (case (:op node)
    :intcomplement
    (do
      (asm/emit-unbox-int mv)
      (.visitInsn mv Opcodes/INEG)
      (asm/emit-box-int mv))

    :not
    (.visitMethodInsn mv Opcodes/INVOKESTATIC
                      types/COOL-RUNTIME "not" (str "(" types/DESC-BOOL ")" types/DESC-BOOL) false)

    :isvoid
    (.visitMethodInsn mv Opcodes/INVOKESTATIC
                      types/COOL-RUNTIME "isVoid" (str "(" types/DESC-OBJECT ")" types/DESC-BOOL) false)))

(defmethod emit-expr BinaryOp [node {:keys [^MethodVisitor mv source-file] :as ctx}]
  (let [op (:op node)]
    (case op
      (:sum :sub :mult)
      (do
        (emit-expr (:left node) ctx)
        (asm/emit-unbox-int mv)
        (emit-expr (:right node) ctx)
        (asm/emit-unbox-int mv)
        (.visitInsn mv (case op
                         :sum  Opcodes/IADD
                         :sub  Opcodes/ISUB
                         :mult Opcodes/IMUL))
        (asm/emit-box-int mv))

      :div
      (do
        (emit-expr (:left node) ctx)
        (asm/emit-unbox-int mv)
        (emit-expr (:right node) ctx)
        (asm/emit-unbox-int mv)
        (.visitLdcInsn mv source-file)
        (asm/emit-push-int mv (or (:row node) 0))
        (.visitMethodInsn mv Opcodes/INVOKESTATIC
                          types/COOL-RUNTIME "div" (str "(IILjava/lang/String;I)" types/DESC-INT) false))

      (:lt :leq)
      (do
        (emit-expr (:left node) ctx)
        (asm/emit-unbox-int mv)
        (emit-expr (:right node) ctx)
        (asm/emit-unbox-int mv)
        (.visitMethodInsn mv Opcodes/INVOKESTATIC
                          types/COOL-RUNTIME (if (= op :lt) "lt" "leq") (str "(II)" types/DESC-BOOL) false))

      :eq
      (do
        (emit-expr (:left node) ctx)
        (emit-expr (:right node) ctx)
        (.visitMethodInsn mv Opcodes/INVOKESTATIC
                          types/COOL-RUNTIME "coolEquals"
                          (str "(" types/DESC-OBJECT types/DESC-OBJECT ")" types/DESC-BOOL)
                          false)))))

(defmethod emit-expr Call [node {:keys [^MethodVisitor mv class-map current-class] :as ctx}]
  (let [mname          (:method node)
        meth           (get-in class-map [current-class :method mname])
        owner-internal (types/cool-type->internal current-class)]
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (doseq [arg (:args node)]
      (emit-expr arg ctx))
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL
                      owner-internal
                      (types/virtual-method-name mname)
                      (types/method-virtual-desc meth)
                      false)
    (when (= (:return-type meth) "SELF_TYPE")
      (asm/emit-checkcast-if-needed mv owner-internal))))

(defmethod emit-expr Dispatch [node {:keys [^MethodVisitor mv class-map current-class source-file] :as ctx}]
  (let [args              (:args node)
        static-type       (:static-type node)
        obj-node          (:obj node)
        obj-type          (:inferred-type obj-node)
        concrete-obj-type (types/resolve-cool-type obj-type current-class)
        target-type       (or static-type concrete-obj-type)
        mname             (:method node)
        meth              (get-in class-map [target-type :method mname])
        ;; Cool Manual §13.3: evaluate arguments e1..en left-to-right BEFORE receiver e0
        arg-slots         (mapv (fn [arg]
                                  (emit-expr arg ctx)
                                  (let [slot (alloc-slot! ctx)]
                                    (.visitVarInsn mv Opcodes/ASTORE (int slot))
                                    slot))
                                args)]
    ;; Evaluate receiver e0 last, check non-void via dup (preserving exact static type on stack)
    (emit-expr obj-node ctx)
    (.visitInsn mv Opcodes/DUP)
    (.visitLdcInsn mv source-file)
    (asm/emit-push-int mv (or (:row node) 0))
    (.visitMethodInsn mv Opcodes/INVOKESTATIC
                      types/COOL-RUNTIME "checkDispatchNotVoid"
                      (str "(" types/DESC-OBJECT "Ljava/lang/String;I)V")
                      false)
    ;; Reload evaluated arguments onto the operand stack
    (doseq [slot arg-slots]
      (.visitVarInsn mv Opcodes/ALOAD (int slot)))
    ;; Invoke static or dynamic target
    (if static-type
      (let [defining-class (:defining-class meth)
            owner-internal (types/cool-type->internal defining-class)]
        (.visitMethodInsn mv Opcodes/INVOKESTATIC
                          owner-internal
                          (types/static-method-name mname)
                          (types/method-static-desc defining-class meth)
                          false))
      (let [owner-internal (types/cool-type->internal target-type)]
        (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL
                          owner-internal
                          (types/virtual-method-name mname)
                          (types/method-virtual-desc meth)
                          false)))
    ;; Cast SELF_TYPE return back to the receiver's resolved static type
    (when (= (:return-type meth) "SELF_TYPE")
      (asm/emit-checkcast-if-needed mv (types/cool-type->internal concrete-obj-type)))))

(defmethod emit-expr Case [node {:keys [^MethodVisitor mv class-map source-file] :as ctx}]
  (emit-expr (:expr node) ctx)
  (.visitInsn mv Opcodes/DUP)
  (.visitLdcInsn mv source-file)
  (asm/emit-push-int mv (or (:row node) 0))
  (.visitMethodInsn mv Opcodes/INVOKESTATIC
                    types/COOL-RUNTIME "checkCaseNotVoid"
                    (str "(" types/DESC-OBJECT "Ljava/lang/String;I)V")
                    false)
  (let [val-slot (alloc-slot! ctx)
        sorted   (sort-by #(- (hierarchy/class-depth class-map (:declared-type %)))
                          (:cases node))
        l-end    (Label.)]
    (.visitVarInsn mv Opcodes/ASTORE (int val-slot))
    (doseq [branch sorted]
      (let [l-next          (Label.)
            branch-internal (types/cool-type->internal (:declared-type branch))
            b-slot          (alloc-slot! ctx)]
        (.visitVarInsn mv Opcodes/ALOAD (int val-slot))
        (.visitTypeInsn mv Opcodes/INSTANCEOF branch-internal)
        (.visitJumpInsn mv Opcodes/IFEQ l-next)
        (.visitVarInsn mv Opcodes/ALOAD (int val-slot))
        (asm/emit-checkcast-if-needed mv branch-internal)
        (.visitVarInsn mv Opcodes/ASTORE (int b-slot))
        (emit-expr (:body branch) (assoc-in ctx [:locals (:name branch)] b-slot))
        (.visitJumpInsn mv Opcodes/GOTO l-end)
        (.visitLabel mv l-next)))
    ;; Fallthrough: no matching branch -> abort via CoolRuntime.caseNoMatch + athrow
    (.visitVarInsn mv Opcodes/ALOAD (int val-slot))
    (.visitLdcInsn mv source-file)
    (asm/emit-push-int mv (or (:row node) 0))
    (.visitMethodInsn mv Opcodes/INVOKESTATIC
                      types/COOL-RUNTIME "caseNoMatch"
                      (str "(" types/DESC-OBJECT "Ljava/lang/String;I)Ljava/lang/RuntimeException;")
                      false)
    (.visitInsn mv Opcodes/ATHROW)
    (.visitLabel mv l-end)))
