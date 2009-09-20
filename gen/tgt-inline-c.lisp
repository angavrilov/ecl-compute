;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun compile-expr-generic (full_expr)
  (let ((types (derive-types full_expr))
        (args  ())
        (arg-types ())
        (arg-map (make-hash-table))
        (arr-map (make-hash-table)))
    (labels ((ref-symbol (sym)
               (if (get sym 'let-clause)
                   (temp-symbol-name sym)
                   (ref-arg sym)))
             (ref-arg (sym)
               (let ((sym-type (gethash sym types))
                     (sym-id (gethash sym arg-map)))
                 (unless sym-id
                   (setf sym-id (hash-table-count arg-map))
                   (setf (gethash sym arg-map) sym-id)
                   (push sym args)
                   (push (match sym-type
                           ('array :object)
                           ('float :float)
                           ('integer :int)
                           ('boolean :int)
                           (_ (error "Bad type ~A on input ~A" sym-type sym)))
                         arg-types))
                 (concatenate 'string "#" (get-inline-tag sym-id))))
             (use-array (arr dim)
               (let ((rarr (unwrap-factored arr)))
                 (setf (gethash rarr arr-map)
                       (max dim (or (gethash rarr arr-map) 0))))))
      (let* ((spec-compiler
              (form-compiler (form)
                ((type symbol sym)
                  (text (ref-symbol sym)))
                (`(multivalue-data ,@_)
                  (text (ref-arg form)))
                (`(arr-dim ,arr ,idx ,rank)
                  (use-array arr (1- rank))
                  (let ((arr-str (recurse-str arr)))
                    (if (= 1 rank)
                        (text "(~A)->vector.dim" arr-str)
                        (text "(~A)->array.dims[~A]" arr-str idx))))
                (`(arr-ptr ,arr)
                  (use-array arr 0)
                  (let ((arr-str (recurse-str arr)))
                    (text "VECTORP(~A)?(~A)->vector.self.sf:(~A)->array.self.sf"
                          arr-str arr-str arr-str)))
                (`(loop-range
                     ,(ranging-spec arg :min min :max max :delta 1 :ordered-p nil :loop-level 0)
                     ,@body)
                  (code ("{~%int ~A = " arg) min ";~%")
                  (let* ((ext-vars (make-hash-table :test #'equal))
                         (sse-code (compile-expr-sse types ext-vars `(progn ,@body))))
                    (when sse-code
                      (code "{")
                      (maphash #'(lambda (v n)
                                   (text "__m128 ~A_4 = _mm_set1_ps(~A);~%" v v))
                               ext-vars)
                      (code ("for (; ~A <= (" arg) max (")-3; ~A += 4) {~%" arg)
                            (:text sse-code)
                            "}}~%")))
                  (code ("for (; ~A <= " arg) max ("; ~A++) {~%" arg))
                  (dolist (cmd body)
                    (recurse cmd :stmt-p t))
                  (code "}}~%"))
                (`(loop-range
                     ,(ranging-spec arg :min min :max max :delta delta :loop-level level)
                     ,@body)
                  (when (eql level 0)
                    (let ((*print-ranges* t))
                      (format t "SSE inapplicable: ~A~%" (second form))))
                  (code ("{~%int ~A;~%for(~A = " arg arg)
                        (if (> delta 0) min max)
                        ("; ~A ~A " arg (if (> delta 0) "<=" ">="))
                        (if (> delta 0) max min)
                        ("; ~A += ~A) {~%" arg delta))
                  (dolist (cmd body)
                    (recurse cmd :stmt-p t))
                  (text "}}~%"))
                (`(safety-check ,checks ,@body)
                  (dolist (check checks)
                    (code "if (!(" (first check) "))~%"
                          ("    FEerror(\"Safety check failed: ~A\",0);~%"
                           (second check))))
                  (dolist (cmd body)
                    (recurse cmd :stmt-p t)))))

             (*cg-type-table* types)
             (*cg-full-expr* full_expr)
             (code (call-form-compilers (list spec-compiler
                                              #'compile-generic-c
                                              #'compile-generic
                                              #'compile-c-inline-temps)
                                        full_expr :stmt-p t))
             (checks (with-output-to-string (out)
                       (maphash #'(lambda (arr dim)
                                    (format out "{ cl_object arr = ~A; /* ~A */~%" (ref-arg arr) arr)
                                    (format out "if (!ARRAYP(arr) && !VECTORP(arr))~%  FEerror(\"Not an array: ~A\",0);~%" arr)
                                    (write-line "if ((VECTORP(arr)?arr->vector.elttype:arr->array.elttype)!=aet_sf)" out)
                                    (format out "  FEerror(\"Not a float array: ~A\",0);~%" arr)
                                    (when (> dim 0)
                                      (format out "if (!ARRAYP(arr) || arr->array.rank <= ~A)~%  FEerror(\"Too few dimensions: ~A\",0);~%"
                                              dim arr))
                                    (write-line "};" out))
                                arr-map))))
        ;; Generate the code
        `(progn
           (ffi:clines "#include <math.h>")
           (ffi:clines "#include <emmintrin.h>")
           (ffi:c-inline ,(nreverse args) ,(nreverse arg-types) :void
             ,(format nil "~A{~%~A}~%" checks code)))))))

(defun do-make-c-compute (original name idxspec expr
                          &key with where carrying parallel precompute cuda-flags)
  (let* ((*current-compute* original)
         (*simplify-cache* (make-hash-table))
         (*range-cache* (make-hash-table))
         (*minlevel-cache* (make-hash-table))
         (*canonify-cache* (make-canonify-cache))
         (*consistency-checks* (make-hash-table :test #'equal)))
    (multiple-value-bind (loop-expr loop-list range-list)
        (make-compute-loops name idxspec expr with where carrying precompute)
      ;; Apply optimizations
      (let* ((noiref-expr (pipeline loop-expr
                            expand-macros expand-let make-canonic
                            simplify-iref))
             ;; A table of all array references
             (ref-list    (collect-arefs noiref-expr))
             ;; Apply final transformations
             (res-expr    (pipeline noiref-expr
                            expand-aref preoptimize-tree
                            canonic-expr-unwrap
                            insert-checks)))
        ;; Generate the computation code
        (wrap-compute-sync-data :host ref-list
          (wrap-compute-parallel parallel range-list res-expr
                                 #'(lambda (code)
                                     `(let ((*current-compute* ',original))
                                        ,(compile-expr-generic
                                          (code-motion code :pull-symbols t))))))))))
