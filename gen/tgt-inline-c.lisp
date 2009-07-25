;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

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
                             (push
                                 (match sym-type
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
            (let*
                ((spec-compiler
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
                                     (text "(~A)->array.dims[~A]"
                                         arr-str idx))))
                         (`(arr-ptr ,arr)
                             (use-array arr 0)
                             (let ((arr-str (recurse-str arr)))
                                 (text "VECTORP(~A)?(~A)->vector.self.sf:(~A)->array.self.sf"
                                     arr-str arr-str arr-str)))
                         (`(loop-range
                              (ranging ,arg ,min ,max 1 nil 0 ,@_)
                              ,@body)
                             (text "{~%int ~A = " arg)
                             (recurse min)
                             (text ";~%")
                             (let* ((ext-vars (make-hash-table :test #'equal))
                                    (sse-code (compile-expr-sse types ext-vars `(progn ,@body))))
                                 (when sse-code
                                     (text "{")
                                     (maphash #'(lambda (v n)
                                                    (text "__m128 ~A_4 = _mm_set1_ps(~A);~%" v v))
                                         ext-vars)
                                     (text "for (; ~A <= (" arg)
                                     (recurse max)
                                     (text ")-3; ~A += 4) {~%" arg)
                                     (text sse-code)
                                     (text "}}~%")))
                             (text "for (; ~A <= " arg)
                             (recurse max)
                             (text "; ~A++) {~%" arg)
                             (dolist (cmd body)
                                 (recurse cmd :stmt-p t))
                             (text "}}~%"))
                         (`(loop-range
                              (ranging ,arg ,min ,max ,delta ,@_)
                              ,@body)
                             (when (eql (ranging-loop-level (second form)) 0)
                                 (format t "SSE inapplicable: ~A~%" (second form)))
                             (text "{~%int ~A;~%for(~A = " arg arg)
                             (recurse (if (> delta 0) min max))
                             (text "; ~A ~A " arg (if (> delta 0) "<=" ">="))
                             (recurse (if (> delta 0) max min))
                             (text "; ~A += ~A) {~%" arg delta)
                             (dolist (cmd body)
                                 (recurse cmd :stmt-p t))
                             (text "}}~%"))
                         (`(safety-check ,checks ,@body)
                             (dolist (check checks)
                                 (text "if (!(")
                                 (recurse (first check))
                                 (text "))~%    FEerror(\"Safety check failed: ~A\",0);~%"
                                     (second check)))
                             (dolist (cmd body)
                                 (recurse cmd :stmt-p t)))))

                 (*cg-type-table* types)
                 (*cg-full-expr* full_expr)
                 (code (call-form-compilers
                           (list spec-compiler
                                 #'compile-generic-c
                                 #'compile-generic
                                 #'compile-c-inline-temps)
                           full_expr :stmt-p t))
                 (checks (with-output-to-string (out)
                             (maphash
                                 #'(lambda (arr dim)
                                       (format out "{ cl_object arr = ~A; /* ~A */~%"
                                           (ref-arg arr) arr)
                                       (format out
                                           "if (!ARRAYP(arr) && !VECTORP(arr))~%  FEerror(\"Not an array: ~A\",0);~%"
                                           arr)
                                       (write-line
                                           "if ((VECTORP(arr)?arr->vector.elttype:arr->array.elttype)!=aet_sf)"
                                           out)
                                       (format out "  FEerror(\"Not a float array: ~A\",0);~%" arr)
                                       (when (> dim 0)
                                           (format out
                                               "if (!ARRAYP(arr) || arr->array.rank <= ~A)~%  FEerror(\"Too few dimensions: ~A\",0);~%"
                                               dim arr))
                                       (write-line "};" out))
                                   arr-map))))
                ;; Generate the code
                `(progn
                    (ffi:clines "#include <math.h>")
                    (ffi:clines "#include <emmintrin.h>")
                    (ffi:c-inline ,(nreverse args) ,(nreverse arg-types)
                         :void ,(format nil "~A{~%~A}~%" checks code)))))))

(defun do-make-c-compute (original name idxspec expr
                               &key with where carrying parallel precompute cuda-flags)
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*consistency-checks* (make-hash-table :test #'equal)))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with where carrying precompute)
            (let* ((nomacro-expr (expand-macros loop-expr))
                   (nolet-expr   (expand-let nomacro-expr))
                   (noiref-expr (simplify-iref nolet-expr))
                   (ref-list    (collect-arefs noiref-expr))
                  ; (opt-expr    (optimize-tree noiref-expr))
                   (noaref-expr (expand-aref noiref-expr))
                   (check-expr  (insert-checks noaref-expr)))
                (wrap-compute-sync-data :host ref-list
                    (wrap-compute-parallel parallel range-list check-expr
                        #'(lambda (code)
                             `(let ((*current-compute* ',original))
                                  ,(compile-expr-generic
                                       (code-motion code :pull-symbols t))))))))))
