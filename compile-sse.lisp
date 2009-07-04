;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun get-factored-cg-type (expr)
    (gethash (unwrap-factored expr) *cg-type-table*))

(defun is-level0-ptr (form)
    (match form
        ((type symbol sym)
            (let ((level (get sym 'loop-level 'unknown)))
                (when (eql level 'unknown)
                    (error "Unmarked symbol in SSE ptr: ~A" sym))
                (if (eql level 0)
                    (is-level0-ptr (unwrap-factored sym))
                    nil)))
        ((type number num)
            nil)
        (`(ranging ,v ,@_)
            (eql (ranging-loop-level form) 0))
        (`(ptr+ ,ptr ,ofs)
            (let ((ptr-0 (is-level0-ptr ptr))
                  (ofs-0 (is-level0-ptr ofs)))
                (if (and ptr-0 ofs-0)
                    (error "Pointer too complex in SSE: ~A" form))
                (or ptr-0 ofs-0)))
        (`(,(or '+ '-) ,a ,(type number b))
            (is-level0-ptr a))
        (`(+ ,(type number b) ,a)
            (is-level0-ptr a))
        (_
            (error "Expression too complex in SSE ptr: ~A" form))))

(def-form-compiler compile-sse-ptr-sym (form)
    ((type symbol sym)
        (let ((level (get sym 'loop-level))
              (stype (get-factored-cg-type sym)))
            (if (and (eql level 0)
                     (not (eql stype 'float-ptr)))
                (recurse (unwrap-factored sym))
                (text (temp-symbol-name sym))))))

(defun compile-sse-ptr (ptr)
    (call-form-compilers
        (list #'compile-sse-ptr-sym
              #'compile-generic-c)
        ptr))

(def-form-compiler compile-sse-temps (form)
    (`(setf-tmp ,var ,expr)
        (let ((var-type (gethash expr *cg-type-table*))
              (var-name (temp-symbol-name var))
              (var-fdiv (or (get var 'fdiv-users) 0)))
            (text "~A ~A = ("
                (case var-type
                    (float "__m128")
                    (boolean "__m128")
                    (float-ptr "float*")
                    ((integer boolean)
                        ;; Will be inlined, so abort assignment
                        (return-from compile-sse-temps))
                    (t
                        (error "Bad type ~A of ~A in SSE ~A"
                            var-type expr full_expr)))
                var-name)
            (case var-type
                (float-ptr
                    (text (compile-sse-ptr expr)))
                (t
                    (recurse expr)))
            (text ");~%")
            (when (> var-fdiv 1)
                (text "__m128 ~A_fdiv = _mm_div_ps(_mm_set1_ps(1.0),~A);~%"
                    var-name var-name)))))

(defparameter *cg-sse-extvars* nil)
(defparameter *cg-sse-auxvars* nil)

(defun ref-sse-extvar (name)
    (incf-nil (gethash name *cg-sse-extvars*))
    (format nil "~A_4" name))

(defun make-sse-auxvar ()
    (let* ((tmp-var (gensym))
           (tmp-name (symbol-name tmp-var)))
        (push tmp-name *cg-sse-auxvars*)
        (values tmp-name tmp-var)))

(def-form-compiler compile-float-sse (form form-type stmt-p)
    ((type symbol sym)
        (let ((sym-type (get-factored-cg-type sym))
              (sym-level (get sym 'loop-level))
              (sym-name (temp-symbol-name sym)))
            (cond
                ((and (eql sym-level 0)
                      (or (eql sym-type 'float)
                          (eql sym-type 'boolean)))
                    (text sym-name))
                ((eql sym-level 0)
                    (recurse
                        (unwrap-factored sym)))
                ((eql sym-type 'boolean)
                    (error "Boolean -> sse cast not implemented"))
                (t
                    (text (ref-sse-extvar sym-name))))))

    ((when (= val 0) (type number val))
        (text "_mm_setzero_ps()"))
    ((type atom val)
        (text "_mm_set1_ps(~A)" val))

    (`(ranging ,v ,@_)
        (if (eql (ranging-loop-level form) 0)
            (text
                "_mm_add_ps(_mm_set1_ps(~A),_mm_setr_ps(0,1,2,3))"
                (symbol-name v))
            (text
                (ref-sse-extvar (symbol-name v)))))

    ((when (> (or (get sym 'fdiv-users) 0) 1)
         `(/ ,x ,(type symbol sym)))
        (if (and (numberp x)
                 (= x 1))
            (text "(")
            (progn
                (text "_mm_mul_ps(")
                (recurse x)
                (text ",")))
        (let ((fd-name (format nil "~A_fdiv" (temp-symbol-name sym))))
            (if (eql (get sym 'loop-level) 0)
                (text fd-name)
                (text (ref-sse-extvar fd-name))))
        (text ")"))

    (`(/ ,x ,(type number num))
        (text "_mm_mul_ps(")
        (recurse x)
        (text ",_mm_set1_ps(1.0/~A))" num))

    (`(/ ,(type number num))
        (text "_mm_set1_ps(1.0/~A)" num))

    (`(/ ,x)
        (text "_mm_div_ps(_mm_set1_ps(1.0),")
        (recurse x)
        (text ")"))

    (`(,(as op (or '+ '- '* '/ 'max 'min
                   'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
        (text "~A("
            (case op
                (+ "_mm_add_ps")
                (- "_mm_sub_ps")
                (* "_mm_mul_ps")
                (/ "_mm_div_ps")
                (< "_mm_cmplt_ps")
                (> "_mm_cmpgt_ps")
                (<= "_mm_cmple_ps")
                (>= "_mm_cmpge_ps")
                (/= "_mm_cmpneq_ps")
                (= "_mm_cmpeq_ps")
                (and "_mm_and_ps")
                (or "_mm_or_ps")
                (max "_mm_max_ps")
                (min "_mm_min_ps")))
        (recurse a)
        (text ",")
        (recurse b)
        (text ")"))

    (`(+ ,a)
        (recurse a))

    (`(- ,a)
        (text "_mm_sub_ps(_mm_setzero_ps(),")
        (recurse a)
        (text ")"))

    (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt)) ,@_)
        (error "Functions not supported in SSE: ~A" func))

    (`(ptr-deref ,ptr)
        (if (is-level0-ptr ptr)
            (text "_mm_loadu_ps(~A)"
                (compile-sse-ptr ptr))
            (text "_mm_set1_ps(*(~A))"
                (compile-sse-ptr ptr))))

    ((when (and (= a 0) (= b 0))
        `(if ,_ ,(type number a) ,(type number b)))
        (text "_mm_setzero_ps()"))

    ((when (or (and (numberp a) (= a 0))
               (and (numberp b) (= b 0)))
         `(if ,icond ,a ,b))
        (let ((a-zero (and (numberp a) (= a 0))))
            (text
                (if a-zero "_mm_andnot_ps(" "_mm_and_ps("))
            (recurse icond)
            (text ",~%")
            (recurse (if a-zero b a))
            (text ")")))

    (`(if ,icond ,a ,b)
        (let ((tmp-name (make-sse-auxvar)))
            (text "(~A = (" tmp-name)
            (recurse icond)
            (text "),~%_mm_or_ps(_mm_and_ps(~A," tmp-name)
            (recurse a)
            (text "),~%_mm_andnot_ps(~A," tmp-name)
            (recurse b)
            (text ")~%))")))

    ((when stmt-p
         `(setf (ptr-deref ,target) ,expr))
        (unless (is-level0-ptr target)
            (error "Store to a non-inner ptr in SSE: ~A" form))
        (text "_mm_storeu_ps(~A,~%"
            (compile-sse-ptr target))
        (recurse expr)
        (text ");~%"))

    (`(setf (ptr-deref ,target) ,expr)
        (unless (is-level0-ptr target)
            (error "Store to a non-inner ptr in SSE: ~A" form))
        (let ((tmp-name (make-sse-auxvar)))
            (text "(~A = (" tmp-name)
            (recurse expr)
            (text "),~%_mm_storeu_ps(~A, ~A), ~A)"
                (compile-sse-ptr target)
                tmp-name tmp-name))))

(defun compile-expr-sse (types ext-vars full_expr)
    (let ((*cg-sse-auxvars* ())
          (*cg-sse-extvars* ext-vars))
        (handler-bind ((condition
               #'(lambda (cond)
                     (format t "~%SSE compilation failed:~%   ~A~%" cond)
                     (format t "Reverting to ordinary C.~%")
                     (return-from compile-expr-sse nil))))
            (let ((code (call-form-compilers
                            (list #'compile-float-sse
                                  #'compile-sse-temps
                                  #'compile-generic)
                            full_expr :stmt-p t))
                  (vars (if (null *cg-sse-auxvars*) ""
                            (format nil "__m128 ~{~A~^, ~};~%"
                                *cg-sse-auxvars*))))
                (concatenate 'string vars code)))))

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
                         (`(arr-dim ,arr ,idx)
                             (use-array arr idx)
                             (let ((arr-str (recurse-str arr)))
                                 (when (= 0 idx)
                                     (text "VECTORP(~A)?(~A)->vector.dim:"
                                         arr-str arr-str))
                                 (text "(~A)->array.dims[~A]"
                                     arr-str idx)))
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
                               &key with where carrying parallel cluster-cache)
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*consistency-checks* (make-hash-table :test #'equal)))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with where carrying cluster-cache)
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
