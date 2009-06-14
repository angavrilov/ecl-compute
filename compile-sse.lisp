;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun expand-aref-1 (expr old-expr)
    (match expr
        (`(aref ,name ,@idxvals)
            (let* ((idx-cnt    (length idxvals))
                   (stride     nil)
                   (stride-lst
                       (loop for i from (1- idx-cnt) downto 0
                        collect (prog1 stride
                                   (let ((cstride `(arr-dim ,name ,i)))
                                       (setf stride
                                           (if stride
                                               `(* ,stride ,cstride)
                                               cstride))))))
                   (ofs-lst (mapcar #'(lambda (idx istride)
                                          (if istride `(* ,idx ,istride) idx))
                                idxvals (nreverse stride-lst))))
                `(ptr-deref
                     ,(reduce #'(lambda (base ofs) `(ptr+ ,base ,ofs))
                            ofs-lst :initial-value `(arr-ptr ,name)))))
        (_ nil)))

(defun expand-aref (expr)
    (simplify-rec-once #'expand-aref-1 expr))

(defun expand-macros (expr)
    (match expr
        ((type atom _) expr)
        (`(declare ,@_) expr)
        (`(1+ ,v)
            (expand-macros `(+ ,v 1)))
        (`(1- ,v)
            (expand-macros `(- ,v 1)))
        (`(expt ,v 1)
            (expand-macros v))
        (`(expt ,v 2)
            (expand-macros `(* ,v ,v)))
        ((cons (or 'ranging 'aref 'iref '_grp
                   '+ '- '* '/ 'mod 'rem 'floor 'ceiling 'truncate
                   'and 'or 'if 'progn
                   'sin 'cos 'exp 'expt
                   '> '< '>= '<= '/= '= 'setf 'loop-range) tail)
            (cons-save-old expr (car expr)
                (mapcar-save-old #'expand-macros tail)))
        (`(,(as op (or 'let 'let*)) ,vars ,@body)
            (cons-save-old expr op
                (cons-save-old (cdr expr)
                    (mapcar-save-old
                        #'(lambda (pair)
                              (if (symbolp pair) (cons pair nil)
                                  (cons-save-old pair
                                      (car pair)
                                      (mapcar-save-old #'expand-macros (cdr pair)))))
                        vars)
                    (mapcar-save-old #'expand-macros body))))
        (_
            (multiple-value-bind (res macro) (macroexpand expr)
                (if macro
                    (expand-macros res)
                    (error "Unknown form in compute: ~A" res))))))

(defun get-inline-tag (idx)
    (aref #("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
            "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
            "u" "v" "w" "x" "y" "z") idx))

(defun compile-expr-sse (types ext-vars full_expr)
    (let ((aux-vars ()))
        (labels ((get-type (expr)
                     (gethash (unwrap-factored expr) types))
                 (ref-extvar (name)
                     (incf-nil (gethash name ext-vars))
                     (format nil "~A_4" name))
                 (ref-symbol (sym)
                     (if (get sym 'let-clause)
                         (concatenate 'string "tmp_" (symbol-name sym))
                         (error "Non-local symbol in SSE: ~A" sym)))
                 (compile-temp-assn (out var expr)
                     (let ((var-type (gethash expr types))
                           (var-name (concatenate 'string "tmp_"
                                         (symbol-name var)))
                           (var-fdiv (or (get var 'fdiv-users) 0)))
                         (write-string
                             (case var-type
                                 (float "__m128")
                                 (boolean "__m128")
                                 (float-ptr "float*")
                                 ((integer boolean)
                                     (return-from compile-temp-assn))
                                 (t
                                     (error "Bad type ~A of ~A in SSE ~A"
                                         var-type expr full_expr)))
                             out)
                         (write-string " " out)
                         (write-string var-name out)
                         (write-string " = (" out)
                         (case var-type
                             (float-ptr
                                 (compile-form-ptr out expr))
                             (t
                                 (compile-form-float out expr)))
                         (format out ");~%")
                         (when (> var-fdiv 1)
                             (format out
                                 "__m128 ~A_fdiv = _mm_div_ps(_mm_set1_ps(1.0),~A);~%"
                                 var-name var-name))))
                 (is-level0-ptr (form)
                     (match form
                         ((type symbol sym)
                             (let ((level (get sym 'loop-level 'unknown)))
                                 (when (eql level 'unknown)
                                     (error "Unmarked symbol in SSE ptr: ~A" sym))
                                 (if (eql level 0)
                                     (is-level0-ptr (unwrap-factored sym))
                                     nil)))
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
                 (compile-form-ptr (out form)
                     (match form
                         ((type symbol sym)
                             (let ((level (get sym 'loop-level))
                                   (stype (get-type sym)))
                                 (if (and (eql level 0)
                                          (not (eql stype 'float-ptr)))
                                     (compile-form-ptr out
                                         (unwrap-factored sym))
                                     (write-string (ref-symbol sym) out))))
                         (`(ranging ,v ,@_)
                             (write-string (symbol-name v) out))
                         (`(ptr+ ,ptr ,ofs)
                             (write-string "(" out)
                             (compile-form-ptr out ptr)
                             (write-string ")+(" out)
                             (compile-form-ptr out ofs)
                             (write-string ")" out))
                         ((or
                             `(,(as op (or '+ '-)) ,a ,(type number b))
                             `(,(as op '+) ,(type number b) ,a))
                             (write-string "(" out)
                             (compile-form-ptr out a)
                             (format out ")~A(~A)" op b))
                         (_
                             (error "Invalid ptr form: ~A" form))))
                 (compile-form-float (out form)
                     (match form
                         ((type symbol sym)
                             (let ((sym-type (get-type sym))
                                   (sym-level (get sym 'loop-level))
                                   (sym-name (ref-symbol sym)))
                                 (cond
                                     ((and (eql sym-level 0)
                                           (or (eql sym-type 'float)
                                               (eql sym-type 'boolean)))
                                         (write-string sym-name out))
                                     ((eql sym-level 0)
                                         (compile-form-float out
                                             (unwrap-factored sym)))
                                     ((eql sym-type 'boolean)
                                         (error "Boolean -> sse cast not implemented"))
                                     (t
                                         (write-string (ref-extvar sym-name) out)))))
                         ((when (= val 0) (type number val))
                             (write-string "_mm_setzero_ps()" out))
                         ((type atom val)
                             (format out "_mm_set1_ps(~A)" val))
                         (`(declare ,@_)
                             (write-string "0" out))
                         (`(_grp ,x)
                             (compile-form-float out x))
                         (`(ranging ,v ,@_)
                             (if (eql (ranging-loop-level form) 0)
                                 (format out
                                     "_mm_add_ps(_mm_set1_ps(~A),_mm_setr_ps(0,1,2,3))"
                                     (symbol-name v))
                                 (write-string (ref-extvar (symbol-name v)) out)))
                         ((when (> (or (get sym 'fdiv-users) 0) 1)
                             `(/ ,x ,(type symbol sym)))
                             (if (and (numberp x)
                                      (= x 1))
                                 (write-string "(" out)
                                 (progn
                                     (write-string "_mm_mul_ps(" out)
                                     (compile-form-float out x)
                                     (write-string "," out)))
                             (let ((fd-name (format nil "~A_fdiv" (ref-symbol sym))))
                                 (if (eql (get sym 'loop-level) 0)
                                     (write-string fd-name out)
                                     (write-string (ref-extvar fd-name) out)))
                             (write-string ")" out))
                         (`(/ ,x ,(type number num))
                             (write-string "_mm_mul_ps(" out)
                             (compile-form-float out x)
                             (format out ",_mm_set1_ps(1.0/~A))" num))
                         (`(/ ,(type number num))
                             (format out "_mm_set1_ps(1.0/~A)" num))
                         (`(/ ,x)
                             (write-string "_mm_div_ps(_mm_set1_ps(1.0)," out)
                             (compile-form-float out x)
                             (write-string ")" out))
                         (`(,(as op (or '+ '- '* '/
                                        'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
                             (write-string (case op
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
                                        (or "_mm_or_ps"))
                                 out)
                             (write-string "(" out)
                             (compile-form-float out a)
                             (write-string "," out)
                             (compile-form-float out b)
                             (write-string ")" out))
                         (`(+ ,a)
                             (compile-form-float out a))
                         (`(- ,a)
                             (write-string "_mm_sub_ps(_mm_setzero_ps()," out)
                             (compile-form-float out a)
                             (write-string ")" out))
                         (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt)) ,@_)
                             (error "Functions not supported in SSE: ~A" func))
                         (`(ptr-deref ,ptr)
                             (if (is-level0-ptr ptr)
                                 (progn
                                     (write-string "_mm_loadu_ps(" out)
                                     (compile-form-ptr out ptr)
                                     (write-string ")" out))
                                 (progn
                                     (write-string "_mm_set1_ps(*(" out)
                                     (compile-form-ptr out ptr)
                                     (write-string "))" out))))
                         ((when (and (= a 0) (= b 0))
                             `(if ,_ ,(type number a) ,(type number b)))
                             (write-string "_mm_setzero_ps()"))
                         ((when (or (and (numberp a) (= a 0))
                                    (and (numberp b) (= b 0)))
                              `(if ,icond ,a ,b))
                             (let ((a-zero (and (numberp a) (= a 0))))
                                 (write-string
                                     (if a-zero "_mm_andnot_ps(" "_mm_and_ps(")
                                     out)
                                 (compile-form-float out icond)
                                 (write-line "," out)
                                 (compile-form-float out (if a-zero b a))
                                 (write-string ")" out)))
                         (`(if ,icond ,a ,b)
                             (let* ((tmp-var (gensym))
                                    (tmp-name (symbol-name tmp-var)))
                                 (push tmp-name aux-vars)
                                 (format out  "(~A = (" tmp-name)
                                 (compile-form-float out icond)
                                 (write-line ")," out)
                                 (write-string "_mm_or_ps(" out)
                                 (format out "_mm_and_ps(~A," tmp-name)
                                 (compile-form-float out a)
                                 (write-line ")," out)
                                 (format out "_mm_andnot_ps(~A," tmp-name)
                                 (compile-form-float out b)
                                 (write-line ")" out))
                                 (write-string "))" out))
                         (`(setf (ptr-deref ,target) ,expr)
                             (let* ((tmp-var (gensym))
                                    (tmp-name (symbol-name tmp-var)))
                                 (unless (is-level0-ptr target)
                                     (error "Store to a non-inner ptr in SSE: ~A" form))
                                 (push tmp-name aux-vars)
                                 (format out  "(~A = (" tmp-name)
                                 (compile-form-float out expr)
                                 (write-line ")," out)
                                 (write-string "_mm_storeu_ps(" out)
                                 (compile-form-ptr out target)
                                 (format out ", ~A), ~A)" tmp-name tmp-name)))
                         (`(progn ,cmd1 ,@rest)
                             (write-string "(" out)
                             (compile-form-float out cmd1)
                             (dolist (cmd rest)
                                 (write-line "," out)
                                 (compile-form-float out cmd))
                             (write-string ")" out))
                         (_
                             (error "Unrecognized form in compile-sse(float): ~A" form))))
                 (compile-form-structure (out form)
                     (match form
                         (`(let* ,assns ,@body)
                             (write-line "{" out)
                             (dolist (assn assns)
                                 (compile-temp-assn out
                                     (first assn) (second assn)))
                             (dolist (cmd body)
                                 (compile-form-structure out cmd))
                             (write-line "}" out))
                         (`(setf (ptr-deref ,target) ,expr)
                             (unless (is-level0-ptr target)
                                 (error "Store to a non-inner ptr in SSE: ~A" form))
                             (write-string "_mm_storeu_ps(" out)
                             (compile-form-ptr out target)
                             (write-line "," out)
                             (compile-form-float out expr)
                             (write-line ");" out))
                         (`(progn ,@body)
                             (dolist (cmd body)
                                 (compile-form-structure out cmd)))
                         (_
                             (error "Unrecognized form in compile-sse(structure): ~A" form)))))
            (handler-bind ((condition
                   #'(lambda (cond)
                         (format t "~%SSE compilation failed:~%   ~A~%" cond)
                         (format t "Reverting to ordinary C.~%")
                         (return-from compile-expr-sse nil))))
                (let ((code (with-output-to-string (out)
                                (compile-form-structure out full_expr)))
                      (vars (if (null aux-vars) ""
                                (format nil "__m128 ~{~A~^, ~};~%" aux-vars))))
                    (concatenate 'string vars code))))))

(defun compile-expr-generic (full_expr)
    (let ((types (derive-types full_expr))
          (args  ())
          (arg-types ())
          (arg-map (make-hash-table))
          (arr-map (make-hash-table)))
        (labels ((ref-symbol (sym)
                     (if (get sym 'let-clause)
                         (concatenate 'string "tmp_" (symbol-name sym))
                         (ref-arg sym)))
                 (ref-atom (atomv)
                     (if (symbolp atomv) (ref-symbol atomv)
                         (format nil "~S" atomv)))
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
                             (max dim (or (gethash rarr arr-map) 0)))))
                 (compile-temp-assn (out var expr)
                     (let ((var-type (gethash expr types))
                           (var-name (concatenate 'string "tmp_"
                                         (symbol-name var)))
                           (var-fdiv (or (get var 'fdiv-users) 0)))
                         (write-string (match var-type
                                     ('array "cl_object")
                                     ('float "float")
                                     ('float-ptr "float*")
                                     ('integer "int")
                                     ('boolean "int")
                                     (_ (error "Bad type ~A of ~A in ~A"
                                            var-type expr full_expr)))
                             out)
                         (write-string " " out)
                         (write-string var-name out)
                         (write-string " = (" out)
                         (compile-form out expr)
                         (format out ");~%")
                         (when (> var-fdiv 1)
                             (format out "float ~A_fdiv = (1.0/~A);~%"
                                 var-name var-name))))
                 (compile-form (out form &optional stmtp)
                     (match form
                         ((type symbol sym)
                             (write-string (ref-symbol sym) out))
                         ((type atom _)
                             (prin1 form out))
                         (`(declare ,@_)
                             (unless stmtp
                                 (write-string "0" out)))
                         (`(_grp ,x)
                             (compile-form out x))
                         (`(ranging ,v ,@_)
                             (if (ranging-loop-level form)
                                 (write-string (symbol-name v) out)
                                 (compile-form out v)))

                         ((when (and (eql (gethash form types) 'float)
                                     (> (or (get sym 'fdiv-users) 0) 1))
                             `(/ ,x ,(type symbol sym)))
                             (unless (and (numberp x)
                                          (= x 1))
                                 (write-string "(" out)
                                 (compile-form out x)
                                 (write-string ")*" out))
                             (format out "~A_fdiv" (ref-symbol sym)))

                         ((when (eql (gethash form types) 'float)
                             `(/ ,x))
                             (write-string "1.0/(" out)
                             (compile-form out x)
                             (write-string ")" out))

                         (`(,(as op (or '+ '- '* '/ 'truncate 'rem 'ptr+
                                        'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
                             (write-string "(" out)
                             (compile-form out a)
                             (write-string ")" out)
                             (write-string (case op
                                        ((+ - * / < > >= <=) (symbol-name op))
                                        (/= "!=")
                                        (= "==")
                                        (ptr+ "+")
                                        (and "&&")
                                        (or "||")
                                        (truncate "/")
                                        (rem "%"))
                                 out)
                             (write-string "(" out)
                             (compile-form out b)
                             (write-string ")" out))
                         (`(,(as op (or '+ '-)) ,a)
                             (prin1 op out)
                             (write-string "(" out)
                             (compile-form out a)
                             (write-string ")" out))

                         ;; This is actually incorrect, because integer division
                         ;; in C is equivalent to truncate, but we use them only
                         ;; for positive numbers, where there is no difference.
                         ;; However, as a special case, for powers of 2 we can
                         ;; precisely emulate floor&rem using bit operations.
                         ((when (and (eql (gethash form types) 'integer)
                                     (= (logand b (1- b)) 0))
                             `(,(as op (or 'floor 'mod))
                                      ,a ,(type integer b)))
                             (write-string "(((int)(" out)
                             (compile-form out a)
                             (if (eql op 'floor)
                                 (format out "))>>~A)"
                                     (do ((cnt 0 (1+ cnt))
                                          (bv b (ash bv -1)))
                                         ((<= bv 1) cnt)))
                                 (format out "))&~A)" (1- b))))
                         ((when (eql (gethash form types) 'integer)
                             `(,(as op (or 'floor 'mod)) ,a ,b))
                             (write-string "((int)((" out)
                             (compile-form out a)
                             (write-string ")" out)
                             (write-string (case op
                                               (floor "/")
                                               (mod "%"))
                                 out)
                             (write-string "(" out)
                             (compile-form out b)
                             (write-string ")))" out))

                         (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt))
                               ,arg ,@rest)
                             (write-string (case func
                                        ('expt "pow")
                                        ('ceiling "ceil")
                                        (t (string-downcase (symbol-name func))))
                                 out)
                             (write-string "(" out)
                             (compile-form out arg)
                             (dolist (arg2 rest)
                                 (write-string ", " out)
                                 (compile-form out arg2))
                             (write-string ")" out))
                         (`(arr-dim ,arr ,idx)
                             (use-array arr idx)
                             (when (= 0 idx)
                                 (write-string "VECTORP(" out)
                                 (compile-form out arr)
                                 (write-string ")?(" out)
                                 (compile-form out arr)
                                 (write-string ")->vector.dim:" out))
                             (write-string "(" out)
                             (compile-form out arr)
                             (format out ")->array.dims[~A]" idx))
                         (`(arr-ptr ,arr)
                             (use-array arr 0)
                             (write-string "VECTORP(" out)
                             (compile-form out arr)
                             (write-string ")?(" out)
                             (compile-form out arr)
                             (write-string ")->vector.self.sf:(" out)
                             (compile-form out arr)
                             (write-string ")->array.self.sf" out))
                         (`(ptr-deref ,ptr)
                             (write-string "*(" out)
                             (compile-form out ptr)
                             (write-string ")" out))
                         (`(if ,icond ,a ,b)
                             (write-string "(" out)
                             (compile-form out icond)
                             (write-string ") ? (" out)
                             (compile-form out a)
                             (write-string ") : (" out)
                             (compile-form out b)
                             (write-string ")" out))
                         (`(let* ,assns ,@body)
                             (write-line "{" out)
                             (dolist (assn assns)
                                 (compile-temp-assn out
                                     (first assn) (second assn)))
                             (dolist (cmd body)
                                 (compile-form out cmd t))
                             (write-line "}" out))
                         (`(setf ,target ,expr)
                             (compile-form out target)
                             (write-string " = (" out)
                             (compile-form out expr)
                             (write-string ")" out)
                             (when stmtp (write-line ";" out)))
                         (`(loop-range
                              (ranging ,arg ,min ,max 1 nil 0 ,@_)
                              ,@body)
                             (format out "{~%int ~A = " arg)
                             (compile-form out min)
                             (write-line ";" out)
                             (let* ((ext-vars (make-hash-table :test #'equal))
                                    (sse-code (compile-expr-sse types ext-vars `(progn ,@body))))
                                 (when sse-code
                                     (write-string "{" out)
                                     (maphash #'(lambda (v n)
                                                    (format out "__m128 ~A_4 = _mm_set1_ps(~A);~%" v v))
                                         ext-vars)
                                     (format out "for (; ~A <= (" arg)
                                     (compile-form out max)
                                     (format out ")-3; ~A += 4) {~%" arg)
                                     (write-string sse-code out)
                                     (write-line "}}" out)))
                             (format out "for (; ~A <= " arg)
                             (compile-form out max)
                             (format out "; ~A++) {~%" arg)
                             (dolist (cmd body)
                                 (compile-form out cmd t))
                             (write-line "}}" out))
                         (`(loop-range
                              (ranging ,arg ,min ,max ,delta ,@_)
                              ,@body)
                             (when (eql (ranging-loop-level (second form)) 0)
                                 (format t "SSE inapplicable: ~A~%" (second form)))
                             (format out "{~%int ~A;~%for(~A = " arg arg)
                             (compile-form out (if (> delta 0) min max))
                             (format out "; ~A ~A " arg (if (> delta 0) "<=" ">="))
                             (compile-form out (if (> delta 0) max min))
                             (format out "; ~A += ~A) {~%" arg delta)
                             (dolist (cmd body)
                                 (compile-form out cmd t))
                             (write-line "}}" out))
                         ((when stmtp `(progn ,@body))
                             (dolist (cmd body)
                                 (compile-form out cmd t)))
                         (`(progn ,cmd1 ,@rest)
                             (compile-form out cmd1)
                             (dolist (cmd rest)
                                 (write-string ", " out)
                                 (compile-form out cmd)))
                         (`(safety-check ,checks ,@body)
                             (dolist (check checks)
                                 (write-string "if (!(" out)
                                 (compile-form out (first check))
                                 (write-line "))" out)
                                 (format out "    FEerror(\"Safety check failed: ~A\",0);~%"
                                     (second check)))
                             (dolist (cmd body)
                                 (compile-form out cmd t)))
                         (_
                             (error "Unrecognized form in compile-generic: ~A" form)))))
            (let* ((code   (with-output-to-string (out)
                               (write-line "{" out)
                               (compile-form out full_expr)
                               (write-line "}" out)))
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
                         :void ,(concatenate 'string checks code)))))))

(define-compiler-macro compute (&whole original name idxspec expr &key with parallel)
    (handler-bind ((condition
                       #'(lambda (cond)
                             (format t "~%Fast C compilation failed:~%   ~A~%" cond)
                             (format t "Reverting to ordinary lisp.~%")
                             (return-from compute original))))
        (let* ((*current-compute* original)
               (*consistency-checks* (make-hash-table :test #'equal)))
            (multiple-value-bind
                    (loop-expr range-list loop-list)
                    (make-compute-loops name idxspec expr with)
                (let* ((nomacro-expr (expand-macros loop-expr))
                       (nolet-expr   (expand-let nomacro-expr))
                       (noiref-expr (simplify-iref nolet-expr))
                      ; (opt-expr    (optimize-tree noiref-expr))
                       (noaref-expr (expand-aref noiref-expr))
                       (check-expr  (insert-checks noaref-expr)))
                    (wrap-compute-parallel parallel range-list check-expr
                        #'(lambda (code)
                             `(let ((*current-compute* ',original))
                                  ,(compile-expr-generic
                                       (code-motion code :pull-symbols t))))))))))
