;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

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
    ((ranging-spec _ :loop-level level)
      (eql level 0))
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
  (call-form-compilers (list #'compile-sse-ptr-sym
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
         (recurse (unwrap-factored sym)))
        ((eql sym-type 'boolean)
         (error "Boolean -> sse cast not implemented"))
        (t
         (text (ref-sse-extvar sym-name))))))

  ((when (= val 0) (type number val))
    (text "_mm_setzero_ps()"))
  ((type atom val)
    (text "_mm_set1_ps(~A)" val))

  ((ranging-spec v :loop-level level)
    (if (eql level 0)
        (text "_mm_add_ps(_mm_set1_ps(~A),_mm_setr_ps(0,1,2,3))"
              (symbol-name v))
        (text (ref-sse-extvar (symbol-name v)))))

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
    (text "_mm_xor_ps(__sign_mask.sse,")
    (recurse a)
    (text ")"))

  (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt)) ,@_)
    (error "Functions not supported in SSE: ~A" func))

  (`(float-sign ,arg ,(type number base))
    (text "_mm_or_ps(")
    (recurse (abs base))
    (text ",_mm_and_ps(__sign_mask.sse,")
    (recurse arg)
    (text "))"))

  (`(float-sign ,arg ,base)
    (text "_mm_or_ps(_mm_andnot_ps(__sign_mask.sse,")
    (recurse base)
    (text "),_mm_and_ps(__sign_mask.sse,")
    (recurse arg)
    (text "))"))

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
      (text (if a-zero "_mm_andnot_ps(" "_mm_and_ps("))
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
    (handler-bind
        ((condition #'(lambda (cond)
                        (format t "~%SSE compilation failed:~%   ~A~%" cond)
                        (format t "Reverting to ordinary C.~%")
                        (return-from compile-expr-sse nil))))
      (let ((code (call-form-compilers (list #'compile-float-sse
                                             #'compile-sse-temps
                                             #'compile-generic)
                                       full_expr :stmt-p t))
            (vars (if (null *cg-sse-auxvars*)
                      ""
                      (format nil "__m128 ~{~A~^, ~};~%" *cg-sse-auxvars*)))
            (preamble "const static union {
                         unsigned bits[4];
                         __m128 sse;
                       } __sign_mask = { { 0x80000000U, 0x80000000U, 0x80000000U, 0x80000000U } };
"))
        (concatenate 'string preamble vars code)))))
