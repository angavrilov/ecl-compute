;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun temp-symbol-name (sym)
    (assert (get sym 'let-clause))
    (concatenate 'string "tmp_" (symbol-name sym)))

(def-form-compiler compile-generic (form stmt-p)
    (`(declare ,@_)
        (unless stmt-p
            (text "0")))

    (`(_grp ,x)
        (recurse x))
    (`(tmp-ref ,x)
        (recurse x))
    
    (`(inline-strs ,@code)
        (dolist (item code)
            (if (stringp item)
                (text item)
                (recurse item))))

    (`(let* ,assns ,@body)
        (unless stmt-p
            (error "Let in a non-stmt context: ~A" form))
        (text "{~%")
        (dolist (assn assns)
            (recurse
                `(setf-tmp ,(first assn) ,(second assn))))
        (dolist (cmd body)
            (recurse cmd :stmt-p t))
        (text "}~%"))

    ((when stmt-p
         `(progn ,@body))
        (dolist (cmd body)
            (recurse cmd :stmt-p t)))

    (`(progn ,cmd1 ,@rest)
        (text "(")
        (recurse cmd1)
        (dolist (cmd rest)
            (text ",~%")
            (recurse cmd))
        (text ")")))

(def-form-compiler compile-generic-c (form form-type stmt-p)
    ((type float fv)
        (text "~Sf" fv))

    ((type number nv)
        (text "~S" nv))

    ((ranging-spec v :loop-level level)
        (if level
            (text (symbol-name v))
            (recurse v)))

    ((when (and (eql form-type 'float)
                (> (or (get sym 'fdiv-users) 0) 1))
         `(/ ,x ,(type symbol sym)))
        (unless (and (numberp x)
                     (= x 1))
            (text "(")
            (recurse x)
            (text ")*"))
        (text "~A_fdiv" (temp-symbol-name sym)))

    ((when (eql form-type 'float)
        `(/ ,x))
        (text "1.0f/(")
        (recurse x)
        (text ")"))

    (`(,(as op (or '+ '- '* '/ 'truncate 'rem 'ptr+
                   'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
        (text "(")
        (recurse a)
        (text ")~A("
            (case op
                ((+ - * / < > >= <=) op)
                (/= "!=")
                (= "==")
                (ptr+ "+")
                (and "&&")
                (or "||")
                (truncate "/")
                (rem "%")))
        (recurse b)
        (text ")"))

    (`(,(as op (or '+ '-)) ,a)
        (text "~A(" op)
        (recurse a)
        (text ")"))

    (`(,(as op (or 'max 'min)) ,a ,b)
        (text "(((")
        (recurse a)
        (text ")~A("
            (if (eql op 'min) "<" ">"))
        (recurse b)
        (text "))?(")
        (recurse a)
        (text "):(")
        (recurse b)
        (text "))"))

    ;; This is actually incorrect, because integer division
    ;; in C is equivalent to truncate, but we use them only
    ;; for positive numbers, where there is no difference.
    ;; However, as a special case, for powers of 2 we can
    ;; precisely emulate floor&rem using bit operations.
    ((when (and (eql form-type 'integer)
                (= (logand b (1- b)) 0))
        `(,(as op (or 'floor 'mod))
                 ,a ,(type integer b)))
        (text "(((int)(")
        (recurse a)
        (if (eql op 'floor)
            (text "))>>~A)"
                (do ((cnt 0 (1+ cnt))
                     (bv b (ash bv -1)))
                    ((<= bv 1) cnt)))
            (text "))&~A)" (1- b))))

    ((when (eql form-type 'integer)
        `(,(as op (or 'floor 'mod)) ,a ,b))
        (text "((int)((")
        (recurse a)
        (text ")~A("
            (case op
                (floor "/")
                (mod "%")))
        (recurse b)
        (text ")))"))

    ((when (eql form-type 'integer)
        `(ceiling ,a ,(type integer b)))
        (text "((int)(((")
        (recurse a)
        (text ")+~A)/~A))"
            (1- b) b))

    (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt))
          ,arg ,@rest)
        (text "~A("
            (ecase func
                (expt "powf")
                (ceiling "ceilf")
                (floor "floorf")
                (sin "sinf")
                (cos "cosf")
                (exp "expf")))
        (recurse arg)
        (dolist (arg2 rest)
            (text ", ")
            (recurse arg2))
        (text ")"))

    (`(float-sign ,arg)
        (recurse `(float-sign ,arg 1.0)))

    (`(float-sign ,arg ,base)
        (text "copysignf(")
        (recurse base)
        (text ", ")
        (recurse arg)
        (text ")"))

    (`(ptr-deref ,ptr)
        (text "*(")
        (recurse ptr)
        (text ")"))

    (`(texture-ref-int ,name ,idx)
        (text "tex1Dfetch(~A, " name)
        (recurse idx)
        (text ")"))

    (`(texture-ref ,name ,idx1 ,idx2)
        (text "tex2D(~A, " name)
        (recurse idx2)
        (text ", ")
        (recurse idx1)
        (text ")"))

    ((when stmt-p
         `(if ,icond ,a ,b))
        (text "if (")
        (recurse icond)
        (text ") {~%")
        (recurse a :stmt-p t)
        (text "} else {~%")
        (recurse b :stmt-p t)
        (text "}~%"))

    (`(if ,icond ,a ,b)
        (text "(")
        (recurse icond)
        (text ") ? (")
        (recurse a)
        (text ") : (")
        (recurse b)
        (text ")"))

    (`(setf ,target ,expr)
        (recurse target)
        (text " = (")
        (recurse expr)
        (text ")")
        (when stmt-p
            (text ";~%"))))

(def-form-compiler compile-c-inline-temps (form)
    (`(setf-tmp ,var (temporary ',name ,dims ,@_))
        (let* ((const-dim (if (and dims (every #'numberp dims))
                              (reduce #'* dims)))
               (is-const (and const-dim (< const-dim 65536))))
            (text "float")
            (unless (or (null dims) is-const)
                (text "*"))
            (text " ~A" (temp-symbol-name var))
            (cond
                (is-const
                    (text "[~A]" const-dim))
                (dims
                    (text " = (float*)ecl_alloc_atomic(")
                    (recurse
                        (reduce #'(lambda (a b) `(* ,a ,b)) dims))
                    (text ")")))
            (text "; /* ~A */~%" name)))

    (`(setf-tmp ,var ,expr)
        (let ((var-type (gethash expr *cg-type-table*))
              (var-name (temp-symbol-name var))
              (var-fdiv (or (get var 'fdiv-users) 0)))
            (text "~A ~A = ("
                (match var-type
                    ('array "cl_object")
                    ('float "float")
                    ('float-ptr "float*")
                    ('integer "int")
                    ('boolean "int")
                    (_ (error "Bad type ~A of ~A in ~A"
                           var-type expr *cg-full-expr*)))
                var-name)
            (recurse expr)
            (text ");~%")
            (when (> var-fdiv 1)
                (text "float ~A_fdiv = (1.0f/~A);~%"
                    var-name var-name)))))
