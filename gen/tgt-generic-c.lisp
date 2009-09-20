;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

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
      (recurse `(setf-tmp ,(first assn) ,(second assn))))
    (dolist (cmd body)
      (recurse cmd :stmt-p t))
    (text "}~%"))

  ((when stmt-p
     `(progn ,@body))
    (dolist (cmd body)
      (recurse cmd :stmt-p t)))

  (`(progn ,cmd1 ,@rest)
    (code "(" cmd1)
    (dolist (cmd rest)
      (code ",~%" cmd))
    (code ")")))

(def-form-compiler compile-generic-c (form form-type stmt-p)
  ((type float fv)
    (text "~Sf" fv))

  ((type number nv)
    (text "~S" nv))

  ((ranging-spec v :loop-level level)
    (if level
        (text (symbol-name v))
        (recurse v)))

  ((when (eql form-type 'float)
     `(/ ,x))
    (code "1.0f/(" x ")"))

  (`(,(as op (or '+ '- '* '/ 'truncate 'rem 'ptr+
                 'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
    (code "(" a ")"
          ("~A" (case op
                  ((+ - * / < > >= <=) op)
                  (/= "!=")
                  (= "==")
                  (ptr+ "+")
                  (and "&&")
                  (or "||")
                  (truncate "/")
                  (rem "%")))
          "(" b ")"))

  (`(,(as op (or '+ '-)) ,a)
    (code ("~A(" op) a ")"))

  (`(,(as op (or 'max 'min)) ,a ,b)
    (code "(((" a ")"
          (:text (if (eql op 'min) "<" ">"))
          "(" b "))?(" a "):(" b "))"))

  ;; This is actually incorrect, because integer division
  ;; in C is equivalent to truncate, but we use them only
  ;; for positive numbers, where there is no difference.
  ;; However, as a special case, for powers of 2 we can
  ;; precisely emulate floor&rem using bit operations.
  ((when (and (eql form-type 'integer)
              (= (logand b (1- b)) 0))
     `(,(as op (or 'floor 'mod)) ,a ,(type integer b)))
    (code "(((int)(" a)
    (if (eql op 'floor)
        (text "))>>~A)"
              (do ((cnt 0 (1+ cnt))
                   (bv b (ash bv -1)))
                  ((<= bv 1) cnt)))
        (text "))&~A)" (1- b))))

  ((when (eql form-type 'integer)
     `(,(as op (or 'floor 'mod)) ,a ,b))
    (code "((int)((" a ")"
          (:text (ecase op
                   (floor "/")
                   (mod "%")))
          "(" b ")))"))

  ((when (eql form-type 'integer)
     `(ceiling ,a ,(type integer b)))
    (code "((int)(((" a
          (")+~A)/~A))" (1- b) b)))

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
      (code ", " arg2))
    (text ")"))

  (`(float-sign ,arg)
    (recurse `(float-sign ,arg 1.0)))

  (`(float-sign ,arg ,base)
    (code "copysignf(" base ", " arg ")"))

  (`(ptr-deref ,ptr)
    (code "*(" ptr ")"))

  (`(texture-ref-int ,_ ,name ,idx)
    (code ("tex1Dfetch(~A, " name) idx ")"))

  (`(texture-ref ,_ ,name ,idx1 ,idx2)
    (code ("tex2D(~A, " name) idx2 ", " idx1 ")"))

  ((when stmt-p
     `(if ,icond ,a ,b))
    (code "if (" icond ") {~%"
          (:recurse a :stmt-p t)
          "} else {~%"
          (:recurse b :stmt-p t)
          "}~%"))

  (`(if ,icond ,a ,b)
    (code "(" icond ") ? (" a ") : (" b ")"))

  (`(setf ,target ,expr)
    (code target " = (" expr ")" (:when stmt-p ";~%"))))

(def-form-compiler compile-c-inline-temps (form)
  (`(setf-tmp ,var (temporary ',name ,dims ,@_))
    (let* ((const-dim (if (and dims (every #'numberp dims))
                          (reduce #'* dims)))
           (is-const (and const-dim (< const-dim 65536))))
      (code "float" (:unless (or (null dims) is-const) "*")
            (" ~A" (temp-symbol-name var)))
      (cond
        (is-const
         (text "[~A]" const-dim))
        (dims
         (code " = (float*)ecl_alloc_atomic(" (reduce #'join* dims) ")")))
      (text "; /* ~A */~%" name)))

  (`(setf-tmp ,var ,expr)
    (let ((var-type (gethash expr *cg-type-table*))
          (var-name (temp-symbol-name var)))
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
      (text ");~%"))))
