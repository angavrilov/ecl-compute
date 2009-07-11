;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *simplify-cache* (make-hash-table))

(defun simplify-index-1 (expr &optional (old-expr expr))
    (match expr
        ;; Expand abbreviations
        (`(1+ ,x) `(+ ,x 1))
        (`(1- ,x) `(- ,x 1))
        ;; Expand trivial ranges
        ((or
             `(ranging ,(type number val) ,@_)
             `(ranging ,_ ,(type number val) ,val ,@_))
            val)
        ;; Collapse arithmetical no-ops
        ((or `(/ ,x 1) `(* ,x 1)
             `(+ ,x 0) `(- ,x 0) `(+ ,x)
             `(floor ,x 1) `(truncate ,x 1) `(ceiling ,x 1))
            x)
        ((or `(* ,_ 0) `(* 0 ,_) `(/ 0 ,_)
             `(mod 0 ,_) `(rem 0 ,_)
             `(mod ,_ 1) `(rem ,_ 1)
             `(floor 0 ,_) `(truncate 0 ,_) `(ceiling 0 ,_))
            0)
        ((or `(/ ,_ 0)
             `(floor ,_ 0) `(truncate ,_ 0))
            (error "Division by zero: ~A" old-expr))
        ;; Calculate fully numerical expressions
        ((when (every #'numberp args)
             `(,(or '+ '- '* '/ 'mod 'rem 'truncate 'floor 'ceiling) ,@args))
            (eval expr))
        ;; Normalize sign
        ((when (< v 0) `(+ ,x ,(type number v)))
            `(- ,x ,(- v)))
        ((when (< v 0) `(- ,x ,(type number v)))
            `(+ ,x ,(- v)))
        ;; Reorder the number in + and * to be last
        ((when (not (numberp b))
             `(,(as op (or '+ '*)) ,(type number a) ,b))
            (list op b a))
        ;; Simplify adjacent * & /
        (`(,(as op (or '* '/))
             (,op ,x ,(type integer i1))
             ,(type integer i2))
            (list op x (* i1 i2)))
        ;; Simplify obviously even division
        ((when (= (mod n1 n2) 0)
             `(,(or '/ 'floor 'ceiling 'truncate)
                  (* ,x ,(type integer n1))
                  ,(type integer n2)))
            `(* ,x ,(/ n1 n2)))
        ;; Simplify adjacent + & -
        (`(,(as op2 (or '+ '-))
             (,(as op1 (or '+ '-)) ,x ,(type integer i1))
             ,(type integer i2))
            (let ((iv (if (eql op1 op2) (+ i1 i2) (- i1 i2))))
                `(,op1 ,x ,iv)))
        ;; Pull constants out of /
        ((when (/= (truncate n1 n2) 0)
             `(/ (,(as op (or '+ '-)) ,x ,(type integer n1))
                  ,(type integer n2)))
            (multiple-value-bind (divr remr) (truncate n1 n2)
                `(,op (/ (,op ,x ,remr) ,n2) ,divr)))
        ;; Push constants into *
        ((when (/= (truncate n1 n2) 0)
             `(,(as op (or '+ '-))
                  (* ,x ,(type integer n2))
                  ,(type integer n1)))
            (multiple-value-bind (divr remr) (truncate n1 n2)
                `(,op (* (,op ,x ,divr) ,n2) ,remr)))
        ;; Remove trivial zero remainder case of mod & rem
        ((when (= (mod mulv divv) 0)
             `(,(or 'mod 'rem)
                  (* ,_ ,(type integer mulv))
                  ,(type integer divv)))
            0)
        ;; Split aligned multiplicative clauses from mod &c
        ((when (= (mod mulv divv) 0)
             `(,(as cmd (or 'mod 'floor 'ceiling)) ; no truncate & rem !
                  (,(as op (or '+ '-))
                      (* ,marg ,(type integer mulv))
                      ,remv)
                  ,(type integer divv)))
            `(+ (,cmd (* ,marg ,mulv) ,divv)
                (,cmd (,op ,remv) ,divv)))
        ;; Likewise for aligned constants
        ((when (= (mod remv divv) 0)
             `(,(as cmd (or 'mod 'floor 'ceiling)) ; no truncate & rem !
                  (,(as op (or '+ '-)) ,exv ,remv)
                  ,(type integer divv)))
            `(+ (,cmd ,exv ,divv)
                (,cmd (,op ,remv) ,divv)))
        ;; Strip mod if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (floor (car range) divv)
                                 (floor (cdr range) divv))))
             `(mod ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                `(- ,modv ,(* (floor (car range) divv) divv))))
        ;; Kill floor if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (floor (car range) divv)
                                 (floor (cdr range) divv))))
             `(floor ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                (floor (car range) divv)))
        ;; Kill ceiling if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (ceiling (car range) divv)
                                 (ceiling (cdr range) divv))))
             `(ceiling ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                (ceiling (car range) divv)))
        ;; Nothing to do
        (_ nil)))

(defun simplify-index (expr)
    (simplify-rec #'simplify-index-1 expr *simplify-cache*))
