;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defmacro ranging (expr min max delta &optional ordered-p loop-level &rest tail)
    (declare (ignore min max delta ordered-p loop-level tail))
    expr)

(defun ranging-order-flag (rspec)
    (unless (eql (car rspec) 'ranging)
        (error "Not a ranging spec: ~A" rspec))
    (nth 5 rspec))

(defun ranging-loop-level (rspec)
    (unless (eql (car rspec) 'ranging)
        (error "Not a ranging spec: ~A" rspec))
    (nth 6 rspec))

(defun get-full-expr (expr)
    (cond
        ((symbolp expr)
            (or (get expr 'full-expr) expr))
        ((consp expr)
            (mapcar-save-old #'get-full-expr expr))
        (t
            expr)))

(defun compute-range-1 (expr &optional (old-expr expr))
    (match expr
        ((when (or (get smin 'full-expr)
                   (get smax 'full-expr))
            `(ranging ,arg ,(type symbol smin) ,(type symbol smax) ,@rest))
            `(ranging ,arg
                 ,(get-full-expr smin)
                 ,(get-full-expr smax)
                 ,@rest))
        (`(- (ranging ,arg ,min ,max ,delta ,@rest))
            `(ranging (- ,arg) (- ,max) (- ,min) (- ,delta) ,@rest))
        (`(,(as op (or '+ '-))
              (ranging ,arg ,min ,max ,@rest)
              (ranging ,arg2 ,min2 ,max2 ,@_))
            `(ranging (,op ,arg ,arg2)
                 (,op ,min ,(if (eql op '+) min2 max2))
                 (,op ,max ,(if (eql op '+) max2 min2))
                 ,@rest))
        (`(,(as op (or '+ '-))
              (ranging ,arg ,min ,max ,@rest) ,@pv)
            `(ranging (,op ,arg ,@pv)
                 (,op ,min ,@pv) (,op ,max ,@pv) ,@rest))
        (`(mod (ranging ,arg ,min ,max ,@rest)
              ,(type number num))
            `(ranging (mod ,arg ,num) 0 ,(1- num) ,@rest))
        ((when (and (numberp mv) (>= mv 0))
            `(,(as op (or '* '/ 'floor))
                 (ranging ,arg ,min ,max ,delta ,@rest) ,mv))
            `(ranging (,op ,arg ,mv)
                 (,op ,min ,mv) (,op ,max ,mv) (,op ,delta ,mv) ,@rest))
        ((when (and (numberp mv) (< mv 0))
            `(,(as op (or '* '/ 'floor))
                 (ranging ,arg ,min ,max ,delta ,@rest) ,mv))
            `(ranging (,op ,arg ,mv)
                 (,op ,max ,mv) (,op ,min ,mv) (,op ,delta ,mv) ,@rest))
        (_ nil)))

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

(defun cons-save-old (old carv cdrv)
    (if (and (eql carv (car old))
             (eql cdrv (cdr old)))
        old
        (cons carv cdrv)))

(defun mapcar-save-old (fun lst)
    (if (null lst) nil
        (cons-save-old lst
            (funcall fun (car lst))
            (mapcar-save-old fun (cdr lst)))))

(defun simplify-rec (engine expr cache)
    (if (or (atom expr) (gethash expr cache))
        expr
        (let* ((rec-res (mapcar-save-old
                            #'(lambda (sub) (simplify-rec engine sub cache))
                            expr))
               (subs-res (funcall engine rec-res expr)))
            (if (null subs-res)
                (progn
                    (setf (gethash rec-res cache) t)
                    rec-res)
                (simplify-rec engine subs-res cache)))))

(defun simplify-rec-once (engine expr)
    (let* ((rec-res (if (atom expr)
                        expr
                        (mapcar-save-old
                            #'(lambda (sub) (simplify-rec-once engine sub))
                            expr)))
           (subs-res (funcall engine rec-res expr)))
        (if (null subs-res) rec-res subs-res)))

(defparameter *simplify-cache* (make-hash-table))

(defun simplify-index (expr)
    (simplify-rec #'simplify-index-1 expr *simplify-cache*))

(defparameter *range-cache* (make-hash-table))

(defun compute-range (expr)
    (let ((cached (gethash expr *range-cache*)))
        (if cached cached
            (setf (gethash expr *range-cache*)
                (simplify-index
                    (simplify-rec-once #'compute-range-1 expr))))))

(defun index-expr-p (expr)
    (or (numberp expr)
        (and (consp expr)
             (find (car expr)
                 '(+ - * / 1+ 1- floor ceiling mod rem truncate ranging)))))

(defun remove-ranges (expr)
    (simplify-rec-once
        #'(lambda (expr old)
              (if (and (consp expr)
                       (eql (car expr) 'ranging))
                  (second expr)
                  nil))
        expr))

(defun compare-indexes (expr1 expr2 &optional (delta 0))
    (match (cons expr1 expr2)
        (`((+ ,le ,(type number lv)) . ,_)
            (compare-indexes le expr2 (+ delta lv)))
        (`((- ,le ,(type number lv)) . ,_)
            (compare-indexes le expr2 (- delta lv)))
        (`(,_ . (+ ,re ,(type number rv)))
            (compare-indexes expr1 re (- delta rv)))
        (`(,_ . (- ,re ,(type number rv)))
            (compare-indexes expr1 re (+ delta rv)))
        (`((* ,le ,(type number lv)) . (* ,re ,lv))
            (compare-indexes le re (/ delta lv)))
        (`((,(or '/ 'floor) ,le ,(type number lv)) . (,(or '/ 'floor) ,re ,lv))
            (compare-indexes le re (* delta lv)))
        ((when (equal expr1 expr2))
            (cond ((< delta 0) '<)
                  ((> delta 0) '>)
                  (t '=)))
        (`(,(type number lv) . ,(type number rv))
            (compare-indexes 0 0 (+ delta (- lv rv))))
        (`((,(or '/ 'floor) ,le ,(type number lv)) . ,re)
            (compare-indexes le `(* ,re ,lv) (* delta lv)))
        (`((* ,le ,(type number lv)) . ,re)
            (compare-indexes le `(/ ,re ,lv) (/ delta lv)))
        (_ nil)))

(defun compute-num-range (expr)
    (match (compute-range expr)
        ((type number val)
            (cons val val))
        (`(ranging ,_ ,(type number min) ,(type number max) ,@_)
            (cons min max))
        (_ nil)))
