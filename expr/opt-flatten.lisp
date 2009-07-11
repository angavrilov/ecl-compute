;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun toggle-minus (expr)
    (match expr
        ((type number num) (- num))
        (`(- ,x) x)
        (x `(- ,x))))

(defun collect-constants-1 (expr reduce-op)
    (if (null expr)
        (values nil nil)
        (multiple-value-bind (expr1 constv)
            (collect-constants-1 (cdr expr) reduce-op)
            (if (numberp (car expr))
                (values expr1
                    (if constv
                        (funcall reduce-op constv (car expr))
                        (car expr)))
                (values
                    (cons (car expr) expr1)
                    constv)))))

(defun collect-constants (reduce-op zero tag-op expr)
    (multiple-value-bind (expr1 constv)
        (collect-constants-1 expr reduce-op)
        (let ((oplst
                    (if (or (null constv) (= constv zero))
                        expr1
                        (cons constv expr1))))
            (if (null oplst)
                zero
                (if (null (cdr oplst))
                    (car oplst)
                    (cons tag-op oplst))))))

(defun flatten+ (args)
    (collect-constants #'+ 0 '+
        (mapcan
            #'(lambda (arg)
                  (match arg
                      (`(+ ,@lst)
                          (copy-list lst))
                      (`(- ,(type number val))
                          (list (- val)))
                      (`(- (+ ,@lst))
                          (mapcar #'toggle-minus lst))
                      (_ (list arg))))
            args)))

(defun toggle-div (expr)
    (match expr
        (`(/ ,x) x)
        (x `(/ ,x))))

(defun flatten* (args)
    (collect-constants #'* 1 '*
        (mapcan
            #'(lambda (arg)
                  (match arg
                      (`(* ,@lst)
                          (copy-list lst))
                      (`(/ (* ,@lst))
                          (mapcar #'toggle-div lst))
                      (`(- (* ,x ,@lst))
                          (cons (toggle-minus x)
                              (copy-list lst)))
                      (`(- (/ (* ,x ,@lst)))
                          (mapcar #'toggle-div
                              (cons (toggle-minus x)
                                  (copy-list lst))))
                      (_ (list arg))))
            args)))

(defun flatten-exprs-1 (expr old-expr)
    (match expr
        (`(ranging ,@_) old-expr)
        (`(+ ,x)     x)
        (`(* ,x)     x)
        (`(- (- ,x)) x)
        (`(/ (/ ,x)) x)
        (`(- ,(type number val)) (- val))
        (`(- ,_)     expr)
        (`(/ ,_)     expr)
        (`(- ,x ,@rest)
            (flatten+
                `(,x (- ,(flatten+ rest)))))
        (`(+ ,@args)
            (flatten+ args))
        (`(/ ,x ,@rest)
            (flatten-exprs-1
                `(* ,x (/ ,(flatten* rest)))
                old-expr))
        (`(* ,@args)
            (let ((rv (flatten* args)))
                (match rv
                    ((when (= val 0)
                        `(* ,(type number val) ,@_))
                        0)
                    (_ rv))))
        (_ nil)))
