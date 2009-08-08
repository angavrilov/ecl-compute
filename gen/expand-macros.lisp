;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun expand-macros (expr)
    (match expr
        ((type atom _) expr)
        (`(declare ,@_) expr)
        (`(multivalue-data ,@_) expr)
        (`(1+ ,v)
            (expand-macros `(+ ,v 1)))
        (`(1- ,v)
            (expand-macros `(- ,v 1)))
        (`(expt ,v 1)
            (expand-macros v))
        (`(expt ,v 2)
            (expand-macros `(* ,v ,v)))
        (`(temporary ,name ,dims ,@rest)
            `(temporary ,name
                 ,(mapcar #'expand-macros dims)
                 ,@rest))
        ((cons (or 'index 'aref 'iref '_grp 'tmp-ref 'quote
                   '+ '- '* '/ 'mod 'rem 'floor 'ceiling 'truncate
                   'and 'or 'if 'progn
                   'sin 'cos 'exp 'expt 'float-sign
                   '> '< '>= '<= '/= '= 'min 'max 'setf 'loop-range) tail)
            (cons-save-old expr (car expr)
                (mapcar-save-old #'expand-macros tail)))
        (`(,(as op (or 'let 'let* 'symbol-macrolet)) ,vars ,@body)
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
