;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun annotate-types (expr)
    (let ((types (derive-types expr)))
        (simplify-rec-once
            #'(lambda (expr old-expr)
                (match expr
                    (`(,(or 'let 'let* 'symbol-macrolet) ,@_)
                        nil)
                    (`(ranging ,@_)
                        old-expr)
                    (`(multivalue-data ,@_)
                        `(the (array single-float) ,old-expr))
                    (`(temporary ,_ ,dims ,@_)
                        `(temporary
                             ,(cadr old-expr) ,dims ,@(cdddr old-expr)))
                    (`(setf (the ,_ ,arg) ,tgt)
                        `(setf ,arg ,tgt))
                    (`(safety-check ,checks1 ,@body)
                        `(safety-check
                             ,(mapcar #'(lambda (new old)
                                            (cons (car new) (cdr old)))
                                  checks1 (second old-expr))
                             ,@body))
                    (_
                        (multiple-value-bind (type found) (gethash old-expr types)
                            (if found
                                (let ((tspec (match type
                                                ('float 'single-float)
                                                ('integer 'fixnum)
                                                ('boolean 'boolean)
                                                ('array 'array)
                                                ('nil 'single-float)
                                                (_ (error "Bad type ~A" type)))))
                                    `(the ,tspec ,expr))
                                expr)))))
            expr)))
