;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun pull-minus-1 (expr old-expr)
    (match expr
        (`(ranging ,@_) old-expr)
        (`(- (- ,x)) x)
        (`(/ (- ,x)) `(- (/ ,x)))
        (`(- ,(type number val)) (- val))
        (`(* ,@args)
            (let* ((minus-cnt 0)
                   (args2 (mapcar
                              #'(lambda (arg)
                                   (match arg
                                       ((when (< val 0)
                                           (type number val))
                                           (incf minus-cnt)
                                           (- val))
                                       (`(- ,x)
                                           (incf minus-cnt)
                                           x)
                                       ((when (< val 0)
                                           `(/ ,(type number val)))
                                           (incf minus-cnt)
                                           `(/ ,(- val)))
                                       (`(/ (- ,x))
                                           (incf minus-cnt)
                                           `(/ ,x))
                                       (_ arg)))
                             args)))
                (if (> minus-cnt 0)
                    (if (= (rem minus-cnt 2) 0)
                        `(* ,@args2)
                        `(- (* ,@args2)))
                    expr)))
        (`(+ ,@args)
            (let* ((nums     (remove-if-not #'numberp args))
                   (non-nums (remove-if #'numberp args)))
                (if (and non-nums
                         (every #'(lambda (arg)
                                          (match arg (`(- ,_) t)))
                                 non-nums))
                    `(- (+ ,@(mapcar #'- nums)
                           ,@(mapcar #'second non-nums))))))
        (_ nil)))

(defun pull-factor-list (init-lst to-pull)
    (let ((diff (subtract-list init-lst to-pull)))
        (cond
            ((null diff)
                1)
            ((null (cdr diff))
                (car diff))
            (t
                (canonify-expr `(* ,@diff))))))

(defun pull-factors-1 (expr old-expr)
    ;; Requires and preserves canonification
    (match expr
        (`(ranging ,@_) old-expr)
        (`(+ ,@args)
            (let ((common-prod
                       (reduce
                           #'(lambda (cur-set arg)
                                 (let ((fct-list
                                           (match arg
                                               (`(* ,@lst)
                                                   lst)
                                               (`(- (* ,@lst))
                                                   lst)
                                               (`(- ,x)
                                                   (list x))
                                               (_
                                                   (list arg)))))
                                     (if (eql cur-set 'none)
                                         fct-list
                                         (common-sublist
                                             cur-set
                                             fct-list))))
                           args
                           :initial-value 'none)))
                (if (or (null common-prod)
                        (eql common-prod 'none))
                    (if (eql expr old-expr) old-expr
                        (canonify-expr expr))
                    (let ((filtered-args
                              (mapcar
                                  #'(lambda (arg)
                                        (match arg
                                            (`(* ,@lst)
                                                (pull-factor-list
                                                    lst common-prod))
                                            (`(- (* ,@lst))
                                                (canonify-expr
                                                    `(- ,(pull-factor-list
                                                            lst common-prod))))
                                            (`(- ,x)
                                                (canonify-expr
                                                    `(- ,(pull-factor-list
                                                            (list x) common-prod))))
                                            (_
                                                (pull-factor-list
                                                    (list arg) common-prod))))
                                  args)))
                        (canonify-expr
                            `(* ,(canonify-expr
                                     (flatten+ filtered-args))
                                ,@common-prod))))))
        ((when (not (eql expr old-expr))
            `(* ,@args))
            (canonify-expr
                (flatten* args)))
        (_
            (if (eql expr old-expr) old-expr
                (canonify-expr expr)))))
