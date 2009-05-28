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

(defparameter *canonify-cache* (make-hash-table :test #'equal))

(defun canonify-compare (expr1 expr2)
    (cond
        ((numberp expr1)
            (if (numberp expr2)
                (< expr1 expr2)
                t))
        ((numberp expr2)
            nil)
        (t
            (< (sxhash expr1) (sxhash expr2)))))

(defun common-sublist (list1 list2)
    (cond
        ((null list1) nil)
        ((null list2) nil)
        ((eql (car list1) (car list2))
            (cons (car list1)
                (common-sublist (cdr list1) (cdr list2))))
        ((canonify-compare (car list1) (car list2))
            (common-sublist (cdr list1) list2))
        ((canonify-compare (car list2) (car list1))
            (common-sublist (cdr list1) list2))
        (t
            (common-sublist (cdr list1) (cdr list2)))))

(defun subtract-list (list1 list2)
    (cond
        ((null list2) list1)
        ((null list1)
            (error "Cannot subtract ~A from NIL" list2))
        ((eql (car list1) (car list2))
            (subtract-list (cdr list1) (cdr list2)))
        ((canonify-compare (car list1) (car list2))
            (cons (car list1)
                (subtract-list (cdr list1) list2)))
        (t
            (error "Cannot subtract ~A from ~A" list2 list1))))

(defun canonify-expr (expr)
    (if (null expr)
        expr
        (let* ((sorted-expr
                   (match expr
                       (`(,(as op (or '+ '*)) ,@rest)
                           (cons op
                               (stable-sort (copy-list rest)
                                   #'canonify-compare)))
                       (_ expr)))
               (cached-expr
                   (gethash sorted-expr *canonify-cache*)))
            (or cached-expr
                (setf
                    (gethash sorted-expr *canonify-cache*)
                    sorted-expr)))))

(defun canonify-tree-1 (expr old-expr)
    (if (and (consp expr)
             (eql (car expr) 'ranging))
        old-expr
        (canonify-expr expr)))

(defun canonify-tree (expr)
    (simplify-rec-once #'canonify-tree-1 expr))

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


(defun split-parts (lst)
    (let* ((len (length lst))
           (half (floor len 2)))
        (do ((head lst (cdr head))
             (part nil (cons (car head) part))
             (i 0 (1+ i)))
            ((= i half)
                (values (nreverse part) head)))))

(defun treeify+ (args)
    (if (null (cdr args))
        (car args)
        (multiple-value-bind (a b) (split-parts args)
            (match (first b)
                (`(- ,_)
                    `(- ,(treeify+ a)
                        ,(treeify+ (mapcar #'toggle-minus b))))
                (_
                    `(+ ,(treeify+ a)
                        ,(treeify+ b)))))))

(defun treeify* (args)
    (labels ((is-div (x) (match x (`(/ ,_) t)))
             (do-tree (args)
                 (if (null (cdr args))
                     (car args)
                     (multiple-value-bind (a b) (split-parts args)
                         `(* ,(do-tree a) ,(do-tree b))))))
        (let ((muls (remove-if #'is-div args))
              (divs (mapcar #'second
                        (remove-if-not #'is-div args))))
            (if divs
                (progn
                    (unless muls
                        (push 1 muls))
                    `(/ ,(do-tree muls) ,(do-tree divs)))
                (do-tree muls)))))

(defun treeify-1 (expr old-expr)
    (match expr
        (`(ranging ,@_) old-expr)
        (`(+ ,_) expr)
        (`(- ,_) expr)
        (`(+ ,@args)
            (treeify+ args))
        (`(* ,@args)
            (treeify* args))
        (_ nil)))

(defun optimize-tree (expr)
    (simplify-rec-once #'treeify-1
        (simplify-rec-once #'pull-minus-1
            (simplify-rec-once #'flatten-exprs-1
                expr))))
