;;;; kate: indent-width 4; replace-tabs yes; space-indent on;


(defun toggle-minus (expr)
    (match expr
        (`(- ,x) x)
        (x `(- ,x))))

(defun flatten+ (args)
    (delete-if
        #'(lambda (x) (and (numberp x) (= x 0)))
        (mapcan
            #'(lambda (arg)
                  (match arg
                      (`(+ ,@lst)
                          (copy-list lst))
                      (`(- (+ ,@lst))
                          (mapcar #'toggle-minus lst))
                      (_ (list arg))))
            args)))

(defun toggle-div (expr)
    (match expr
        (`(/ ,x) x)
        (x `(/ ,x))))

(defun flatten* (args)
    (delete-if
        #'(lambda (x) (and (numberp x) (= x 1)))
        (mapcan
            #'(lambda (arg)
                  (match arg
                      (`(* ,@lst)
                          (copy-list lst))
                      (`(/ (* ,@lst))
                          (mapcar #'toggle-div lst))
                      (_ (list arg))))
            args)))

(defun flatten-exprs-1 (expr old-expr)
    (match expr
        (`(ranging ,@_) old-expr)
        (`(+ ,x)     x)
        (`(* ,x)     x)
        (`(- (- ,x)) x)
        (`(/ (/ ,x)) x)
        (`(- ,_)     expr)
        (`(/ ,_)     expr)
        (`(- ,x ,@rest)
            (flatten-exprs-1
                `(+ ,x ,@(mapcar #'toggle-minus
                            (flatten+ rest)))
                old-expr))
        (`(+ ,@args)
            `(+ ,@(flatten+ args)))
        (`(/ ,x ,@rest)
            (flatten-exprs-1
                `(* ,x ,@(mapcar #'toggle-div
                            (flatten* rest)))
                old-expr))
        (`(* ,@args)
            `(* ,@(flatten* args)))
        (_ nil)))

(defun pull-minus-1 (expr old-expr)
    (match expr
        (`(ranging ,@_) old-expr)
        (`(- (- ,x)) x)
        (`(* ,@args)
            (let* ((minus-cnt 0)
                   (args2 (mapcar
                              #'(lambda (arg)
                                   (match arg
                                       (`(- ,x)
                                           (incf minus-cnt)
                                           x)
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
            (if (every #'(lambda (arg)
                             (match arg (`(- ,_) t)))
                    args)
                `(- (+ ,@(mapcar #'second args)))))
        (_ nil)))

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
