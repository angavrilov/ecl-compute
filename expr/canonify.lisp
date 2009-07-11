;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

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

