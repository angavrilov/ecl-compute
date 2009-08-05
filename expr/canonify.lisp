;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

;;; Canonification cache structure
(defstruct canonify-cache
  (next-id 0 :type fixnum)
  (next-sym-id 0 :type fixnum)
  (fast-lookup (make-hash-table :test #'eq))
  (slow-lookup (make-hash-table :test #'equal)))

(defparameter *canonify-cache* (make-canonify-cache))

;;; Canonified expression wrapper
(defstruct
    (canonic-expr
      (:print-function print-canonic-expr))
  (id nil :type fixnum)
  (symbol nil :type symbol)
  (node nil)
  (cache nil :type canonify-cache))

(defun print-canonic-expr (expr stream level)
  (format stream "#<~A:~S>"
          (canonic-expr-id expr) (canonic-expr-node expr)))

(defmethod compare ((e1 canonic-expr) (e2 canonic-expr))
    (let ((cv (compare-slots e1 e2 #'canonic-expr-id)))
        (if (and (eql cv :equal) (not (eql e1 e2)))
            :unequal
            cv)))

(define-cross-type-compare-methods canonic-expr)

;;; Misc

(defun canonic-expr-unwrap (expr)
  (if (canonic-expr-p expr)
      (values (canonic-expr-node expr) expr)
      expr))

(defun lookup-canonic (expr &key (cache *canonify-cache*))
  (if (canonic-expr-p expr)
      (values expr t)
      (let ((rv (gethash expr
                         (canonify-cache-fast-lookup cache))))
        (if rv
            (values rv t)
            (values expr nil)))))

(defun less? (a b)
  (eql (compare a b) :less))

;;; Canonification

(defun make-canonic (expr &key (cache *canonify-cache*))
  (let ((eq-map (canonify-cache-fast-lookup cache))
        (equal-map (canonify-cache-slow-lookup cache)))
    (labels ((lookup (expr)
               (if (canonic-expr-p expr) expr
                   (or (gethash expr eq-map)
                       (if (atom expr)
                           expr
                           (register (canonify expr))))))
             (lookup-unwrap (expr)
               (canonic-expr-unwrap (lookup expr)))
             (register (expr)
               (or (gethash expr equal-map)
                   (let ((item (make-canonic-expr
                                :node expr
                                :cache cache
                                :id (incf (canonify-cache-next-id cache)))))
                     (setf (gethash expr eq-map) item)
                     (setf (gethash expr equal-map) item)
                     item)))
             (canonify (expr)
               (match expr
                 (`(ranging ,@_)
                   expr)
                 (`(,(as op (or '+ '*)) ,@args)
                   (list* op
                          (mapcar #'canonic-expr-unwrap
                                  (stable-sort (mapcar #'lookup args)
                                               #'less?))))
                 (_
                  (mapcar #'lookup-unwrap expr)))))
      (lookup expr))))

(defun canonify-tree (expr &key (cache *canonify-cache*))
  (canonic-expr-unwrap (make-canonic expr :cache cache)))

;;; Symbol generation

(defun get-new-symbol (&key (stem "X") (cache *canonify-cache*))
  (make-symbol
   (format nil "~A~A"
           stem (incf (canonify-cache-next-sym-id cache)))))

(defun get-canonic-symbol (expr &key
                           (cache *canonify-cache*)
                           (force-new nil))
  (multiple-value-bind (c-expr item) (canonify-tree expr)
    (let ((sym (if (or (null item)
                       (and force-new
                            (canonic-expr-symbol item)))
                   (get-new-symbol :cache cache)
                   (or (canonic-expr-symbol item)
                       (setf (canonic-expr-symbol item)
                             (make-symbol
                              (format nil "C~A"
                                      (canonic-expr-id item))))))))
      (setf (get sym 'canonic-expr) (or item c-expr))
      (values sym c-expr item))))

;;; Ordered lists

(defun canonify-compare (expr1 expr2)
  (less? (lookup-canonic expr1)
         (lookup-canonic expr2)))

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
