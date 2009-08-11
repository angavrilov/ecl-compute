;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(use-std-readtable)

;;; Select a suitable representation based on the list length
(defun wrap-assoc-op (lst op zero)
  (cond ((null lst) zero)
        ((null (cdr lst)) (car lst))
        (t (list* op lst))))

;;; Combine values, ignoring nil
(defun reduce-non-nil (op a b)
  (cond ((null a) b)
        ((null b) a)
        (t (funcall op a b))))

;;; Remove items that are symmetric wrt inv-func
(defun cancel-items (items inv-func)
  (let* ((ritems (image #f(canonic (funcall inv-func) _) items))
         (common (intersection items ritems)))
    (if (empty? common)
        items
        (progn
          (format t "Cancelled out: ~A~%" common)
          (bag-difference items common)))))

;;; Collect terms while combining numbers and removing redundant ones.
(defun collect-terms (reduce-op inv-op tag-op zero exprs &key null-value)
  (let ((items (empty-bag))
        (value nil))
    (dolist (item exprs)
      (if (numberp item)
          (setf value (reduce-non-nil reduce-op value item))
          (adjoinf items (make-canonic item))))
    (if (and null-value value (= value null-value))
        (progn
          (format t "Reduced to ~A: ~A~%" null-value exprs)
          null-value)
        (let ((items (cancel-items items inv-op)))
          (when (and value (not (= value zero)))
            (adjoinf items value))
          (wrap-assoc-op (canonic-bag-to-list items)
                         tag-op zero)))))

;;; Flattening of addition
(defun toggle-minus (expr)
  (match expr
    ((type number num) (- num))
    (`(- ,x) x)
    (x `(- ,x))))

(defun collect+ (args)
  (collect-terms #'+ #'toggle-minus '+ 0 args))

(defun flatten+-args (args)
  (mapcan #'(lambda (arg)
              (match arg
                (`(+ ,@lst)
                  (copy-list lst))
                (`(- ,(type number val))
                  (list (- val)))
                (`(- (+ ,@lst))
                  (mapcar #'toggle-minus lst))
                (_ (list arg))))
          args))

(defun flatten+ (args)
  (collect+ (flatten+-args args)))

;;; Flattening of multiplication
(defun toggle-div (expr)
  (match expr
    (1 1)
    (`(/ ,x) x)
    (x `(/ ,x))))

(defun collect* (args)
  (collect-terms #'* #'toggle-div '* 1 args :null-value 0))

(defun flatten*-args (args)
  (mapcan #'(lambda (arg)
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
          args))

(defun flatten* (args)
  (collect* (flatten*-args args)))

;;; Flattening walker
(def-rewrite-pass flatten-exprs (:canonic t)
  (`(+ ,x)     x)
  (`(* ,x)     x)
  (`(- (- ,x)) x)
  (`(/ (/ ,x)) x)
  (`(- ,(type number val)) (- val))
  (`(- ,_)     expr)
  (`(/ ,_)     expr)
  (`(- ,x ,@rest)
    (flatten+ `(,x (- ,(flatten+ rest)))))
  (`(+ ,@args)
    (flatten+ args))
  (`(/ ,x ,@rest)
    (flatten* `(,x (/ ,(flatten* rest)))))
  (`(* ,@args)
    (flatten* args)))
