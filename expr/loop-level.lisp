;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(use-std-readtable)

(defparameter *minlevel-cache* (make-hash-table))

;;; Level comparison; NIL = infinity
(defun min-level (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (min a b))))

(defun level< (a b)
  (and a (or (null b) (< a b))))

(defun level> (a b)
  (and b (or (null a) (> a b))))

;;; Active loop level calculation
(defun temporary-level (expr)
  (match expr
    (`(ptr+ ,ptr ,@_)
      (temporary-level ptr))
    (`(temporary ,_ ,_ ,level ,@_)
      level)
    (_ nil)))

(defun get-loop-levels (expr)
  (use-cache (expr *minlevel-cache*)
    (match expr
      ((type atom _) (empty-set))
      (`(temporary ,@_) (empty-set))
      ((ranging-spec _ :loop-level level)
       (set level))
      (`(tmp-ref ,tmp ,@args)
        (reduce #'union
                (mapcar #'get-loop-levels args)
                :initial-value
                (set (temporary-level tmp))))
      (`(ptr-deref ,ptr)
        (union (get-loop-levels ptr)
               (set (temporary-level ptr))))
      (_ (reduce #'union
                 (mapcar #'get-loop-levels expr))))))

(defun min-loop-level (expr)
  (reduce #'min-level
          (get-loop-levels expr)
          :initial-value nil))

;;; Split multiple-arity sums and products by loop level
(defun split-by-level-1 (expr &optional old-expr)
  (match (canonic-unwrap-all expr)
    (`(,(as op (or '+ '*)) ,@args)
      (nlet ((sum-level (min-loop-level args))
             ((upper cur (split-list #'(lambda (arg)
                                         (level< sum-level (min-loop-level arg)))
                                     args))))
        (if (or (third upper)
                (and (not (numberp (first upper)))
                     (second upper)))
            `(,op ,(split-by-level-1 `(,op ,@upper))
                  ,@cur)
            expr)))
    (_ expr)))

