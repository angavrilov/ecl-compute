;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *minlevel-cache* (make-hash-table))

(defun min-level (a b)
    (cond
        ((null a) b)
        ((null b) a)
        (t (min a b))))

(defun level< (a b)
    (or (null b)
        (and a (> a b))))

(defun level> (a b)
    (or (null a)
        (and b (> a b))))

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
            ((type atom _)
                nil)
            (`(ranging ,@_)
                (list (ranging-loop-level expr)))
            (`(temporary ,@_)
                nil)
            (`(tmp-ref ,tmp ,@args)
                (reduce #'union
                    (mapcar #'get-loop-levels args)
                    :initial-value
                        (list (temporary-level tmp))))
            (`(ptr-deref ,ptr)
                (union
                    (get-loop-levels ptr)
                    (list (temporary-level ptr))))
            (_
                (reduce #'union
                    (mapcar #'get-loop-levels expr))))))

(defun min-loop-level (expr)
    (reduce #'min-level
        (get-loop-levels expr)
        :initial-value nil))
