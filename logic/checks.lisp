;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *consistency-checks* nil)

(defun get-checks-for-level (level)
    (let ((clst nil))
        (maphash
            #'(lambda (expr cnt)
                  (when (eql (min-loop-level expr) level)
                      (push (list expr expr) clst)))
            *consistency-checks*)
        clst))

(defun get-check-level-set ()
    (let ((levels nil))
        (maphash
            #'(lambda (expr cnt)
                  (pushnew (min-loop-level expr) levels))
            *consistency-checks*)
        levels))

(defmacro safety-check (checks &body body)
    (let ((check-code (mapcar
                           #'(lambda (expr)
                                 `(unless ,(first expr)
                                      (error "Safety check failed: ~A" (quote ,(second expr)))))
                           checks)))
        `(progn ,@check-code ,@body)))

(defun insert-checks-1 (expr old-expr)
    (match expr
        (`(loop-range ,range ,@body)
            (let ((checks (get-checks-for-level (ranging-loop-level range))))
                (if checks
                    `(loop-range ,range (safety-check ,checks ,@body)))))
        (_ nil)))

(defun insert-checks (expr)
    (let ((new-expr (simplify-rec-once #'insert-checks-1 expr))
          (checks   (get-checks-for-level nil)))
        (if checks
            `(safety-check ,checks ,new-expr)
            new-expr)))
