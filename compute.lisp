;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(defun get-index-var (idx-spec)
    (if (or (atom idx-spec)
            (index-expr-p idx-spec))
        idx-spec
        (car idx-spec)))

(defun get-iter-spec (spec-pair)
    (let ((idx-name (caar spec-pair))
          (idx-spec (cdr spec-pair)))
        (if (index-expr-p idx-spec)
            nil
            (list
                (cons idx-name
                    (if (atom idx-spec)
                        (list idx-spec)
                        idx-spec))))))

(defun expand-let-1 (expr old-expr)
    (match expr
        (`(let ,vars ,@body)
            (replace-unquoted (wrap-progn body)
                (mapcar
                    #'(lambda (x) (cons (first x) (second x)))
                    vars)))
        (`(let* () ,@body)
            (wrap-progn body))
        (`(let* (,vspec ,@vars) ,@body)
            `(let (,vspec) (let* (,@vars) ,@body)))
        (_ nil)))

(defun expand-let (expr)
    (simplify-rec #'expand-let-1 expr (make-hash-table)))

(define-modify-macro incf-nil (&optional (delta 1))
    (lambda (val delta) (+ (or val 0) delta)))

(defparameter *minlevel-cache* (make-hash-table))

(defun min-level (a b)
    (cond
        ((null a) b)
        ((null b) a)
        (t (min a b))))

(defun min-loop-level (expr)
    (multiple-value-bind (cached-val found) (gethash expr *minlevel-cache*)
        (if found cached-val
            (setf (gethash expr *minlevel-cache*)
                (match expr
                    ((type atom _)
                        nil)
                    (`(ranging ,@_)
                        (ranging-loop-level expr))
                    (_
                        (reduce #'min-level
                            (mapcar #'min-loop-level expr))))))))

(defparameter *consistency-checks* nil)

(defun arr-dim (arr idx)
    (nth idx (array-dimensions arr)))

(defun simplify-iref-1 (expr old-expr)
    (match expr
        (`(iref ,name ,@idxvals)
            (multiple-value-bind
                (rexpr mv-p checks) (expand-iref name idxvals :verbose-p t)
                (unless mv-p
                    (error "Not a multivalue reference: ~A" expr))
                ;; Remember bound consistency checks
                (dolist (check checks)
                    (incf-nil (gethash check *consistency-checks*)))
                ;; Create dimension consistency checks
                (do ((dims (get name 'mv-dimensions) (cdr dims))
                     (idx 0 (1+ idx)))
                    ((null dims) nil)
                    (incf-nil (gethash `(<= ,(car dims) (arr-dim ,name ,idx))
                                  *consistency-checks*)))
                ;; Return the expression
                rexpr))
        (_ nil)))

(defun simplify-iref (expr)
    (simplify-rec-once #'simplify-iref-1 expr))

(defun get-checks-for-level (level)
    (let ((clst nil))
        (maphash
            #'(lambda (expr cnt)
                  (when (eql (min-loop-level expr) level)
                      (push expr clst)))
            *consistency-checks*)
        clst))

(defmacro safety-check (checks &body body)
    (let ((check-code (mapcar
                           #'(lambda (expr)
                                 `(unless ,expr
                                      (error "Safety check failed: ~A" (quote ,expr))))
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

(defmacro compute (name idxspec expr &key with)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (when (null indexes)
            (error "Unknown multivalue ~A" name))
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (let-expr  (if with `(let* ,with ,expr) expr))
               (full-expr `(setf (iref ,name ,@idxvars) ,let-expr))
               (loop-expr (wrap-idxloops name indexes idxlist
                              (list full-expr) :min-layer 0))
               (nolet-expr (expand-let loop-expr))
               (*consistency-checks* (make-hash-table :test #'equal))
               (noiref-expr (simplify-iref nolet-expr))
               (check-expr  (insert-checks noiref-expr)))
            check-expr)))

(pprint (macroexpand-1 '(compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k))))))
(pprint (macroexpand-1 '(compute HFIFi (z (+ k)) (+ (iref HFIFi z k) 10))))


