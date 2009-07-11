;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *iref-cache* (make-hash-table))

(defun check-dimension (expr dim verbose-p)
    (let* ((range (compute-any-range expr))
           (min-val (car range))
           (max-val (cdr range))
           (checks  nil))
        (when (and (null range) verbose-p)
            (format t "Cannot determine range for: ~A~%" expr))
        (case (compare-indexes (or min-val expr) 0)
            (<
                (error "Index ~A can be less than the 0 limit" expr))
            ((> =) nil)
            (t
                (when verbose-p
                    (format t "Cannot compare ~A~%  as min value ~A with 0~%" expr min-val))
                (push `(>= ,(or min-val expr) 0) checks)))
        (case (compare-indexes (or max-val expr) dim)
            (< nil)
            ((> =)
                (error "Index ~A can reach the top limit of ~A" expr dim))
            (t
                (when verbose-p
                    (format t "Cannot compare ~A~%  as max value ~A with ~A~%" expr max-val dim))
                (push `(< ,(or max-val expr) ,dim) checks)))
        checks))

(defun expand-iref (name idxvals &key verbose-p)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (if (null indexes)
            `(aref ,name ,@idxvals)
            (let* ((idxtab (mapcar #'cons indexes idxvals))
                   (idxord (reorder idxtab layout #'caar))
                   (idxlst (mapcan #'index-refexpr idxord))
                   (dimchk (mapcan #'(lambda (iexpr dim)
                                         (check-dimension iexpr dim verbose-p))
                               idxlst dimensions)))
                (when (/= (length idxvals) (length indexes))
                    (error "Index count mismatch for ~A: ~A instead of ~A"
                        name idxvals indexes))
                (values
                    `(aref
                         (multivalue-data ,name ,(if *current-compute* t))
                         ,@idxlst)
                    t dimchk)))))

(defmacro iref (&whole form name &rest idxvals)
    (let ((cached (gethash form *iref-cache*)))
        (if cached cached
            (setf (gethash form *iref-cache*)
                (expand-iref name idxvals)))))

(defmacro enable-expr-quotes ()
   `(eval-when (:compile-toplevel :execute)
        (formula:enable-expr-quotes)
        (setf formula:*index-access-symbol* 'iref)))

(defun check-index-alignment (expr iref-expr aref-expr)
    (match expr
        (`(ranging ,_ ,minv ,maxv ,step ,@_)
            (cons step
                (or (force-integer
                        (simplify-index `(rem ,minv ,step)))
                    (force-integer
                        (simplify-index `(rem ,maxv ,step))))))
        (`(* ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (* (or (car alignment) 0) b)
                      (* (or (cdr alignment) 1) b))))
        (`(+ ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (car alignment)
                      (if (cdr alignment) (+ (cdr alignment) b)))))
        (`(- ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (car alignment)
                      (if (cdr alignment) (- (cdr alignment) b)))))
        (`(/ ,a ,(type number b))
            (let* ((alignment (check-index-alignment a iref-expr aref-expr))
                   (step-ref  (/ (or (car alignment) 0) b))
                   (ofs-ref   (/ (or (cdr alignment) 1) b)))
                (unless (integerp step-ref)
                    (error "Too dense index iteration: ~A (~A,~A)~% in ~A~% orig ~A~%"
                        (remove-ranges expr) step-ref ofs-ref
                        (remove-ranges iref-expr)
                        (remove-ranges aref-expr)))
                (unless (integerp ofs-ref)
                    (if (/= step-ref 0)
                        (error "Misaligned reference: ~A (~A,~A)~% in ~A~% orig ~A~%"
                            (remove-ranges expr) step-ref ofs-ref
                            (remove-ranges iref-expr)
                            (remove-ranges aref-expr))
                        (when (or (null *consistency-checks*)
                                  (not (null (min-loop-level expr)))
                                  (= 1 (incf-nil
                                               (gethash `(= (rem ,expr ,b) 0)
                                                   *consistency-checks*))))
                            (format t
                                "~%Possibly misaligned reference: ~A (~A,~A)~% in ~A~% orig ~A~%"
                                (remove-ranges expr) step-ref ofs-ref
                                (remove-ranges aref-expr)
                                (remove-ranges iref-expr)))))
                (cons (force-integer step-ref)
                      (force-integer ofs-ref))))
        (`(,_ ,@rest)
            (dolist (arg rest) (check-index-alignment arg iref-expr aref-expr))
            nil)))

(defun simplify-iref-1 (expr old-expr)
    (match expr
        (`(iref ,name ,@idxvals)
            (multiple-value-bind
                (rexpr mv-p checks) (expand-iref name idxvals :verbose-p *consistency-checks*)
                (unless mv-p
                    (error "Not a multivalue reference: ~A" expr))
                (check-index-alignment rexpr expr rexpr)
                (when *consistency-checks*
                    ;; Remember bound consistency checks
                    (dolist (check checks)
                        (incf-nil (gethash check *consistency-checks*)))
                    ;; Create dimension consistency checks
                    (do ((dims (get name 'mv-dimensions) (cdr dims))
                         (rank (length (get name 'mv-dimensions)))
                         (idx 0 (1+ idx)))
                        ((null dims) nil)
                        (incf-nil
                            (gethash `(<= ,(car dims)
                                          (arr-dim (multivalue-data ,name t)
                                              ,idx ,rank))
                                      *consistency-checks*))))
                ;; Return the expression
                rexpr))
        (_ nil)))

(defun simplify-iref (expr)
    (simplify-rec-once #'simplify-iref-1 expr))
