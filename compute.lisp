;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *current-compute* nil)

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

(defparameter *consistency-checks* nil)

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
                         (idx 0 (1+ idx)))
                        ((null dims) nil)
                        (incf-nil (gethash `(<= ,(car dims) (arr-dim ,name ,idx))
                                      *consistency-checks*))))
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
                      (push (list expr expr) clst)))
            *consistency-checks*)
        clst))

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

(defun wrap-with-let (with expr)
    (if with
        (if (or (eql (car with) 'progn)
                (eql (car with) 'setf))
            `(letv ,with ,expr)
            `(let* ,with ,expr))
        expr))

(defmacro compute (&whole original name idxspec expr &key with)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (let* ((*current-compute* original)
               (idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (let-expr  (wrap-with-let with expr))
               (full-expr `(setf (iref ,name ,@idxvars) ,let-expr))
               (loop-expr (wrap-idxloops name indexes idxlist
                              (list full-expr) :min-layer 0))
               (nolet-expr (expand-let loop-expr))
               (*consistency-checks* (make-hash-table :test #'equal))
               (noiref-expr (simplify-iref nolet-expr))
               (check-expr  (insert-checks noiref-expr))
               (motion-expr (code-motion check-expr))
               (annot-expr  (annotate-types motion-expr)))
            `(let ((*current-compute* ',original))
                (declare (optimize (safety 1) (debug 1)))
                ,annot-expr))))

(defmacro calc (exprs)
    (annotate-types (code-motion (simplify-iref (expand-let (macroexpand-1 `(letv ,exprs)))))))

(setf var1 (macroexpand-1 '(compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k)))))
      var2 (macroexpand-1 '(compute HFIFi (z (+ k 1)) (+ (iref HFIFi z (1+ k)) (iref HFIFi z (1+ k)) 10))))
(pprint var1)
(pprint var2)

(defun xxx1 () (compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k)))))
(defun xxx2 () (loop-indexes HFIFi ((k :skip-high 1))
                   (compute HFIFi (z (+ k 2)) (+ (iref HFIFi z (+ k 2)) (iref HFIFi z (+ k 2)) 10))))
