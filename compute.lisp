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

(defmacro letv (exprs &body blk)
    (let* ((elst (if (and (consp exprs) (eql (car exprs) 'progn))
                     (cdr exprs)
                     (list exprs)))
           (nonull (remove-if-not #'identity elst))
           (body   (if blk blk (last nonull)))
           (noblk  (if blk nonull (butlast nonull)))
           (items  (mapcar
                        #'(lambda (expr)
                            (match expr
                                (`(setf ,(type symbol var) ,vexpr)
                                    (list var vexpr))
                                (_
                                    (error "letv: invalid expression ~A" expr))))
                        noblk)))
        (if items
            `(let* ,items ,@body)
            body)))

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
        (`(letv ,vars ,@body)
            (expand-let-1 (macroexpand-1 expr) old-expr))
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

(defun count-subexprs-rec (expr cnt-table)
    ;; Increment the counter, and avoid walking trees twice
    (when (> (incf-nil (gethash expr cnt-table)) 1)
        (return-from count-subexprs-rec nil))
    (match expr
        ((type atom var)
            nil)
        (`(ranging ,@_)
            nil)
        (`(loop-range ,_ ,@body)
            (dolist (item body)
                (count-subexprs-rec item cnt-table)))
        (`(setf ,target ,val)
            (dolist (titem (cdr target))
                (count-subexprs-rec titem cnt-table))
            (count-subexprs-rec val cnt-table))
        (_
            (dolist (item (cdr expr))
                (count-subexprs-rec item cnt-table)))))

(defun count-subexprs (expr)
    (let ((table (make-hash-table :test #'equal)))
        (count-subexprs-rec expr table)
        table))

(defun build-factor-table (cnt-table)
    (let ((fct-table (make-hash-table :test #'equal)))
        (maphash
            #'(lambda (expr cnt)
                  ;; Factor common subexpressions
                  (when (> cnt 1)
                      (setf (gethash expr fct-table) t))
                  ;; Factor loop-invariant subexpressions
                  (when (and (consp expr)
                            (not (find (car expr)
                                     '(ranging safety-check loop-range setf))))
                      (let ((level (min-loop-level expr)))
                          (dolist (sub (cdr expr))
                              (unless (eql level (min-loop-level sub))
                                  (setf (gethash sub fct-table) t))))))
            cnt-table)
        (maphash
            #'(lambda (expr flag)
                  ;; Don't factor constants and loop vars
                  (when (match expr
                            ((type number _) t)
                            ('nil t)
                            ((type symbol var) (get var 'mv-indexes))
                            (`(ranging ,@_) (ranging-loop-level expr))
                            (_ nil))
                      (remhash expr fct-table)))
            fct-table)
        fct-table))

(defun factor-vars-dumb (expr fct-table cur-level var-list nil-list)
    (if (or (atom expr) (find (car expr) '(ranging)))
        expr
        (mapcar-save-old
            #'(lambda (x) (factor-vars-rec x fct-table cur-level var-list nil-list))
            expr)))

(defun factor-vars-rec (expr fct-table cur-level var-list nil-list)
    (match expr
        (`(loop-range ,range ,@body)
            (let* ((level (ranging-loop-level range))
                   (vlist (cons nil var-list))
                   (nbody (factor-vars-rec body fct-table level vlist nil-list)))
                (unless (or (null cur-level) (= level (1- cur-level)))
                    (error "Invalid loop nesting: ~A at level ~A" expr cur-level))
                (if (car vlist)
                    `(loop-range ,range (let* ,(nreverse (car vlist)) ,@nbody))
                    `(loop-range ,range ,@nbody))))
        (`(setf ,target ,val)
            `(setf ,(factor-vars-dumb target fct-table cur-level var-list nil-list)
                 ,(factor-vars-rec val fct-table cur-level var-list nil-list)))
        (_
            (let ((factor (gethash expr fct-table)))
                (cond
                    ((eql factor nil)
                        (factor-vars-dumb expr fct-table cur-level var-list nil-list))
                    ((eql factor t)
                        (let* ((sym   (gensym))
                               (nexpr (factor-vars-dumb expr fct-table cur-level var-list nil-list))
                               (level (min-loop-level expr))
                               (expr-pair (list sym nexpr)))
                            (setf (get sym 'let-clause) expr-pair)
                            (setf (get sym 'full-expr) expr)
                            (setf (get sym 'loop-level) level)
                            (cond
                                ((eql level nil)
                                    (push expr-pair (car nil-list)))
                                ((and cur-level (>= level cur-level))
                                    (push expr-pair (nth (- level cur-level) var-list)))
                                (t
                                    (error "Invalid level ~A at current ~A" level cur-level)))
                            (setf (gethash expr fct-table) sym)))
                    (t  factor))))))

(defun factor-vars (expr fct-table)
    (let* ((nil-list (list nil))
           (nexpr    (factor-vars-rec expr fct-table nil nil nil-list)))
        (if (car nil-list)
            `(let* ,(nreverse (car nil-list)) ,nexpr)
            nexpr)))

(defun code-motion (expr)
    (factor-vars expr (build-factor-table (count-subexprs expr))))

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
               (check-expr  (insert-checks noiref-expr))
               (motion-expr (code-motion check-expr)))
            motion-expr)))

(setf var1 (macroexpand-1 '(compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k)))))
      var2 (macroexpand-1 '(compute HFIFi (z (+ k 1)) (+ (iref HFIFi z (1+ k)) (iref HFIFi z (1+ k)) 10))))
(pprint var1)
(pprint var2)

(defun xxx1 () (compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k)))))
(defun xxx2 () (loop-indexes HFIFi ((k :skip-high 1))
                   (compute HFIFi (z (+ k 2)) (+ (iref HFIFi z (+ k 2)) (iref HFIFi z (+ k 2)) 10))))
