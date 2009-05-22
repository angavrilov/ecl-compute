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

(defun apply-hash-change (table vals)
    (mapcar #'(lambda (assn)
                (let ((key (car assn)))
                    (prog1
                        (cons key (gethash key table))
                        (setf (gethash key table) (cdr assn)))))
        vals))

(defmacro with-hash-update (table vals &body rest)
    (let ((ss (gensym)))
        `(let ((,ss (apply-hash-change ,table ,vals)))
            (unwind-protect (progn ,@rest)
                (apply-hash-change ,table ,ss)))))

(defun expand-let-1 (expr table)
    (or (gethash expr table)
        (match expr
            ((type atom _) expr)
            (`(quote ,@_) expr)
            (`(let ,vars ,@body)
                (with-hash-update table
                    (mapcar
                        #'(lambda (x)
                            (cons (first x)
                                (expand-let-1 (second x) table)))
                        vars)
                    (expand-let-1 (wrap-progn body) table)))
            (`(let* () ,@body)
                (expand-let-1 (wrap-progn body) table))
            (`(let* (,vspec ,@vars) ,@body)
                (expand-let-1
                    `(let (,vspec) (let* (,@vars) ,@body))
                    table))
            (`(letv ,vars ,@body)
                (expand-let-1 (macroexpand-1 expr) table))
            (_
                (mapcar-save-old
                    #'(lambda (e) (expand-let-1 e table))
                    expr)))))

(defun expand-let (expr)
    (expand-let-1 expr (make-hash-table)))

(define-modify-macro incf-nil (&optional (delta 1))
    (lambda (val delta) (+ (or val 0) delta)))

(defmacro use-cache ((key cache) &body code)
    (let ((cached-val (gensym))
          (found (gensym)))
        `(multiple-value-bind (,cached-val ,found) (gethash ,key ,cache)
            (if ,found ,cached-val
                (setf (gethash ,key ,cache)
                    (progn ,@code))))))

(defparameter *minlevel-cache* (make-hash-table))

(defun min-level (a b)
    (cond
        ((null a) b)
        ((null b) a)
        (t (min a b))))

(defun min-loop-level (expr)
    (use-cache (expr *minlevel-cache*)
        (match expr
            ((type atom _)
                nil)
            (`(ranging ,@_)
                (ranging-loop-level expr))
            (_
                (reduce #'min-level
                    (mapcar #'min-loop-level expr))))))

(defparameter *consistency-checks* nil)

(defun arr-dim (arr idx)
    (nth idx (array-dimensions arr)))

(defun simplify-iref-1 (expr old-expr)
    (match expr
        (`(iref ,name ,@idxvals)
            (multiple-value-bind
                (rexpr mv-p checks) (expand-iref name idxvals :verbose-p *consistency-checks*)
                (unless mv-p
                    (error "Not a multivalue reference: ~A" expr))
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

(defun build-factor-table (cnt-table pull-symbols)
    (let ((fct-table (make-hash-table :test #'equal)))
        (maphash
            #'(lambda (expr cnt)
                  ;; Factor common subexpressions
                  (when (> cnt 1)
                      (setf (gethash expr fct-table) t))
                  ;; Factor symbols
                  (when (and pull-symbols (symbolp expr))
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
                            ((type symbol var) (not pull-symbols))
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

(defun code-motion (expr &key pull-symbols)
    (factor-vars expr (build-factor-table (count-subexprs expr) pull-symbols)))

(defun recurse-factored (fun expr &rest args)
    (let ((full-expr (if (symbolp expr) (get expr 'let-clause))))
        (apply fun (or (cadr full-expr) expr) args)))

(defparameter *upper-type-cache* nil)

(defun propagate-upper-type (expr type)
    (let ((cur-type (gethash expr *upper-type-cache*)))
        (when (and cur-type type (not (eql cur-type type))
                    (not (and (eql type 'float) (eql cur-type 'integer))))
            (error "Conflicting type requirement: ~A must be both ~A and ~A"
                    expr type cur-type))
        (unless (or cur-type (numberp expr))
            (when type
                (setf (gethash expr *upper-type-cache*) type))

            (labels ((mark-list (subexprs subtype)
                        (dolist (e subexprs)
                            (recurse-factored #'propagate-upper-type e subtype))))
                (match expr
                    ((type atom _) nil)
                    (`(ranging ,expr ,min ,max ,@_)
                        (mark-list (list expr min max) 'integer))
                    (`(,(or 'aref 'iref) ,_ ,@indices)
                        (mark-list indices 'integer))
                    (`(,(or '+ '- '* '/ 'mod 'rem 'floor 'ceiling 'truncate) ,@rest)
                        (mark-list rest type))
                    (`(,(or 'and 'or) ,@rest)
                        (mark-list rest 'boolean))
                    (`(if ,cond ,@rest)
                        (mark-list (list cond) type)
                        (mark-list rest type))
                    (`(,(or 'let 'let*) ,_ ,@rest)
                        (mark-list (last rest) type))
                    (`(,(or '> '< '>= '<= '/= '= 'setf 'loop-range) ,@rest)
                        (mark-list rest nil))
                    (`(_ ,@rest)
                        (mark-list rest nil)))))))

(defparameter *bottom-type-cache* nil)

(defun get-bottom-type-1 (expr)
    (use-cache (expr *bottom-type-cache*)
        (let ((upper-type (gethash expr *upper-type-cache*)))
            (labels ((merge-types (rest)
                        (let ((types (mapcar #'get-bottom-type rest)))
                            (when (find 'boolean types)
                                (error "Cannot do arithmetics with booleans: ~A" expr))
                            (cond
                                ((find 'float types) 'float)
                                ((every #'(lambda (tp) (eql tp 'integer)) types)
                                    'integer)
                                (t nil)))))
                (match expr
                    ((type float _) 'float)
                    ((type integer _) 'integer)
                    ((type symbol s) upper-type)
                    (`(ranging ,@_) 'integer)
                    (`(,(or 'aref 'iref) ,_ ,@idxlst)
                        (dolist (idx idxlst) (get-bottom-type idx))
                        'float)
                    (`(,(or '+ '- '* '/ 'mod 'rem 'floor 'ceiling 'truncate) ,@rest)
                        (merge-types rest))
                    (`(if ,cond ,tb ,eb)
                        (merge-types (list tb eb)))
                    (`(,(or '> '< '>= '<= '/= '= 'and 'or) ,@rest)
                        (dolist (arg rest) (get-bottom-type arg))
                        'boolean)
                    (`(,(or 'let 'let*) ,_ ,@rest)
                        (dolist (arg rest) (get-bottom-type arg))
                        (merge-types (last rest)))
                    (`(,_ ,@rest)
                        (dolist (arg rest) (get-bottom-type arg))
                        nil)
                    (_ nil))))))

(defun get-bottom-type (expr)
    (recurse-factored #'get-bottom-type-1 expr))

(defun apply-skipping-structure (fun expr args)
    (match expr
        (`(,(or 'let 'let* 'loop-range) ,_ ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(safety-check ,_ ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(setf ,tgt ,src)
            (apply fun tgt args)
            (apply fun src args))
        (`(declare ,@_) nil)
        (_
;            (format t "Unknown structure statement: ~A" expr)
            (apply fun expr args))))

(defun derive-types (expr)
    (let ((*bottom-type-cache* (make-hash-table))
          (*upper-type-cache* (make-hash-table)))
        (propagate-upper-type expr nil)
        (apply-skipping-structure #'get-bottom-type expr nil)
        (maphash
            #'(lambda (sub type)
                  (let ((upper (gethash sub *upper-type-cache*)))
                      (when (and upper type (not (eql upper type))
                                (not (and (eql upper 'float)
                                          (eql type 'integer))))
                          (error "Type conflict: ~A is ~A, required ~A~%~A"
                              sub type upper expr))
                      (when (and type (not upper))
                          (propagate-upper-type sub type))))
            *bottom-type-cache*)
        (clrhash *bottom-type-cache*)
        (apply-skipping-structure #'get-bottom-type expr nil)
        *bottom-type-cache*))

(defun annotate-types (expr)
    (let ((types (derive-types expr)))
        (simplify-rec-once
            #'(lambda (expr old-expr)
                (match expr
                    (`(,(or 'let 'let*) ,@_)
                        nil)
                    (`(ranging ,@_)
                        old-expr)
                    (`(setf (the ,_ ,arg) ,tgt)
                        `(setf ,arg ,tgt))
                    (_
                        (multiple-value-bind (type found) (gethash old-expr types)
                            (if found
                                (let ((tspec (match type
                                                ('float 'single-float)
                                                ('integer 'fixnum)
                                                ('boolean 'boolean)
                                                ('nil 'single-float)
                                                (_ (error "Bad type ~A" type)))))
                                    `(the ,tspec ,expr))
                                expr)))))
            expr)))

(defparameter *current-compute* nil)

(defmacro compute (&whole original name idxspec expr &key with)
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
