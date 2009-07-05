;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defmacro _grp (arg) arg)

(defun replace-let (let-data replace-tbl)
    (let ((new-defs (mapcar-save-old
                        #'(lambda (item)
                              (cons-save-old item
                                  (car item)
                                  (replace-unquoted (cdr item) replace-tbl)))
                        (car let-data)))
          (new-table (set-difference replace-tbl (car let-data) :key #'car)))
        (cons-save-old let-data
            new-defs
            (mapcar-save-old
                #'(lambda (subexpr) (replace-unquoted subexpr new-table))
                (cdr let-data)))))

(defun replace-let* (let-data replace-tbl)
    (let* ((new-table replace-tbl)
           (new-defs (mapcar-save-old
                         #'(lambda (item)
                               (let ((newv (replace-unquoted (cdr item) new-table)))
                                   (setf new-table
                                       (remove (car item) new-table :key #'car))
                                   (cons-save-old item (car item) newv)))
                         (car let-data))))
        (cons-save-old let-data
            new-defs
            (mapcar-save-old
                #'(lambda (subexpr) (replace-unquoted subexpr new-table))
                (cdr let-data)))))

(defun replace-unquoted (expr replace-tbl)
    (let ((target (cdr (assoc expr replace-tbl))))
        (cond
            (target target)
            ((atom expr) expr)
            ((null replace-tbl) expr)
            ((eql (first expr) 'quote) expr)
            ((eql (first expr) 'let)
                (cons-save-old expr
                    'let (replace-let (cdr expr) replace-tbl)))
            ((eql (first expr) 'symbol-macrolet)
                (cons-save-old expr
                    'symbol-macrolet (replace-let (cdr expr) replace-tbl)))
            ((eql (first expr) 'let*)
                (cons-save-old expr
                    'let* (replace-let* (cdr expr) replace-tbl)))
            (t (cons-save-old expr
                   (replace-unquoted (car expr) replace-tbl)
                   (replace-unquoted (cdr expr) replace-tbl))))))

(defun subst-save-old (new old expr)
    (cond
        ((eql expr old)
            new)
        ((atom expr)
            expr)
        (t
            (cons-save-old expr
                (subst-save-old new old (car expr))
                (subst-save-old new old (cdr expr))))))

(defun wrap-progn (code)
    (if (cdr code) `(progn ,@code) (car code)))

(defun wrap-progn-filter (code)
    (wrap-progn
        (mapcan
            #'(lambda (expr)
                  (match expr
                      ('nil nil)
                      (`(declare ,@_) nil)
                      (`(progn ,@code) code)
                      (_ (list expr))))
            code)))

(defun convert-letv-exprs (exprs &key pull-last)
    (let* ((elst (if (and (consp exprs) (eql (car exprs) 'progn))
                     (cdr exprs)
                     (list exprs)))
           (nonull (remove-if-not #'identity elst))
           (body   (if pull-last (last nonull) nil))
           (noblk  (if pull-last (butlast nonull) nonull))
           (items  (mapcar
                        #'(lambda (expr)
                            (match expr
                                (`(setf ,(type symbol var) ,vexpr)
                                    (list var vexpr))
                                (_
                                    (error "letv: invalid expression ~A" expr))))
                        noblk)))
        (values items body)))

(defun convert-letv-exprs-auto (exprs)
    (if (or (eql (car exprs) 'progn)
            (eql (car exprs) 'setf))
        (convert-letv-exprs exprs)
        exprs))

(defmacro letv (exprs &body blk)
    (multiple-value-bind
            (items body)
            (convert-letv-exprs exprs :pull-last (null blk))
        (if items
            `(let* ,items ,@(or blk body))
            (wrap-progn (or blk body)))))

(defun wrap-with-let (with expr)
    (if with
        `(let* ,(convert-letv-exprs-auto with) ,expr)
        expr))

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
            (`(declare ,@_) expr)
            (`(,(or 'let 'symbol-macrolet) ,vars ,@body)
                (with-hash-update table
                    (mapcar
                        #'(lambda (x)
                            (cons (first x)
                                (expand-let-1 (second x) table)))
                        vars)
                    (expand-let-1 (wrap-progn-filter body) table)))
            (`(let* () ,@body)
                (expand-let-1 (wrap-progn-filter body) table))
            (`(let* (,vspec ,@vars) ,@body)
                (expand-let-1
                    `(let (,vspec) (let* (,@vars) ,@body))
                    table))
            (`(letv ,vars ,@body)
                (expand-let-1 (macroexpand-1 expr) table))
            (`(progn ,@code)
                (wrap-progn-filter
                    (mapcar #'(lambda (x)
                                  (expand-let-1 x table))
                        code)))
            (_
                (mapcar-save-old
                    #'(lambda (e) (expand-let-1 e table))
                    expr)))))

(defun expand-let (expr)
    (expand-let-1 expr (make-hash-table)))

(defun apply-skipping-structure (fun expr args)
    (match expr
        (`(progn ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(,(or 'let 'let* 'symbol-macrolet 'loop-range) ,_ ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(safety-check ,checks ,@rest)
            (dolist (item checks)
                (apply-skipping-structure fun (first item) args))
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(setf ,_ ,_)
            (apply fun expr args))
        (`(declare ,@_) nil)
        (_
;            (format t "Unknown structure statement: ~A" expr)
            (apply fun expr args))))

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

(defun min-loop-level (expr)
    (use-cache (expr *minlevel-cache*)
        (match expr
            ((type atom _)
                nil)
            (`(ranging ,@_)
                (ranging-loop-level expr))
            (`(temporary ,@_)
                nil)
            (`(tmp-ref ,tmp ,@args)
                (reduce #'min-level
                    (mapcar #'min-loop-level args)
                    :initial-value
                        (temporary-level tmp)))
            (`(ptr-deref ,ptr)
                (min-level
                    (min-loop-level ptr)
                    (temporary-level ptr)))
            (_
                (reduce #'min-level
                    (mapcar #'min-loop-level expr))))))

(defmacro arr-dim (arr idx rank)
    `(array-dimension ,arr ,idx))

(defun force-integer (expr)
    (if (integerp expr) expr nil))


(defun count-subexprs-rec (expr cnt-table)
    ;; Increment the counter, and avoid walking trees twice
    (when (> (incf-nil (gethash expr cnt-table)) 1)
        (return-from count-subexprs-rec nil))
    (match expr
        ((type atom var)
            nil)
        (`(ranging ,@_)
            nil)
        (`(declare ,@_)
            nil)
        (`(multivalue-data ,@_)
            nil)
        (`(loop-range (ranging ,_ ,minv ,maxv ,@_) ,@body)
            (count-subexprs-rec minv cnt-table)
            (count-subexprs-rec maxv cnt-table)
            (dolist (item body)
                (count-subexprs-rec item cnt-table)))
        (`(temporary ,_ ,dims ,@_)
            (dolist (titem dims)
                (count-subexprs-rec titem cnt-table)))
        (`(setf ,target ,val)
            (dolist (titem (cdr target))
                (count-subexprs-rec titem cnt-table))
            (count-subexprs-rec val cnt-table))
        (`(safety-check ,checks ,@body)
            (dolist (check checks)
                (count-subexprs-rec (first check) cnt-table))
            (dolist (item body)
                (count-subexprs-rec item cnt-table)))
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
                  ;; Factor temporaries and arrays
                  (ifmatch `(,(or 'temporary 'multivalue-data) ,@_) expr
                      (setf (gethash expr fct-table) t))
                  ;; Factor references used on the lhs of an
                  ;; assignment, and also somewhere else
                  (when (and (consp expr)
                             (eql (car expr) 'setf)
                             (>= (or (gethash (second expr) cnt-table) 0) 1))
                      (setf (gethash (second expr) fct-table) t))
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
                            (`(_grp ,@_) t)
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
        (`(declare ,@_) expr)
        (`(quote ,@_) expr)
        (`(loop-range ,range ,@body)
            (let* ((level (ranging-loop-level range))
                   (level-gap (if cur-level (- cur-level level 1) 0))
                   (pad-list (loop for i from 1 to level-gap collect nil))
                   (vlist (cons nil (nconc pad-list var-list)))
                   (nbody (factor-vars-rec body fct-table level vlist nil-list)))
                (unless (or (null cur-level) (< level cur-level))
                    (error "Invalid loop nesting: ~A at level ~A" expr cur-level))
                ;; Factor the loop range args
                (setf (third range)
                    (factor-vars-rec (third range) fct-table cur-level var-list nil-list))
                (setf (fourth range)
                    (factor-vars-rec (fourth range) fct-table cur-level var-list nil-list))
                ;; Pop the substitutions
                (dolist (subs (car vlist))
                    (setf (gethash
                              (get (first subs) 'full-expr)
                              fct-table)
                        t))
                ;; Verify that the gap is empty
                (loop for i from 1 to level-gap
                 do (unless (null (nth i vlist))
                        (error "Invalid loop nesting: ~A of level ~A in gap ~A to ~A"
                            (nth i vlist) (+ level i) level cur-level)))
                ;; Wrap with let if needed
                (if (car vlist)
                    `(loop-range ,range (let* ,(nreverse (car vlist)) ,@nbody))
                    `(loop-range ,range ,@nbody))))
        (`(setf ,target ,val)
            `(setf ,(factor-vars-dumb target fct-table cur-level var-list nil-list)
                 ,(optimize-tree
                     (factor-vars-rec val fct-table cur-level var-list nil-list))))
        (`(safety-check ,checks ,@body)
            (cons-save-old expr 'safety-check
                (cons-save-old (cdr expr)
                    (mapcar-save-old
                        #'(lambda (check)
                              (cons-save-old check
                                  (factor-vars-rec (car check)
                                      fct-table cur-level var-list nil-list)
                                  (cdr check)))
                        checks)
                    (factor-vars-dumb body
                        fct-table cur-level var-list nil-list))))
        (_
            (let ((factor (gethash expr fct-table)))
                (cond
                    ((eql factor nil)
                        (factor-vars-dumb expr fct-table cur-level var-list nil-list))
                    ((eql factor t)
                        (let* ((sym   (gensym))
                               (nexpr (factor-vars-dumb expr fct-table cur-level var-list nil-list))
                               (level (min-loop-level expr))
                               (expr-pair (list sym (optimize-tree nexpr))))
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

(defun unwrap-factored (expr)
    (let ((full-expr (if (symbolp expr) (get expr 'let-clause))))
        (or (cadr full-expr) expr)))

(defun recurse-factored (fun expr &rest args)
    (apply fun (unwrap-factored expr) args))
