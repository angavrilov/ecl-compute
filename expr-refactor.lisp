;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

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
            ((eql (first expr) 'let*)
                (cons-save-old expr
                    'let* (replace-let* (cdr expr) replace-tbl)))
            (t (cons-save-old expr
                   (replace-unquoted (car expr) replace-tbl)
                   (replace-unquoted (cdr expr) replace-tbl))))))

(defun wrap-progn (code)
    (if (cdr code) `(progn ,@code) (car code)))

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
            (`(declare ,@_) expr)
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

(defun apply-skipping-structure (fun expr args)
    (match expr
        (`(,(or 'let 'let* 'loop-range) ,_ ,@rest)
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

(defun arr-dim (arr idx)
    (nth idx (array-dimensions arr)))

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
        (`(loop-range (ranging ,_ ,minv ,maxv ,@_) ,@body)
            (count-subexprs-rec minv cnt-table)
            (count-subexprs-rec maxv cnt-table)
            (dolist (item body)
                (count-subexprs-rec item cnt-table)))
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
        (`(declare ,@_) expr)
        (`(loop-range ,range ,@body)
            (let* ((level (ranging-loop-level range))
                   (vlist (cons nil var-list))
                   (nbody (factor-vars-rec body fct-table level vlist nil-list)))
                (unless (or (null cur-level) (= level (1- cur-level)))
                    (error "Invalid loop nesting: ~A at level ~A" expr cur-level))
                ;; Factor the loop range args
                (setf (third range)
                    (factor-vars-rec (third range) fct-table cur-level var-list nil-list))
                (setf (fourth range)
                    (factor-vars-rec (fourth range) fct-table cur-level var-list nil-list))
                (if (car vlist)
                    `(loop-range ,range (let* ,(nreverse (car vlist)) ,@nbody))
                    `(loop-range ,range ,@nbody))))
        (`(setf ,target ,val)
            `(setf ,(factor-vars-dumb target fct-table cur-level var-list nil-list)
                 ,(factor-vars-rec val fct-table cur-level var-list nil-list)))
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

(defun unwrap-factored (expr)
    (let ((full-expr (if (symbolp expr) (get expr 'let-clause))))
        (or (cadr full-expr) expr)))

(defun recurse-factored (fun expr &rest args)
    (apply fun (unwrap-factored expr) args))
