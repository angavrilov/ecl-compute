;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun count-subexprs (expr &key
                       (test #'eql)
                       (table (make-hash-table :test test)))
  (labels ((recurse (expr)
             ;; Increment the counter, and avoid walking trees twice
             (when (> (incf-nil (gethash expr table)) 1)
               (return-from recurse nil))
             (match expr
               ((type atom var)
                 nil)
               (`(index ,@_)
                 nil)
               (`(declare ,@_)
                 nil)
               (`(multivalue-data ,@_)
                 nil)
               (`(loop-range ,(ranging-spec _ :min minv :max maxv) ,@body)
                 (recurse minv)
                 (recurse maxv)
                 (mapc #'recurse body))
               (`(temporary ,_ ,dims ,@_)
                 (mapc #'recurse dims))
               (`(setf ,target ,val)
                 (mapc #'recurse (cdr target))
                 (recurse val))
               (`(safety-check ,checks ,@body)
                 (dolist (check checks)
                   (recurse (first check)))
                 (mapc #'recurse body))
               (_
                 (mapc #'recurse (cdr expr))))))
    (recurse expr)
    table))


;; Determine expressions to factor off

(defun build-factor-table (cnt-table pull-symbols)
  (let ((fct-table (make-hash-table :test #'equal)))
    (maphash #'(lambda (expr cnt)
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
                                       '(index safety-check loop-range setf))))
                   (let ((level (min-loop-level expr)))
                     (dolist (sub (cdr expr))
                       (unless (eql level (min-loop-level sub))
                         (setf (gethash sub fct-table) t))))))
             cnt-table)
    (maphash #'(lambda (expr flag)
                 ;; Don't factor constants and loop vars
                 (when (match expr
                         ((type number _) t)
                         ((type string _) t)
                         ('nil t)
                         (`(_grp ,@_) t)
                         ((type symbol var) (not pull-symbols))
                         ((ranging-spec _ :loop-level level) level)
                         (_ nil))
                   (remhash expr fct-table)))
             fct-table)
    fct-table))


;; Factorization engine

(defcontext factor-vars-engine (fct-table)
  (deflex replace-table (make-hash-table
                         :test (hash-table-test fct-table)))

  ;; Loop level stack
  (deflex loop-stack nil)
  (deflex loop-var-map (make-hash-table))

  (defun current-level ()
    (car loop-stack))

  (defun push-level (level)
    (unless (or (null loop-stack)
                (level< level (current-level)))
      (error "Invalid loop nesting: ~A at level ~A"
             level (current-level)))
    (assert (null (gethash level loop-var-map)))
    (push level loop-stack))

  (defun pop-level (level)
    (assert (eql (pop loop-stack) level))
    (prog1
        (gethash level loop-var-map)
      (remhash level loop-var-map)
      (let ((junk (set-difference
                   (hash-table-keys loop-var-map) loop-stack)))
        (when junk
          (error "Invalid loop nesting:~{ ~A~} in gap at level ~A"
                 junk (current-level))))))

  (defun recurse-level (level exprs)
    (push-level level)
    (values (mapcar-save-old #'factor-rec exprs)
            (pop-level level)))

  ;; Factorization
  (defun factor-dumb (expr)
    (if (or (atom expr) (find (car expr) '(index)))
        expr
        (mapcar-save-old #'factor-rec expr)))

  (defun split-off (expr)
    (let* ((sym       (get-new-symbol))
           (nexpr     (factor-dumb expr))
           (level     (min-loop-level expr))
           (expr-pair (list sym (optimize-tree nexpr))))
      (setf (get sym 'let-clause) expr-pair)
      (setf (get sym 'full-expr) expr)
      (setf (get sym 'loop-level) level)
      (when (level< level (current-level))
        (error "Invalid level ~A at current ~A"
               level (current-level)))
      (push expr-pair (gethash level loop-var-map))
      (setf (gethash expr replace-table) sym)))

  (defun factor-rec (expr)
    (match expr
      (`(declare ,@_) expr)
      (`(quote ,@_) expr)

      (`(loop-range ,range ,@body)
        (let* ((range-info (ranging-info range))
               (level      (range-loop-level range-info)))
          (multiple-value-bind (nbody var-list)
              (recurse-level level body)
            ;; Factor the loop range args
            (setf (range-min range-info)
                  (factor-rec (range-min range-info)))
            (setf (range-max range-info)
                  (factor-rec (range-max range-info)))
            ;; Pop the substitutions
            (dolist (subs var-list)
              (remhash (get (first subs) 'full-expr) replace-table))
            ;; Wrap with let if needed
            (if var-list
                `(loop-range ,range (let* ,(nreverse var-list) ,@nbody))
                `(loop-range ,range ,@nbody)))))

      (`(setf ,target ,val)
        `(setf ,(factor-dumb target) ,(optimize-tree (factor-rec val))))

      (`(safety-check ,checks ,@body)
        (list*-save-old expr
                        'safety-check
                        (mapcar-save-old (lambda (check)
                                           (cons-save-old check
                                                          (factor-rec (car check))
                                                          (cdr check)))
                                         checks)
                        (mapcar-save-old #'factor-rec body)))

      (_
        (or (gethash expr replace-table)
            (if (gethash expr fct-table)
                (split-off expr)
                (factor-dumb expr)))))))

(defun factor-vars (expr fct-table)
  (with-context (factor-vars-engine fct-table)
    (multiple-value-bind (body nil-list)
        (recurse-level nil (list expr))
      (if nil-list
          `(let* ,(nreverse nil-list) ,@body)
          (car body)))))


;; Code motion wrapper

(defun code-motion (expr &key pull-symbols)
  (pipeline expr
    (count-subexprs _ :test #'equal)
    (build-factor-table _ pull-symbols)
    (factor-vars expr _)))
