;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

;;; Ordinary tree rewrite

(defun rewrite-rec-once (engine expr &key (map-recurse-func #'mapcar-save-old))
  (labels ((recurse (expr)
             (let* ((rec-res (if (atom expr)
                                 expr
                                 (funcall map-recurse-func #'recurse expr)))
                    (subs-res (funcall engine rec-res expr)))
               (or subs-res rec-res))))
    (recurse (canonic-expr-unwrap expr))))

(defun rewrite-rec (engine expr &key memoize-hash (map-recurse-func #'mapcar-save-old))
  (let ((memo (or memoize-hash (make-hash-table))))
    (labels ((recurse (expr)
               (use-cache (expr memo)
                 (repeat-subst expr expr)))
             (repeat-subst (expr old-expr)
               (let* ((rec-res (if (atom expr)
                                   expr
                                   (funcall map-recurse-func #'recurse expr)))
                      (subs-res (funcall engine rec-res old-expr)))
                 (if subs-res
                     (repeat-subst subs-res old-expr)
                     (setf (gethash rec-res memo) rec-res)))))
      (recurse (canonic-expr-unwrap expr)))))

;;; Canonic tree rewrite

(defun canonic-rewrite-rec-once (engine expr &key memoize-hash (map-recurse-func #'mapcar))
  (let ((memo (or memoize-hash (make-hash-table))))
    (labels ((recurse (expr)
               (use-cache (expr memo)
                 (let* ((rec-res (if (atom expr)
                                     expr
                                     (funcall map-recurse-func #'recurse expr)))
                        (subs-res (funcall engine rec-res expr)))
                   (make-canonic (or subs-res rec-res))))))
      (recurse (canonic-expr-force-unwrap expr)))))

(defun canonic-rewrite-rec (engine expr &key memoize-hash (map-recurse-func #'mapcar))
  (let ((memo (or memoize-hash (make-hash-table))))
    (labels ((recurse (expr)
               (use-cache (expr memo)
                 (repeat-subst expr expr)))
             (repeat-subst (expr old-expr)
               (let* ((rec-res (if (atom expr)
                                   expr
                                   (funcall map-recurse-func #'recurse expr)))
                      (subs-res (funcall engine rec-res old-expr))
                      (can-res (make-canonic (or subs-res rec-res)))
                      (can-tree (canonic-expr-unwrap can-res)))
                 (if subs-res
                     (repeat-subst can-tree old-expr)
                     (setf (gethash can-tree memo) can-res)))))
      (recurse (canonic-expr-force-unwrap expr)))))

;;; Tree walker for skipping structure

(defun map-skipping-structure (func expr)
  (match expr
    (`(,(as op (or 'let* 'let)) ,assns ,@code)
      (let ((assns2 (mapcar-save-old #'(lambda (assn)
                                         (list-save-old assn
                                                        (first assn)
                                                        (funcall func (second assn))))
                                     assns)))
        (list*-save-old expr
                        op assns2 (mapcar-save-old func code)))
    (_
     (mapcar-save-old func expr)))))

;;; Rewrite pass declaration

(defun rewrite-pass-step-name (pass-name)
  (intern (format nil "~A-1" pass-name)
          (symbol-package pass-name)))

(defmacro rewrite-once (pass-name expr &rest args)
  "Syntax: (rewrite-once pass-name expr)
Applies the rewrite pass rules to the expression once."
  (with-gensyms (ev)
    `(let ((,ev ,expr))
       (or (,(rewrite-pass-step-name pass-name) ,ev ,ev ,@args) ,ev))))

(defmacro def-rewrite-pass (name params &body patterns)
  "Syntax: (def-rewrite-pass name (flags) &body patterns)
Defines a new expression rewrite pass. The flags specify
the rewrite mode. Valid flags are:
    :canonic t            - The pass works on canonic expressions.
    :multiple-pass t      - Repeat until no rewrite rule applies.
    :unwrap-args nil      - Accept canonic-expr nodes in input.
    :memoize-hash expr    - Explicitly specify a value cache.
    :fallback-to pass     - Include rules from the specified pass.
    :recurse-function #'f - Controls the recursion mode.
                            (Default is equivalent to #'mapcar)
Inside the rules the following expressions can be used:
    EXPR          - The expression being matched.
    OLD-EXPR      - Expression before recursive rewrite.
    (REDO X)      - Recursive call with a new expression.
    (FALLBACK X?) - Explicitly invoke the fallback pass."
  (destructuring-bind (&key (canonic nil) (multiple-pass nil)
                            (unwrap-args canonic) (memoize-hash nil)
                            (fallback-to nil) (recurse-function nil)
                            (rewrite-engine (cond
                                              ((and canonic multiple-pass)
                                               'canonic-rewrite-rec)
                                              (canonic 'canonic-rewrite-rec-once)
                                              (multiple-pass 'rewrite-rec)
                                              (t 'rewrite-rec-once))))
      params
    (let* ((step-name     (rewrite-pass-step-name name))
           (expr-var      (intern "EXPR"))
           (old-expr-var  (intern "OLD-EXPR"))
           (redo-name     (intern "REDO"))
           (fallback-name (intern "FALLBACK"))
           (unwrap-expr   (if unwrap-args
                              `(canonic-unwrap-all ,expr-var)
                              expr-var))
           (engine-args   (cond-list (memoize-hash
                                      :memoize-hash memoize-hash)
                                     (recurse-function
                                      :map-recurse-func recurse-function))))
      `(progn
         (defun ,step-name (,expr-var ,old-expr-var)
           (declare (ignorable ,old-expr-var))
           (macrolet ((,redo-name (ev)
                        `(,',step-name ,ev ,',old-expr-var))
                      (,fallback-name (&optional ev)
                        ,(if fallback-to
                             ``(,',(rewrite-pass-step-name fallback-to)
                                  ,(or ev ',expr-var))
                             `(declare (ignore ev)))))
             (match ,unwrap-expr
               ,@patterns
               ,@(if (and fallback-to
                          (not (member '_ patterns :key #'first)))
                     (list `(_ (,fallback-name)))))))
         (defun ,name (expr)
           (,rewrite-engine #',step-name expr ,@engine-args))))))

