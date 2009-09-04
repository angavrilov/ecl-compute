;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun do-make-lisp-compute (original name idxspec expr
                             &key with where carrying parallel precompute cuda-flags)
  (let* ((*current-compute* original)
         (*simplify-cache* (make-hash-table))
         (*range-cache* (make-hash-table))
         (*minlevel-cache* (make-hash-table))
         (*canonify-cache* (make-canonify-cache))
         (*consistency-checks* (make-hash-table :test #'equal)))
    (multiple-value-bind (loop-expr loop-list range-list)
        (make-compute-loops name idxspec expr with where carrying precompute)
      ;; Apply optimizations
      (let* ((noiref-expr (pipeline loop-expr
                            expand-let make-canonic
                            simplify-iref))
             ;; A table of all array references
             (ref-list    (collect-arefs noiref-expr))
             ;; Apply final transformations
             (res-expr    (pipeline noiref-expr
                            canonic-expr-unwrap
                            insert-checks code-motion
                            annotate-types)))
        ;; Generate the computation code
        (wrap-compute-sync-data :host ref-list
          (wrap-compute-parallel parallel range-list
                                 `(let ((*current-compute* ',original)
                                        (*current-compute-body* ',res-expr))
                                    (declare (optimize (safety 1) (debug 1)))
                                    ,res-expr)))))))

(defmacro calc (exprs)
  (pipeline `(letv ,exprs)
    macroexpand-1 expand-let make-canonic
    simplify-iref
    canonic-expr-unwrap
    code-motion annotate-types))

