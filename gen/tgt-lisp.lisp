;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun do-make-lisp-compute (original name idxspec expr
                                &key with where carrying parallel cluster-cache
                                    cuda-block-size)
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*consistency-checks* (make-hash-table :test #'equal)))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with where carrying cluster-cache)
            (let* ((nolet-expr (expand-let loop-expr))
                   (noiref-expr (simplify-iref nolet-expr))
                   (ref-list    (collect-arefs noiref-expr))
                   (check-expr  (insert-checks noiref-expr))
                   (motion-expr (code-motion check-expr))
                   (annot-expr  (annotate-types motion-expr)))
                (wrap-compute-sync-data :host ref-list
                    (wrap-compute-parallel parallel range-list
                        `(let ((*current-compute* ',original)
                               (*current-compute-body* ',motion-expr))
                            (declare (optimize (safety 1) (debug 1)))
                            ,annot-expr)))))))

(defmacro calc (exprs)
    (annotate-types (code-motion (simplify-iref (expand-let (macroexpand-1 `(letv ,exprs)))))))

