;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

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

(defun wrap-compute-sync-data (sync-code ref-list body)
    (multivalue-wrap-sync
        (cons sync-code
            (mapcan
               #'(lambda (spec)
                     (match spec
                         (`((multivalue-data ,mv ,@_) ,rv ,wv ,@_)
                             (list
                                 (cond
                                     ((and rv wv) :read-write)
                                     (rv :read)
                                     (wv :write))
                                 mv))))
               ref-list))
         body))

(defun wrap-compute-parallel (parallel loop-list code &optional (gen-func #'identity))
    (if (null parallel)
        (funcall gen-func code)
        (let ((range (if (eql parallel t)
                         (let ((loops1 (remove-if #'ranging-order-flag loop-list)))
                             (when (null loops1)
                                 (error "Cannot find a parallelizable loop: ~A" loop-list))
                             (car loops1))
                         (let ((range (find parallel loop-list :key #'second)))
                             (when (null range)
                                 (error "Cannot find loop ~A in: ~A" parallel loop-list))
                             (when (ranging-order-flag range)
                                 (error "Loop ~A not parallelizable: ~A" parallel loop-list))
                             range))))
            (wrap-parallel range code :gen-func gen-func))))

(defun make-compute-loops (name idxspec expr with-arg where-arg
                              carrying cluster-cache
                              &key force-cluster)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (with      (append
                              (convert-letv-exprs-auto where-arg)
                              (convert-letv-exprs-auto with-arg))))
            (multiple-value-bind
                    (range-list replace-tbl)
                    (build-loop-list name indexes idxlist)
                ;; Don't cluster unless the last loop is ordered
                (unless (and cluster-cache range-list
                        (ranging-order-flag (car (last range-list))))
                    (setf cluster-cache nil))
                ;; Apply clustering (alters ranges)
                (multiple-value-bind
                        (range-list in-with cluster-loop cluster-syms)
                        (make-cluster-refs
                            range-list cluster-cache replace-tbl with
                            :force-cluster force-cluster)
                    ;; Set the level tags
                    (correct-loop-levels range-list 0)
                    ;; Create actual loops
                    (multiple-value-bind
                            (loop-expr loop-list)
                            (do-wrap-loops
                                (list
                                    (wrap-with-let in-with
                                        `(setf (iref ,name ,@idxvars) ,expr)))
                                range-list replace-tbl)
                        ;; Apply carry
                        (let* ((carry-expr
                                   (make-compute-carry
                                       carrying loop-expr loop-list
                                       range-list with in-with replace-tbl))
                               (loop-cnt (length loop-list))
                               (pre-last-loop (if (> loop-cnt 1)
                                                  (nth (- loop-cnt 2) loop-list))))
                            ;; Insert the cluster precalculation loop
                            (when cluster-loop
                                (if pre-last-loop
                                    (prepend-loop-item pre-last-loop cluster-loop)
                                    (setf carry-expr `(progn ,cluster-loop ,carry-expr))))
                            (values
                                (wrap-symbol-macrolet cluster-syms carry-expr)
                                loop-list range-list))))))))
