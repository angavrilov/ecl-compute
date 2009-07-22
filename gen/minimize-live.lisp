;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(use-std-readtable)

(defun flatten-inner-loop (expr types &key split-all)
    (let* ((assns nil))
        (labels
            ((copy-tags (old new)
                 (copy-hash-data new old types)
                 new)
             (dispatch (stmt)
                 (when stmt
                     (push stmt assns)))
             (new-temp (expr)
                 (if (atom expr)
                     expr
                     (let* ((sym (gensym))
                            (clause (list sym expr)))
                         (setf (get sym 'let-clause) clause)
                         (copy-hash-data sym expr types)
                         (dispatch `(setf-tmp ,sym ,expr))
                         sym)))
             (unwrap-split (expr)
                 (new-temp (unwrap expr)))
             (unwrap (expr &optional (evals t))
                 (copy-tags expr
                     (match expr
                         ((type atom _)
                             expr)
                         (`(ranging ,@_)
                             expr)
                         (`(progn ,@code)
                             (dolist (stmt (butlast code))
                                 (dispatch (unwrap stmt nil)))
                             (unwrap (car (last code)) evals))
                         ((when (not evals)
                             `(let* ((,x ,v)) ,x))
                             (unwrap v nil))
                         (`(let* ,assns ,@code)
                             (dolist (assn assns)
                                 (dispatch
                                     `(setf-tmp
                                          ,(first assn)
                                          ,(unwrap (second assn)))))
                             (dolist (stmt (butlast code))
                                 (dispatch (unwrap stmt nil)))
                             (unwrap (car (last code)) evals))
                         ((when evals
                             `(setf ,var ,rhs))
                             (let ((sym (new-temp (unwrap rhs))))
                                 (dispatch `(setf ,var ,sym))
                                 sym))
                         ((when split-all
                             `(setf ,(type list tgt) ,rhs))
                             `(setf ,(mapcar #'unwrap-split tgt)
                                    ,(unwrap rhs)))
                         ((when split-all _)
                             (mapcar-save-old #'unwrap-split expr))
                         (_
                             (mapcar-save-old #'unwrap expr))))))
            (dispatch (unwrap expr nil))
            (nreverse assns))))

(defun get-temp-vars (stmt-list)
    (mapcan
        #'(lambda (stmt)
              (ifmatch
                  `(setf-tmp ,var ,rhs) stmt
                  (list var)))
        stmt-list))

(defstruct assn-info
    expr
    lhs-var rhs-vars rhs-var-count
    lhs-refs rhs-refs)

(defun get-expr-ref-info (expr temp-set)
    (let* ((lrefs (make-hash-table))
           (rrefs (make-hash-table)))
        (collect-refs lrefs rrefs expr)
        (multiple-value-bind
                (lvar rhsvars)
                (match expr
                    (`(setf-tmp ,var ,rhs)
                        (values var
                            (hash-set-intersect
                                (get-symbol-set rhs)
                                    temp-set)))
                    (_
                        (values nil
                            (hash-set-intersect
                                (get-symbol-set expr)
                                temp-set))))
            (let ((rhs (hash-table-keys rhsvars)))
                (make-assn-info
                    :expr expr
                    :lhs-var lvar
                    :rhs-vars rhs :rhs-var-count (length rhs)
                    :lhs-refs (hash-table-keys lrefs)
                    :rhs-refs (hash-table-keys rrefs))))))

(defun find-best-candidate (info-list temp-set
                               &optional
                                   (block-set (make-hash-table))
                                   best-tmp setf-list)
    (if (null info-list)
        (nreverse
            (cond-list*
                (best-tmp best-tmp)
                setf-list))
        (let* ((head-info (first info-list))
               (tail      (rest info-list))
               (svar      (assn-info-lhs-var head-info))
               (lrefs     (assn-info-lhs-refs head-info))
               (can-select
                   (and (every #f(gethash _ temp-set)
                            (assn-info-rhs-vars head-info))
                        (not
                            (some #f(gethash _ block-set)
                                (assn-info-lhs-refs head-info))))))
            (sethash-all block-set
                (assn-info-rhs-refs head-info))
            (cond
                ((not can-select)
                    (find-best-candidate
                        tail temp-set block-set
                        best-tmp setf-list))
                ((null svar)
                    (find-best-candidate
                        tail temp-set block-set
                        best-tmp
                        (cons head-info setf-list)))
                ((or (null best-tmp)
                     (>  (assn-info-rhs-var-count head-info)
                         (assn-info-rhs-var-count best-tmp)))
                    (find-best-candidate
                        tail temp-set block-set
                        head-info setf-list))
                (t
                    (find-best-candidate
                        tail temp-set block-set
                        best-tmp setf-list))))))

(defun shuffle-stmts (info-list temp-set)
    (if (null info-list) nil
        (let* ((head-info (first info-list))
               (tail      (rest info-list))
               (svar      (assn-info-lhs-var head-info)))
            (cons
                head-info
                (if (null svar)
                    (shuffle-stmts tail temp-set)
                    (progn
                        (setf (gethash svar temp-set) t)
                        (let* ((picked
                                   (find-best-candidate tail temp-set))
                               (remained
                                   (remove-if #f(find _ picked) tail)))
                            (shuffle-stmts
                                (nconc picked remained) temp-set))))))))

(defun print-live-stats (info-list)
    (let ((tbl (make-hash-table))
          (die (make-hash-table))
          (live (empty-set))
          (idx 0))
        (dolist (info info-list)
            (when (assn-info-lhs-var info)
                (setf (gethash (assn-info-lhs-var info) tbl)
                    (list idx)))
            (dolist (rhv (assn-info-rhs-vars info))
                (push idx (gethash rhv tbl)))
            (incf idx))
        (do-hashtable (key val tbl)
            (let* ((lastv (car val))
                   (revv (nreverse val)))
                (setf (gethash key tbl) revv)
                (push (car revv) (gethash lastv die))))
        (setf idx 0)
        (dolist (info info-list)
            (if (assn-info-lhs-var info)
                (let* ((key (assn-info-lhs-var info))
                       (val (gethash key tbl))
                       (end (car (last val))))
                    (format t "~A: ~A ~A " key val (- end (first val))))
                (format t "Write ~A " (assn-info-lhs-refs info)))
            (format t " <- ~A~%"
                (sort
                    (mapcar #'(lambda (x) (- idx (first (gethash x tbl))))
                        (assn-info-rhs-vars info))
                    #'<))
            (when (assn-info-lhs-var info)
                (setf live (with live idx)))
            (dolist (k (gethash idx die))
                (setf live (less live k)))
            (format t "///~A: ~A///~%" (size live) live)
            (incf idx))))

(defparameter *shuffled* nil)

(defun minimize-live-vars (stmt-list)
    (let* ((temp-vars    (get-temp-vars stmt-list))
           (temp-var-set (sethash-all (make-hash-table) temp-vars))
           (shuffled
               (shuffle-stmts
                   (mapcar #f(get-expr-ref-info _ temp-var-set)
                       stmt-list)
                   (make-hash-table))))
        (print-live-stats shuffled)
        (push shuffled *shuffled*)
        (mapcar #'assn-info-expr shuffled)))
