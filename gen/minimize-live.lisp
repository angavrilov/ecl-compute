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
    expr rank
    lhs-var rhs-vars rhs-var-count
    lhs-refs rhs-refs)

(defmethod compare ((p1 assn-info) (p2 assn-info))
    (let ((cv (compare-slots p1 p2 #'assn-info-rank)))
        (if (and (eql cv :equal) (not (eql p1 p2)))
            :unequal
            cv)))

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

(defun set-assn-ranks (info-list &optional (idx 0))
    (dolist (info info-list)
        (setf (assn-info-rank info) idx)
        (incf idx)))

(defcontext stmt-reordering (info-list #'get-score)
    (deflex fwd-tbl (make-hash-table))
    (deflex cnt-tbl (make-hash-table))
    (deflex remaining 0)

    (deflex queue
        (let ((var-tbl (make-hash-table)) ; var->info
              (rhs-tbl (make-hash-table)) ; ref->info*
              (queue (empty-set)))
            (set-assn-ranks info-list)
            (dolist (info info-list)
                (let ((deps (reduce #'union
                                (list*
                                    (mapcar #f(gethash _ var-tbl)
                                        (assn-info-rhs-vars info))
                                    (mapcar #f(gethash _ rhs-tbl)
                                        (assn-info-lhs-refs info))))))
                    (dolist (info2 deps)
                        (push info (gethash info2 fwd-tbl)))
                    (if (null deps)
                        (adjoinf queue
                            (cons (- (get-score info)) info))
                        (progn
                            (setf (gethash info cnt-tbl) (length deps))
                            (incf remaining))))
                (let ((lvar (assn-info-lhs-var info)))
                    (when lvar
                        (setf (gethash lvar var-tbl) info)))
                (dolist (rhs (assn-info-rhs-refs info))
                    (push info (gethash rhs rhs-tbl))))
            queue))

    (deflex schedule nil)

    (defun queued-score (qi)
        (car qi))

    (defun queued-info (qi)
        (cdr qi))

    (defun queue-item (info)
        (adjoinf queue
            (cons (- (get-score info)) info)))

    (defun schedule-item (info)
        (push info schedule)
        (dolist (info2 (gethash info fwd-tbl))
            (let ((cnt (decf (gethash info2 cnt-tbl))))
                (when (<= cnt 0)
                    (remhash info2 cnt-tbl)
                    (decf remaining)
                    (queue-item info2)))))

    (defun pop-queue-info ()
        (let ((val (least queue)))
            (setf queue (less queue val))
            (queued-info val)))

    (defun result-schedule ()
        (assert (= 0 remaining))
        (nreverse schedule)))

(defun shuffle-stmts (info-list)
    (with-context
            (stmt-reordering info-list
                #'assn-info-rhs-var-count)
        (until (empty? queue)
            (schedule-item (pop-queue-info)))
        (result-schedule)))

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
                       stmt-list))))
        ;(print-live-stats shuffled)
        ;(push shuffled *shuffled*)
        (mapcar #'assn-info-expr shuffled)))
