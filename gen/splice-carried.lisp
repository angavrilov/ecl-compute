;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun mark-carried-low (expr low-table crefs carried-p)
  (multiple-value-bind (cur-carried found)
      (gethash expr low-table)
    (when (or (not found)
              (and carried-p (not cur-carried)))
      (setf (gethash expr low-table) carried-p)
      (match expr
        (`(setf ,ref ,rhs)
          (let ((new-c (gethash (get-ref-root ref) crefs)))
            (when new-c
              (setf (gethash expr low-table) t))
            (recurse-factored #'mark-carried-low
                              ref low-table crefs (or new-c carried-p))
            (recurse-factored #'mark-carried-low
                              rhs low-table crefs (or new-c carried-p))))
        ((type list e)
          (dolist (sub e)
            (recurse-factored #'mark-carried-low
                              sub low-table crefs carried-p)))))))

(defun mark-carried-high (expr high-table crefs)
  (use-cache (expr high-table)
    (match expr
      ((type symbol e)
        (gethash (get-ref-root e) crefs))
      ((or `(tmp-ref ,e)
           `(ptr-deref ,e))
        (or (recurse-factored #'mark-carried-high
                              e high-table crefs)
            (gethash (get-ref-root expr) crefs)))
      ((type list e)
        (some #'identity
              ;; Force full evaluation
              (mapcar #'(lambda (sub)
                          (recurse-factored #'mark-carried-high
                                            sub high-table crefs))
                      expr))))))

(defun splice-carried (expr types low-table high-table)
  (labels
      ((copy-tags (old new)
         (copy-hash-data new old
                         types low-table high-table)
         new)
       (wrap-let (expr l-esc)
         (let* ((sym (get-new-symbol))
                (clause (list sym expr)))
           (setf (get sym 'let-clause) clause)
           (setf (get sym 'lower-escape) l-esc)
           (copy-hash-data sym expr types)
           (copy-tags expr
                      `(let* (,clause) ,sym))))
       (wrap-item (item cur-high cur-low)
         (let ((expr (unwrap-factored item)))
           (cond
             ((and (not cur-low)
                   (gethash expr low-table))
              (match item
                ((when (get item 'let-clause)
                   (type symbol _))
                  (setf (get item 'lower-escape) t)
                  item)
                ((ranging-spec _)
                  item)
                ((type list _)
                  (wrap-let item t))
                (_
                  item)))
             ((and cur-high
                   (not (gethash expr high-table)))
              (match item
                ((ranging-spec _)
                  item)
                ((type list _)
                  (wrap-let item nil))
                (_
                  item)))
             (t item)))))
    (simplify-rec-once-struct
     #'(lambda (expr old-expr)
         (copy-tags
          old-expr
          (let ((cur-high (gethash old-expr high-table))
                (cur-low (gethash old-expr low-table)))
            (match expr
              (`(let* ,assns ,@code)
                `(let* ,assns ,@(butlast code)
                       ,(wrap-item (car (last code))
                                   cur-high cur-low)))
              (`(progn ,@code)
                `(progn
                   ,@(butlast code)
                   ,(wrap-item (car (last code))
                               cur-high cur-low)))
              (`(setf ,lhs ,rhs)
                `(setf ,lhs
                       ,(wrap-item rhs cur-high cur-low)))
              ((type list _)
                (mapcar-save-old #'(lambda (item)
                                     (wrap-item item cur-high cur-low))
                                 expr))
              (_
                expr)))))
     expr)))

(defun splice-inner-loop (expr types)
  (let* ((lvals (make-hash-table))
         (rvals (make-hash-table))
         (low-table (make-hash-table))
         (high-table (make-hash-table)))
    (collect-refs lvals rvals expr)
    (maphash #'(lambda (r v)
                 (unless (gethash r rvals)
                   (remhash r lvals)))
             lvals)
    (mark-carried-low expr low-table lvals nil)
    (mark-carried-high expr high-table lvals)
    (values (splice-carried expr types low-table high-table)
            low-table high-table)))

(defun flatten-inner-loop-with-carry (expr low-table high-table types)
  (let* ((preamble nil)
         (inner nil)
         (out-vars nil)
         (finalize nil))
    (labels
        ((copy-tags (old new)
           (copy-hash-data new old
                           types low-table high-table)
           new)
         (dispatch-assn (var expr)
           (cond
             ((not (gethash expr high-table))
              (push `(setf-tmp ,var ,expr) preamble))
             ((and
               (get var 'lower-escape)
               (gethash expr low-table))
              (push var out-vars)
              (push `(setf ,var ,expr) inner))
             ((gethash expr low-table)
              (push `(setf-tmp ,var ,expr) inner))
             (t
              (push `(setf-tmp ,var ,expr) finalize))))
         (dispatch (stmt)
           (cond
             ((atom stmt) nil)
             ((not (gethash stmt high-table))
              (push stmt preamble))
             ((gethash stmt low-table)
              (push stmt inner))
             (t
              (push stmt finalize))))
         (unwrap (expr &optional (evals t))
           (copy-tags
            expr
            (match expr
              ((type atom _)
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
                  (dispatch-assn (first assn)
                                 (unwrap (second assn))))
                (dolist (stmt (butlast code))
                  (dispatch (unwrap stmt nil)))
                (unwrap (car (last code)) evals))
              (_
                (mapcar-save-old #'unwrap expr))))))
      (dispatch (unwrap expr nil))
      (values (nreverse preamble)
              (nreverse inner)
              out-vars
              (nreverse finalize)))))

(defun make-flattened-loop (expr types)
  (multiple-value-call
      #'flatten-inner-loop-with-carry
    (splice-inner-loop expr types)
    types))
