;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun get-symbol-set (expr)
  (let ((table (make-hash-table)))
    (labels ((recur (expr)
               (cond
                 ((symbolp expr)
                  (setf (gethash expr table) t))
                 ((consp expr)
                  (mapc #'recur expr)))))
      (recur expr)
      table)))

(defun collect-arefs (tree)
  (let ((read-tbl (make-hash-table :test #'equal))
        (write-tbl (make-hash-table :test #'equal))
        (res-tbl (make-hash-table :test #'equal)))
    (labels ((do-collect (form)
               (match form
                 ((type atom _) nil)
                 (`(setf (,(or 'aref 'tmp-ref) ,@args) ,rexpr)
                   (record args t)
                   (do-collect rexpr))
                 (`(setf ,@_)
                   (error "Non-array stores not allowed: ~A" form))
                 (`(,(or 'aref 'tmp-ref) ,@args)
                   (record args nil)
                   (do-collect (cdr args)))
                 (_
                   (dolist (item form)
                     (do-collect item)))))
             (record (args written)
               (let* ((obj (first args))
                      (indexes (cdr args))
                      (rtab (if written write-tbl read-tbl))
                      (rentry
                       (or (gethash obj res-tbl)
                           (setf (gethash obj res-tbl)
                                 (list nil nil)))))
                 (unless (gethash args rtab)
                   (setf (gethash args rtab) t)
                   (if written
                       (push indexes (cadr rentry))
                       (push indexes (car rentry)))))))
      (do-collect (canonic-expr-unwrap tree))
      (hash-table-alist res-tbl))))

(defun get-ref-root (expr)
  (match expr
    ((type symbol sym)
      sym)
    (`(multivalue-data ,@_)
      expr)
    (`(temporary ,@_)
      expr)
    (`(,(or 'texture-ref 'texture-ref-int) ',name ,@_)
      `(multivalue-data ,name))
    (`(tmp-ref ,tmp)
      (recurse-factored #'get-ref-root tmp))
    (`(ptr-deref ,ptr)
      (recurse-factored #'get-ref-root ptr))
    (`(ptr+ ,ptr ,_)
      (recurse-factored #'get-ref-root ptr))
    (`(arr-ptr ,arr)
      (recurse-factored #'get-ref-root arr))
    (_
      (error "Invalid reference: ~A" expr))))

(def-rewrite-pass trivialize-refs ()
  (`(multivalue-data ,name ,@_)
    name)
  (`(temporary ,name ,@_)
    name)
  (`(,(or 'texture-ref 'texture-ref-int) ',name ,@_)
    name)
  ((ranging-spec iv)
    iv)
  (`(,(or 'tmp-ref 'ptr-deref) ,ptr ,@_)
    (trivialize-refs (get-ref-root old-expr))))

(defun collect-refs (lvals rvals expr)
  (match expr
    (`(setf ,ref ,rhs)
      (setf (gethash (get-ref-root ref) lvals) t)
      (collect-refs lvals rvals rhs))
    ((or (type symbol e)
         `(tmp-ref ,e)
         `(ptr-deref ,e))
      (setf (gethash (get-ref-root expr) rvals) t)
      (when (consp e)
        (collect-refs lvals rvals e)))
    ((type list e)
      (dolist (sub e)
        (collect-refs lvals rvals sub)))))

(defun get-inner-delta-1 (expr)
  (match expr
    ((type number num)
      num)
    (`(,(or 'arr-ptr 'temporary 'arr-dim 'index) ,@_)
      0)
    (`(- ,a ,@rest)
      (- (get-inner-delta a)
         (get-inner-delta `(+ ,@rest))))
    (`(,(or 'ptr+ '+) ,@args)
      (reduce #'+
              (mapcar #'get-inner-delta args)))
    (`(* ,@args)
      (reduce #'*
              (mapcar #'get-inner-delta args)))
    (_ 0)))

(defun get-inner-delta (expr)
  (recurse-factored #'get-inner-delta-1 expr))

(defun deriv* (&rest args)
  (let ((nz-vals (remove-if #'zerop args)))
    (if nz-vals nil 0)))

(defun get-inner-step-1 (expr)
  (match expr
    ((type number num)
      0)
    ((when (eql level 0)
       (ranging-spec _ :loop-level level))
      1)
    (`(,(or 'arr-ptr 'temporary 'arr-dim 'index) ,@_)
      0)
    (`(- ,a ,@rest)
      (apply-unless-nil #'-
                        (list
                         (get-inner-step a)
                         (get-inner-step `(+ ,@rest)))))
    (`(,(or 'ptr+ '+) ,@args)
      (apply-unless-nil #'+
                        (mapcar #'get-inner-step args)))
    (`(* ,@args)
      (apply-unless-nil #'deriv*
                        (mapcar #'get-inner-step args)))))

(defun get-inner-step (expr)
  (recurse-factored #'get-inner-step-1 expr))
