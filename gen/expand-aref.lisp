;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun join* (lv rv)
  (cond
    ((null lv) rv)
    ((null rv) lv)
    (t `(* ,lv ,rv))))

(defun aref-stride-list (name idx-cnt &optional (total-cnt idx-cnt))
  (loop
     with stride = nil
     for i from (1- idx-cnt) downto 1
     collect (setf stride
                   (join* stride `(arr-dim ,name ,i ,total-cnt)))
     into lst
     finally (return
               (if (> idx-cnt 0)
                   (nreverse (cons nil lst))))))

(defun get-summands (expr)
  (match (canonic-expr-unwrap expr)
    (`(+ ,@rest) rest)
    (x (list x))))

(defun sort-summands-by-level (exprs)
  (let* ((ofs-items (pipeline `(+ ,@exprs)
                      make-canonic flatten-exprs get-summands))
         (num-ofs-items (remove-if-not #'numberp ofs-items))
         (var-ofs-items (remove-if #'numberp ofs-items))
         (levels   (sort (remove-duplicates
                          (mapcar #'min-loop-level var-ofs-items))
                         #'level>))
         (ofs-groups (mapcar #'(lambda (lvl)
                                 (pipeline
                                     `(+ ,@(remove lvl var-ofs-items
                                                   :test-not #'eql
                                                   :key #'min-loop-level))
                                   make-canonic treeify canonic-expr-unwrap))
                             levels)))
    (nconc ofs-groups num-ofs-items)))

(def-rewrite-pass eval-temporary-dims ()
  (`(arr-ptr (temporary ,@_))                (second expr))
  (`(arr-dim (temporary ,_ ,dims ,@_) ,i ,_) (nth i dims)))

(def-rewrite-pass expand-aref (:canonic t)
  (`(aref ,name ,@idxvals)
    (let* ((idx-cnt    (length idxvals))
           (stride-lst (aref-stride-list name idx-cnt))
           (ofs-lst    (mapcar #'join* idxvals stride-lst))
           (summands   (sort-summands-by-level ofs-lst)))
      `(ptr-deref ,(reduce #'(lambda (base ofs)
                               `(ptr+ ,base ,ofs))
                           summands
                           :initial-value `(arr-ptr ,name)))))
  (`(tmp-ref ,name)
    nil)
  (`(tmp-ref ,name ,@idxvals)
    (let ((rexpr (expand-aref-1 `(aref ,name ,@idxvals) old-expr)))
      (simplify-index (eval-temporary-dims rexpr)))))
