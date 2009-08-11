;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

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
        collect
            (setf stride
                (join* stride `(arr-dim ,name ,i ,total-cnt)))
        into lst
        finally
            (return
                (if (> idx-cnt 0)
                    (nreverse (cons nil lst))))))

(defun get-summands (expr)
    (match (canonic-expr-unwrap expr)
        (`(+ ,@rest) rest)
        (x (list x))))

(defun sort-summands-by-level (exprs)
    (let* ((ofs-items (get-summands
                       (flatten-exprs (make-canonic `(+ ,@exprs)))))
           (num-ofs-items (remove-if-not #'numberp ofs-items))
           (var-ofs-items (remove-if #'numberp ofs-items))
           (levels   (sort
                         (remove-duplicates
                             (mapcar #'min-loop-level var-ofs-items))
                         #'level>))
           (ofs-groups (mapcar
                           #'(lambda (lvl)
                                 (treeify
                                     `(+ ,@(remove lvl var-ofs-items
                                               :test-not #'eql :key #'min-loop-level))))
                            levels)))
        (nconc ofs-groups num-ofs-items)))

(def-rewrite-pass eval-temporary-dims ()
  (`(arr-ptr (temporary ,@_))                (second expr))
  (`(arr-dim (temporary ,_ ,dims ,@_) ,i ,_) (nth i dims)))

(defun expand-aref-1 (expr old-expr)
    (match expr
        (`(aref ,name ,@idxvals)
            (let* ((idx-cnt    (length idxvals))
                   (stride-lst (aref-stride-list name idx-cnt))
                   (ofs-lst    (mapcar #'join* idxvals stride-lst))
                   (summands   (sort-summands-by-level ofs-lst)))
                `(ptr-deref
                     ,(reduce #'(lambda (base ofs) `(ptr+ ,base ,ofs))
                          summands
                          :initial-value `(arr-ptr ,name)))))
        (`(tmp-ref ,name)
            nil)
        (`(tmp-ref ,name ,@idxvals)
            (let ((rexpr (expand-aref-1 `(aref ,name ,@idxvals) old-expr)))
                (simplify-index (eval-temporary-dims rexpr))))
        (_ nil)))

(defun expand-aref (expr)
    (simplify-rec-once 
        (cached-simplifier expand-aref-1 
            `(,(or 'aref 'tmp-ref) ,@_)
            (make-hash-table :test #'equal))
        expr))
