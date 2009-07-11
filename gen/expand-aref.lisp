;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun expand-aref-1 (expr old-expr)
    (match expr
        (`(aref ,name ,@idxvals)
            (let* ((idx-cnt    (length idxvals))
                   (stride     nil)
                   (stride-lst
                       (loop for i from (1- idx-cnt) downto 0
                        collect (prog1 stride
                                   (let ((cstride `(arr-dim ,name ,i ,idx-cnt)))
                                       (setf stride
                                           (if stride
                                               `(* ,stride ,cstride)
                                               cstride))))))
                   (ofs-lst (mapcar #'(lambda (idx istride)
                                          (if istride `(* ,idx ,istride) idx))
                                idxvals (nreverse stride-lst)))
                   (ofs-expr (simplify-rec-once #'flatten-exprs-1 `(+ ,@ofs-lst)))
                   (ofs-items (match ofs-expr (`(+ ,@rest) rest) (x (list x))))
                   (num-ofs-items (remove-if-not #'numberp ofs-items))
                   (var-ofs-items (remove-if #'numberp ofs-items))
                   (levels   (sort
                                 (remove-duplicates
                                     (mapcar #'min-loop-level var-ofs-items))
                                 #'level>))
                   (ofs-groups (mapcar
                                   #'(lambda (lvl)
                                         (simplify-rec-once #'treeify-1
                                             `(+ ,@(remove lvl var-ofs-items
                                                       :test-not #'eql :key #'min-loop-level))))
                                    levels)))
                `(ptr-deref
                     ,(reduce #'(lambda (base ofs) `(ptr+ ,base ,ofs))
                          (nconc ofs-groups num-ofs-items)
                          :initial-value `(arr-ptr ,name)))))
        (`(tmp-ref ,name)
            nil)
        (`(tmp-ref ,name ,@idxvals)
            (let ((rexpr (expand-aref-1 `(aref ,name ,@idxvals) old-expr)))
                (simplify-index
                    (simplify-rec-once
                        #'(lambda (expr old-expr)
                              (match expr
                                  (`(arr-ptr (temporary ,@_)) (second expr))
                                  (`(arr-dim (temporary ,_ ,dims ,@_) ,i ,_)
                                      (nth i dims))))
                        rexpr))))
        (_ nil)))

(defun expand-aref (expr)
    (simplify-rec-once #'expand-aref-1 expr))
