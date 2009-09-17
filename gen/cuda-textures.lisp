;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun join+ (lv rv)
  (cond
    ((null lv) rv)
    ((null rv) lv)
    (t `(+ ,lv ,rv))))

(defcontext cuda-texture-transform (cv-mval-set)
  (deflex texture-refs (make-hash-table :test #'equal))

  (defun register-ref (name dim &optional id)
    (let ((rid (or id
                   (format nil "tex_~A_R~A" name dim))))
      (setf (gethash `(:float ,dim ,rid
                              (multivalue-cuda-buffer ,name))
                     texture-refs) t)
      rid))

  (def-rewrite-pass expand-rec (:canonic t)
    ((when (@ cv-mval-set name)
       `(aref (multivalue-data ,name ,@_) ,@idxvals))
      (let* ((idx-cnt    (length idxvals))
             (stride-lst (aref-stride-list (second expr) (1- idx-cnt) idx-cnt))
             (ofs-lst    (mapcar #'join* idxvals stride-lst))
             (summands   (sort-summands-by-level (cons 0.1 ofs-lst))))
        (if (> idx-cnt 1)
            `(texture-ref ',name
                          ,(register-ref name 2)
                          ,(reduce #'join+ summands)
                          ,(reduce #'join+
                                   (sort-summands-by-level
                                    (list 0.1 (car (last idxvals))))))
            `(texture-ref-int ',name
                              ,(register-ref name 1)
                              ,(car (last idxvals))))))))

(defun use-textures (tex-set expr)
  (with-context (cuda-texture-transform tex-set)
    (values (if (empty? tex-set)
                expr
                (expand-rec expr))
            (hash-table-keys texture-refs))))
