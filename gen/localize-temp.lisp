;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun localize-temps (expr ref-list range-list)
    (let* ((inner-index
               (second (car (last range-list))))
           (cluster-level
             (convert 'set
               (mapcar #'ranging-loop-level
                   (remove-if-not
                       #'(lambda (rg)
                             (and
                                 (eql inner-index
                                     (get (second rg) 'band-master))
                                 (get (second rg) 'is-cluster)))
                       range-list))))
           (local-temps
              (remove-if-not
                  #'(lambda (ref-entry)
                        (ifmatch
                                `(temporary ,_ ,dims 0 ,@_)
                                (first ref-entry)
                            (and
                                (= (length dims) 1)
                                (null
                                    (set-difference
                                        (third ref-entry) (second ref-entry)
                                        :test #'equal))
                                (every
                                    #'(lambda (x)
                                          (and
                                              (= (length x) 1)
                                              (equal?
                                                  (set-difference
                                                      (get-loop-levels (first x))
                                                      cluster-level)
                                                  (set 0))))
                                    (append
                                        (third ref-entry) (second ref-entry))))))
                  ref-list))
           (replacement-tbl
               (make-hash-table :test #'equal)))
        (if (null local-temps)
            expr
            (progn
                (dolist (ref-entry local-temps)
                    (setf
                        (gethash
                            (first ref-entry) replacement-tbl)
                        `(temporary
                             ,(second (first ref-entry))
                             nil 0 :local)))
                (simplify-rec-once
                    #'(lambda (form old-form)
                          (match form
                              ((when (gethash temp replacement-tbl)
                                  `(tmp-ref ,temp ,@_))
                                  `(tmp-ref ,(gethash temp replacement-tbl)))))
                    expr)))))
