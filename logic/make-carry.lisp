;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun create-carry (index carrying range-list loop-list with in-with replace-tbl)
    (let* ((pos       (or (position index range-list :key #'second)
                          (error "Invalid carry index: ~A" index)))
           (range     (nth pos range-list))
           ;; Inner dimensions
           (iranges   (nthcdr (1+ pos) range-list))
           (act-ranges (mapcar #'compute-range
                           (remove-if #'(lambda (rg)
                                            (get (second rg) 'is-cluster))
                               iranges)))
           (idims     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(+ (- ,(fourth rg) ,(third rg)) 1)))
                          act-ranges))
           (irefs     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(- ,rg ,(third rg))))
                          act-ranges))
           ;; Modification points
           (out-pos    (position index range-list :key #'range-band-master))
           (out-loop   (nth out-pos loop-list))
           (last-loop  (car (last loop-list)))
           ;; Carry variables
           (carry-list (remove-if-not
                           #'(lambda (ce)
                                 (eql (first ce) index))
                           carrying))
           (init-code (do-wrap-loops
                          (list (wrap-with-let with
                                    (list* 'progn
                                        (mapcar
                                            #'(lambda (iexpr)
                                                  `(setf ,(second iexpr)
                                                         ,(or (fourth iexpr) 0.0)))
                                            carry-list))))
                          iranges replace-tbl))
           (alter-code  (replace-unquoted
                            (wrap-with-let in-with
                                (list* 'progn
                                    (mapcar
                                        #'(lambda (iexpr)
                                              `(setf ,(second iexpr)
                                                     ,(third iexpr)))
                                        carry-list)))
                            replace-tbl))
           (name-table (mapcar
                           #'(lambda (iexpr)
                                 (list (second iexpr)
                                     `(tmp-ref
                                          (temporary ',(second iexpr) ,idims 0)
                                          ,@irefs)))
                           carry-list)))
        (unless (ranging-order-flag range)
            (error "Cannot carry over unordered index ~A" index))
        ;; Splice in the new imperative code
        (prepend-loop-item out-loop init-code)
        (append-loop-item last-loop alter-code)
        ;; Return the new names
        name-table))

(defun make-compute-carry (carrying loop-expr loop-list range-list with in-with replace-tbl)
    (let* ((carry-indices (if carrying
                              (reduce #'nunion
                                  (mapcar #'list
                                      (mapcar #'first
                                          carrying)))))
           (carry-body `(progn nil ,loop-expr))
           (carry-table
               (mapcan
                   #'(lambda (idx)
                         (create-carry
                             idx carrying range-list
                             (cons carry-body loop-list)
                             with in-with replace-tbl))
                   carry-indices)))
        (if (null carry-table)
            loop-expr
            (wrap-symbol-macrolet carry-table carry-body))))
