;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *loop-cluster-size* 1024)
(defparameter *align-cluster* nil)

(defun cluster-loop (range-list range size &key align)
    (let* ((pos     (position range range-list))
           (index   (second range))
           (minv    (third range))
           (maxv    (fourth range))
           (delta   (fifth range))
           (clidx   (get-new-symbol :stem index))
           (new-minv (if align
                         (if (< delta 0) minv
                             `(* (floor ,minv ,align) ,align))
                         minv))
           (new-maxv (if align
                         (if (> delta 0) maxv
                             `(- (* (ceiling (+ ,maxv 1) ,align) ,align) 1))
                         maxv))
           (nrange `(ranging ,clidx
                        ,(simplify-index new-minv)
                        ,(simplify-index new-maxv)
                        ,(* delta size)
                        ,(ranging-order-flag range) nil)))
        (setf (get clidx 'band-master) index)
        (setf (get clidx 'is-cluster) t)
        (setf (third range)
            (simplify-index
                (if (> delta 0)
                    (if align
                        `(max ,minv ,nrange)
                        nrange)
                    `(max ,minv
                         ,(optimize-tree
                              `(- ,nrange ,(* (abs delta)
                                              (1- size))))))))
        (setf (fourth range)
            (simplify-index
                (if (< delta 0)
                    (if align
                        `(min ,maxv ,nrange)
                        nrange)
                    `(min ,maxv (+ ,nrange ,(* (abs delta)
                                               (1- size)))))))
        (values
            (nconc
                (subseq range-list 0 pos)
                (list nrange)
                (subseq range-list pos))
            nrange)))

(defun get-range-value (minv maxv)
    (match (cons (optimize-tree (unwrap-factored minv))
                 (optimize-tree (unwrap-factored maxv)))
        ((when (equal x y)
            `(,x
                 . (min ,maxv (+ ,(type number d) ,y))))
            (values d x nil maxv))
        ((when (equal x y)
            `((max ,minv ,x)
                 . (min ,maxv (+ ,(type number d) ,y))))
            (values d x minv maxv))
        ((when (equal x y)
            `((max ,minv ,(as bv `(+ ,(type number d) ,y)))
                 . ,x))
            (values (- d) bv minv nil))
        ((when (equal x y)
            `((max ,minv ,(as bv `(+ ,(type number d) ,y)))
                 . (min ,maxv ,x)))
            (values (- d) bv minv maxv))
        (`(,(type number a) ,(type number b))
            (values (- b a) a nil nil))
        (x
            (format t "???: ~A" x)
            nil)))

(defun get-range-cluster-base (range)
    (multiple-value-bind
            (dim base)
            (get-range-value (third range) (fourth range))
        (or base (third range))))

(defun make-cluster-refs (range-list vars replace-tbl with
                             &key force-cluster)
    (if (and (null vars)
             (not force-cluster))
        (values range-list with nil nil)
        (let* ((index (car (last range-list)))
               (index-var (second index))
               ;; Cluster the loop (alters ranges)
               (range-list
                   (cluster-loop range-list index *loop-cluster-size*
                       :align *align-cluster*))
               ;; Build a let map fragment
               (cache-index (copy-list index))
               (cluster-base (get-range-cluster-base index))
               (symtbl
                   (mapcar
                       #'(lambda (name)
                             `(,(get-new-symbol :stem name)
                                  (tmp-ref
                                      (temporary ',name
                                          (,*loop-cluster-size*) 0)
                                      (- ,index ,cluster-base))))
                       vars))
               (reftbl
                   (mapcar
                       #'(lambda (name symdef)
                             (list name (first symdef)))
                       vars symtbl))
               ;; Build a loop to compute values
               (calc-loop
                   `(loop-range ,cache-index
                       ,(replace-unquoted
                            (wrap-with-let with
                                `(progn
                                     ,@(mapcar
                                           #'(lambda (name symdef)
                                                 `(setf ,(first symdef) ,name))
                                           vars symtbl)))
                            (subst-save-old cache-index index replace-tbl))))
               ;; Build a with map with vars replaced with temp refs
               (in-with (append
                            reftbl
                            (remove-if #'(lambda (x) (find x vars))
                                with :key #'first))))
            (setf (fifth cache-index) (abs (fifth cache-index))) ; delta
            (setf (sixth cache-index) nil) ;order
            (setf (seventh cache-index) 0) ;level
            (values range-list
                (if symtbl in-with with)
                (if symtbl calc-loop)
                symtbl))))
