;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun wrap-parallel (range code &optional (gen-func #'identity))
    (destructuring-bind
        (rs arg iminv imaxv stepv &rest tail) range
        (unless (eql rs 'ranging)
            (error "Invalid range: ~A" range))
        (let ((min-sym (gensym))
              (max-sym (gensym))
              (rng-sym (gensym))
              (idx-sym (gensym))
              (cnt-sym (gensym))
              (minv (get-full-expr iminv))
              (maxv (get-full-expr imaxv)))
            (setf (third range) min-sym)
            (setf (fourth range) max-sym)
            (setf (get min-sym 'full-expr) minv)
            (setf (get max-sym 'full-expr) maxv)
            `(run-work
                 #'(lambda (,idx-sym ,cnt-sym)
                       (let* ((,rng-sym
                                  ,(simplify-index
                                       `(* (ceiling
                                               (+ (- ,maxv ,minv) ,stepv)
                                               (* ,cnt-sym ,stepv))
                                           ,stepv)))
                              (,min-sym
                                  ,(simplify-index
                                       `(+ ,minv
                                            (* ,rng-sym ,idx-sym))))
                              (,max-sym (min ,maxv
                                            (- (+ ,min-sym ,rng-sym) ,stepv))))
                           ,(funcall gen-func code)))))))

(defmacro parallel-loop ((name idxlist &key private-mv)
                            &body code)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (multiple-value-bind
                (code loops) (wrap-idxloops name indexes idxlist code)
            (let ((loops1 (remove-if #'ranging-order-flag loops)))
                (when (null loops1)
                    (error "Cannot find a parallelizable loop: ~A" loops))
                (wrap-parallel (car loops1)
                    (if private-mv
                       `(with-local-multivalues
                            ,private-mv
                            ,code)
                        code))))))
