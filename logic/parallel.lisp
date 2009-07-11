;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *use-thread-dispatch* nil)

(defun wrap-parallel (range code &key (gen-func #'identity) (wrap-func #'identity))
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
            (labels
                ((wrap-range (idx-sym cnt-sym aux-defs disp-ofs disp-sz body)
                      `(let* ((,rng-sym
                                  ,(simplify-index
                                       `(ceiling
                                               (+ (- ,maxv ,minv) ,stepv)
                                               (* ,cnt-sym ,stepv))))
                              ,@aux-defs
                              (,min-sym
                                  ,(simplify-index
                                       `(+ ,minv
                                            (* (+ (* ,rng-sym ,idx-sym) ,disp-ofs) ,stepv))))
                              (,max-sym (min ,maxv
                                            (- (+ ,min-sym (* ,disp-sz ,stepv)) ,stepv))))
                           (when (>= ,max-sym ,min-sym) ,body))))
                (if (or (not *use-thread-dispatch*)
                        (eql (ranging-loop-level range) 0))
                    `(run-work
                         #'(lambda (,idx-sym ,cnt-sym)
                               ,(wrap-range idx-sym cnt-sym nil 0 rng-sym
                                    (funcall wrap-func (funcall gen-func code)))))
                    (let ((thread-idx-sym (gensym))
                          (sub-idx-sym (gensym))
                          (disp-idx-sym (gensym))
                          (dispatch-cnt-sym (gensym)))
                        `(run-work
                             #'(lambda (,thread-idx-sym ,cnt-sym)
                                   ,(funcall wrap-func
                                       `(thread-dispatch ,thread-idx-sym
                                          #'(lambda (,idx-sym ,dispatch-cnt-sym)
                                                (declare (ignore ,dispatch-cnt-sym))
                                                ,(wrap-range 
                                                    `(mod ,idx-sym ,cnt-sym)
                                                    cnt-sym
                                                    `((,sub-idx-sym (floor ,idx-sym ,cnt-sym))
                                                      (,disp-idx-sym
                                                         (loop for i from 1 to ,sub-idx-sym 
                                                          sum (ceiling ,rng-sym (ash 1 i)))))
                                                    disp-idx-sym
                                                    `(min (- ,rng-sym ,disp-idx-sym)
                                                          (ceiling ,rng-sym (ash 2 ,sub-idx-sym)))
                                                    (funcall gen-func code))))))
                            :dispatch-limit
                                (ceiling (log (max 1 ,(simplify-index 
                                                        `(ceiling (+ (- ,maxv ,minv) ,stepv) ,stepv))))
                                         (log 2)))))))))

(defmacro parallel-loop ((name idxlist &key private-mv)
                            &body code)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (multiple-value-bind
                (code loops ranges) (wrap-idxloops name indexes idxlist code)
            (let ((loops1 (remove-if #'ranging-order-flag ranges)))
                (when (null loops1)
                    (error "Cannot find a parallelizable loop: ~A" ranges))
                (wrap-parallel (car loops1) code
                    :wrap-func 
                        #'(lambda (body)
                            (if private-mv
                               `(with-local-multivalues
                                    ,private-mv
                                    ,body)
                                body)))))))

