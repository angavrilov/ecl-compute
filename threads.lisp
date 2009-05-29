;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defvar *worker-count*    0)
(defvar *worker-threads*  ())
(defvar *worker-mutex*    (mp:make-lock))
(defvar *work-start-cond* (mp:make-condition-variable))
(defvar *work-done-cond*  (mp:make-condition-variable))

(defvar *working-threads* 0)
(defvar *task-id*         0)
(defvar *task-function*   nil)

(defun worker-thread (idx)
    (format t "Worker ~A starting.~%" idx)
    (unwind-protect
        (let ((last-id 0))
            (loop
                (multiple-value-bind
                        (task task-id w-count)
                        (mp:with-lock (*worker-mutex*)
                            (unless (and *task-function*
                                         (/= *task-id* last-id))
                                (mp:condition-variable-wait
                                    *work-start-cond* *worker-mutex*))
                            (when (> idx *worker-count*)
                                (return-from worker-thread))
                            (values *task-function* *task-id* *worker-count*))
                    (when (and task (/= task-id last-id))
                        (setf last-id task-id)
                        (unwind-protect
                            (handler-case
                                (funcall task idx (1+ w-count))
                                (error (err)
                                    (format t "Worker ~A failed:~%  ~A~%" idx err)))
                            (mp:with-lock (*worker-mutex*)
                                (incf *working-threads* -1)
                                (when (<= *working-threads* 0)
                                    (mp:condition-variable-broadcast *work-done-cond*))))))))
        (format t "Worker ~A exited.~%" idx)))

(defun spawn-worker-threads (num)
    (mp:with-lock (*worker-mutex*)
        (do () ((<= *worker-count* num))
            (incf *worker-count* -1)
            (pop *worker-threads*))
        (mp:condition-variable-broadcast *work-start-cond*)
        (do () ((>= *worker-count* num))
            (incf *worker-count* 1)
            (push
                (mp:process-run-function 'worker
                    #'worker-thread *worker-count*)
                *worker-threads*))))

(defun set-compute-thread-count (num)
    (spawn-worker-threads (max (1- num) 0)))

(defun run-work (fun)
    (when (null fun)
        (error "Cannot run a NIL task"))
    (mp:with-lock (*worker-mutex*)
        (when *task-function*
            (error "Task already running"))
        (incf *task-id*)
        (setf *task-function* fun)
        (setf *working-threads* *worker-count*)
        (mp:condition-variable-broadcast *work-start-cond*))
    (unwind-protect
        (funcall fun 0 (1+ *worker-count*))
        (mp:with-lock (*worker-mutex*)
            (when (> *working-threads* 0)
                (mp:condition-variable-wait
                    *work-done-cond* *worker-mutex*))
            (setf *task-function* nil))))

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
