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
