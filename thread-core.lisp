;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

#+linux (progn
    (ffi:clines "#include <unistd.h>")

    (defun get-cpu-count ()
        (max 1
            (ffi:c-inline () () :int
                "sysconf(_SC_NPROCESSORS_ONLN)"
                :one-liner t)))

    (ffi:clines "#include <sched.h>")
    (ffi:clines "#include <errno.h>")
    (ffi:clines "#include <string.h>")

    (defun set-cpu-affinity (cpus)
        (let ((cpu-list (if (listp cpus) cpus (list cpus)))
              (numcpus (get-cpu-count)))
            (unless cpus
                (error "Cannot set affinity to an empty list"))
            (dolist (item cpu-list)
                (unless (and (integerp item) 
                             (<= 0 item)
                             (< item numcpus))
                    (error "Invalid CPU index in affinity: ~A" item)))
            (ffi:c-inline (cpu-list) (:object) :void
                "cpu_set_t cset;
                 CPU_ZERO(&cset);
                 cl_object lst = #0;
                 while (CONSP(lst)) {
                   CPU_SET(fixnnint(ECL_CONS_CAR(lst)), &cset);
                   lst = ECL_CONS_CDR(lst);
                 }
                 if (sched_setaffinity(0,sizeof(cset),&cset) < 0)
                   FEerror(\"Affinity setting failed: ~A\", 1,
                           make_base_string_copy(strerror(errno)));
                ")))
)

#-linux (progn
    (defun get-cpu-count () 1)
    (defun set-cpu-affinity (cpus) (declare (ignore cpus)) nil)
)

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

(defun set-compute-thread-count (num-threads &key (adjust-affinity t))
    (let ((num-threads (or num-threads
                           (ignore-errors (parse-integer (si:getenv "OMP_NUM_THREADS")))
                           (get-cpu-count))))
        (spawn-worker-threads (max (1- num-threads) 0)))
    (when adjust-affinity
        (let ((cpu-count (get-cpu-count)))
            (run-work
                #'(lambda (idx num)
                      (set-cpu-affinity (- cpu-count 1 (mod idx cpu-count))))))))

