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

(defun get-lock-spin (lock &key max-tries)
    (if max-tries
        (loop for try-id from 1 to max-tries
         do (when (mp:get-lock lock nil)
                 (return))
         finally (mp:get-lock lock t))
        (loop
            (when (mp:get-lock lock nil)
                (return)))))

(defmacro with-lock-spin ((lock &key max-tries) &body code)
    (let ((lock-sym (gensym)))
        `(let ((,lock-sym ,lock))
            (unwind-protect
                (progn
                    (get-lock-spin ,lock-sym :max-tries ,max-tries)
                    ,@code)
                (mp:giveup-lock ,lock-sym)))))

(defmacro condition-wait-spin ((cond mutex &key (max-tries 10000)) check) 
    (let ((lock-sym (gensym))
          (cond-sym (gensym))
          (tries-sym (gensym))
          (try-sym (gensym))
          (found-sym (gensym)))
        `(let ((,lock-sym ,mutex)
               (,cond-sym ,cond)
               (,tries-sym ,max-tries)
               (,found-sym ,nil))
            (when (> ,tries-sym 0)
                (mp:giveup-lock ,lock-sym)
                (unwind-protect
                    (loop for ,try-sym from 1 to ,tries-sym do 
                        (when ,check
                            (setf ,found-sym t)
                            (return)))
                    (get-lock-spin ,lock-sym)))
            (unless (or ,found-sym ,check)
                (mp:condition-variable-wait ,cond-sym ,lock-sym)))))

(defparameter *worker-cond-spins* 100000)

(defvar *worker-count*    0)
(defvar *worker-threads*  ())
(defvar *worker-mutex*    (mp:make-lock))
(defvar *work-start-cond* (mp:make-condition-variable))
(defvar *work-done-cond*  (mp:make-condition-variable))

(defvar *working-threads* 0)
(defvar *task-id*         0)
(defvar *workers-failed*  0)
(defvar *task-function*   nil)

(defvar *dispatch-lock*   (mp:make-lock))
(defvar *dispatch-pos*    0)
(defvar *dispatch-limit*  0)

(defun worker-thread (idx)
    (format t "Worker ~A starting.~%" idx)
    (unwind-protect
        (let ((last-id 0) (caught-error nil))
            (loop
                (multiple-value-bind
                        (task task-id w-count)
                        (with-lock-spin (*worker-mutex*)
                            (unless (and *task-function*
                                         (/= *task-id* last-id))
                                (condition-wait-spin (*work-start-cond* *worker-mutex*
                                                         :max-tries *worker-cond-spins*)
                                    (/= *task-id* last-id)))
                            (when (> idx *worker-count*)
                                (return-from worker-thread))
                            (values *task-function* *task-id* *worker-count*))
                    (when (and task (/= task-id last-id))
                        (setf last-id task-id)
                        (setf caught-error nil)
                        (unwind-protect
                            (handler-case
                                (funcall task idx (1+ w-count))
                                (condition (err)
                                    (setf caught-error t)
                                    (format t "Worker ~A failed:~%  ~A~%" idx err)))
                            (with-lock-spin (*worker-mutex*)
                                (incf *working-threads* -1)
                                (when caught-error
                                    (incf *workers-failed*))
                                (when (<= *working-threads* 0)
                                    (mp:condition-variable-broadcast *work-done-cond*))))))))
        (format t "Worker ~A exited.~%" idx)))

(defun thread-dispatch (idx fun)
    (loop
        (funcall fun idx *dispatch-limit*)
        (with-lock-spin (*dispatch-lock*)
            (setf idx *dispatch-pos*)
            (when (< *dispatch-pos* *dispatch-limit*)
                (incf *dispatch-pos*)))
        (unless (< idx *dispatch-limit*)
            (return))))

(defun wrap-dispatch (fun)
    #'(lambda (idx num)
        (thread-dispatch idx fun)))

(defun spawn-worker-threads (num)
    (mp:with-lock (*worker-mutex*)
        (do () ((<= *worker-count* num))
            (incf *worker-count* -1)
            (pop *worker-threads*))
        (incf *task-id*)
        (mp:condition-variable-broadcast *work-start-cond*)
        (do () ((>= *worker-count* num))
            (incf *worker-count* 1)
            (push
                (mp:process-run-function 'worker
                    #'worker-thread *worker-count*)
                *worker-threads*))))

(defun run-work (fun &key (dispatch-limit 1))
    (when (null fun)
        (error "Cannot run a NIL task"))
    (mp:with-lock (*worker-mutex*)
        (when *task-function*
            (error "Task already running"))
        (setf *task-function* fun)
        (setf *workers-failed* 0)
        (setf *working-threads* *worker-count*)
        (setf *dispatch-pos* (1+ *worker-count*))
        (setf *dispatch-limit* (* *dispatch-pos* (max 1 dispatch-limit)))
        (incf *task-id*)
        (mp:condition-variable-broadcast *work-start-cond*))
    (let ((success nil))
        (unwind-protect
            (progn
                (funcall fun 0 (1+ *worker-count*))
                (setf success t))
            (with-lock-spin (*worker-mutex*)
                (when (> *working-threads* 0)
                    (condition-wait-spin (*work-done-cond* *worker-mutex*
                                             :max-tries *worker-cond-spins*)
                        (<= *working-threads* 0)))
                (setf *task-function* nil))
            ;; Transfer errors from worker threads if we succeeded
            (when (and success (> *workers-failed* 0))
                (error "~A worker threads failed." *workers-failed*)))))

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

