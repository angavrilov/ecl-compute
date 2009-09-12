;;; -*- mode:lisp; indent-tabs-mode: nil; -*-
;;
;; A time iteration control engine with check-points.
;;
;; In order to start the process, write an initialization
;; script of the following form:
;;
;;   (init-processing
;;      ... mandatory initialization ...
;;      ... e.g. memory allocation ...)
;;
;;   ... first-time initialization ...
;;   ... (setf *time-step* ...) ...
;;
;;   (execute-tasks '((iter-cnt name task-step :keys...)
;;                    ...))
;;
;; The system will automatically generate similar scripts
;; to restart from check-points, which are saved after each
;; task iteration, and in the case of failure.
;;

;;; Interface variables

(fast-compute:deflex *cur-time* 0.0
  "Current inner time of the simulation.")

(fast-compute:deflex *time-step* 0.0
  "Base time step of the simulation.")

(fast-compute:deflex *default-ps-value* 1.0
  "Default value for the :ps boost parameter.")

(defvar *main-step-function* (lambda (step &rest flags)
                               (declare (ignore step flags))
                               (error "Step function not defined"))
  "Function to call for each step. Receives the actual time
   step and other flags as arguments.")

(defvar *init-array-list* nil
  "List of arrays that hold initial conditions and never change.")

(defvar *work-array-list* nil
  "List of arrays that hold the current state of the iteration.")

;;; Internal variables

(fast-compute:deflex *iter-index* 0
  "Index of the current iteration within the task")

(defparameter *init-script-path* nil)
(defparameter *base-log-path* "output")
(defparameter *init-script* nil)

(defparameter *task-list* nil)

;;; State dump and restore

(defun dump-arrays (id arr-lst)
  (labels ((name (sym)
             (concatenate 'string id "." (symbol-name sym))))
    (format t "~%Saving state to:~%  ~A.*~%" id)
    (ensure-directories-exist id)
    (dolist (arr arr-lst)
      (fast-compute:dump-array (name arr) (symbol-value arr)))))

(defun restore-arrays (id arr-lst)
  (labels ((name (sym)
             (concatenate 'string id "." (symbol-name sym))))
    (format t "~%Restoring state from:~%  ~A.*~%" id)
    (dolist (arr arr-lst)
      (format t "   ~A ...~%" arr)
      (fast-compute:restore-array (name arr) (symbol-value arr)))))

(defmacro init-processing (&whole script &body code)
  "Denotes the mandatory initialization part of the init script.
The code is automatically replicated to all checkpoints."
  `(progn
     (setf *init-script* ',script)
     (setf *init-script-path* ,*load-pathname*)
     (setf *base-log-path*
           ,(concatenate 'string
                         (directory-namestring *load-pathname*)
                         (pathname-name *load-pathname*)))
     ,@code))

(defun dump-state (tag &key (tail-tag ""))
  (let ((save-path (format nil "~A/~A~3,'0D.~5,'0D~A"
                           *base-log-path* tag *iter-index*
                           (round *cur-time*) tail-tag)))
    (dump-arrays save-path *work-array-list*)
    (with-open-file
        (restart-file (concatenate 'string save-path ".lisp")
                      :direction :output :if-exists :supersede)
      (pprint *init-script* restart-file)
      (pprint `(restore-state) restart-file)
      (pprint `(setf *cur-time* ,*cur-time*) restart-file)
      (pprint `(setf *time-step* ,*time-step*) restart-file)
      (pprint `(setf *iter-index* ,*iter-index*) restart-file)
      (pprint `(execute-tasks ',*task-list*) restart-file))))

(defun restore-state (&optional (path *base-log-path*))
  (restore-arrays (concatenate 'string
                               (directory-namestring path) "INIT")
                  *init-array-list*)
  (restore-arrays path *work-array-list*))

;;; Iteration control

(defun execute-step (end-time &rest params
                     &key (ps *default-ps-value* ps-set-p)
                     &allow-other-keys)
  ;; Inject the default value for ps if not set
  (unless ps-set-p
    (setf params (list* :ps ps params)))
  ;; Main loop
  (let ((msg-time (get-internal-real-time))
        (step     (/ *time-step* ps))
        (tfmt     (formatter "  NOW=~,4F, END=~,2F; STEP=~,4E, STEP/PS=~,4E~%")))
    (standard-cl:while (< *cur-time* end-time)
      ;; Print the progress line every second
      (let ((now (get-internal-real-time)))
        (when (>= (- now msg-time) internal-time-units-per-second)
          (format t tfmt *cur-time* end-time *time-step* step)
          (setf msg-time now)))

      ;; Call the step function
      (apply *main-step-function* step :allow-other-keys t params)

      ;; Advance time
      (setf *cur-time* (+ *cur-time* step)))))

(defun execute-task (rem-iter tag min-step &rest params
                     &key (max-step min-step) &allow-other-keys)
  (let* ((end-time (+ *cur-time*
                      (min (* min-step *iter-index*)
                           max-step))))
    (format t "~%STAGE ~A~3,'0D: START=~,2F, END=~,2F~%"
            tag *iter-index* *cur-time* end-time)

    ;; Execute the step with automatic dump in case of failure
    (handler-bind
        ((condition (lambda (cond)
                      (dump-state tag :tail-tag ".FAIL"))))
      (time (apply #'execute-step end-time params)))

    (dump-state tag)))

(defun do-execute-tasks (tasklist)
  (do ((head (car tasklist) (car tasklist)))
      ((null tasklist) nil)
    (if (<= (first head) 0)
        ;; Next task
        (setf *iter-index* 0
              tasklist (cdr tasklist))
        (progn
          (incf *iter-index*)
          (decf (first head))
          (let ((*task-list* tasklist))
            (apply #'execute-task head))))))

(defun execute-tasks (tasklist)
  "Executes the supplied list of computation tasks."
  (dump-arrays (format nil "~A/INIT" *base-log-path*)
               *init-array-list*)
  (when *init-script-path*
    (si:copy-file *init-script-path*
                  (format nil "~A/INIT.lisp" *base-log-path*)))
  (do-execute-tasks tasklist))
