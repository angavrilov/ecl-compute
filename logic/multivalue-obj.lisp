;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defvar *log-cuda-memcpy* nil)

(defstruct multivalue
    (name nil :type symbol)
    (data-dims nil :type list :read-only t)
    (data-array nil :type (array single-float) :read-only t)
  #+cuda
    (cuda-buffer nil)
  #+cuda
    (host-valid t)
  #+cuda
    (cuda-valid nil))

#+cuda
(defun multivalue-cuda-dimension (mv idx)
    (let ((mvarr (multivalue-data-array mv)))
        (if (= idx (1- (array-rank mvarr)))
            (/ (cuda:linear-pitch
                   (multivalue-cuda-buffer mv))
                4)
            (array-dimension mvarr idx))))

(defun multivalue-wrap-sync (sync-spec body)
  #-cuda body
  #+cuda
    (let ((cur-target :host)
          (cur-mode :unknown)
          (cmd-list nil))
        (dolist (op sync-spec)
            (if (or (null op) (keywordp op))
                (case op
                    ((nil) nil)
                    ((:host :cuda-device)
                        (setf cur-target op))
                    ((:unknown :read :read-write :write :write-all)
                        (setf cur-mode op))
                    (t
                        (error "Unknown keyword: ~A" op)))
                (push
                   `(let* ((mv ,op)
                           (mvcb (multivalue-cuda-buffer mv))
                           (mvarr (multivalue-data-array mv)))
                        (declare (ignorable mvcb mvarr))
                      ,@(case cur-target
                            (:cuda-device
                                ;; For a CUDA kernel
                                (cond-list
                                   (t
                                       `(unless (cuda:valid-linear-buffer-p mvcb)
                                            (setf mvcb
                                                (cuda:create-linear-for-array mvarr))
                                            (setf (multivalue-cuda-buffer mv) mvcb)
                                            (setf (multivalue-cuda-valid mv) nil)))
                                   ((not (eql cur-mode :write-all))
                                       `(unless (multivalue-cuda-valid mv)
                                            (when *log-cuda-memcpy*
                                                (format t "host->device: ~A (~A)~%"
                                                    ',op (multivalue-name mv)))
                                            (cuda:copy-linear-for-array mvcb mvarr)))
                                   (t
                                       `(setf (multivalue-cuda-valid mv) t))
                                   ((not (eql cur-mode :read))
                                       `(setf (multivalue-host-valid mv) nil))))
                            (:host
                                ;; For host code
                                (cond-list
                                   ((not (eql cur-mode :write-all))
                                       `(unless (multivalue-host-valid mv)
                                            (when *log-cuda-memcpy*
                                                (format t "device->host: ~A (~A)~%"
                                                    ',op (multivalue-name mv)))
                                            (cuda:copy-linear-for-array
                                                mvcb mvarr :from-device t)))
                                   (t
                                       `(setf (multivalue-host-valid mv) t))
                                   ((not (eql cur-mode :read))
                                       `(setf (multivalue-cuda-valid mv) nil))))))
                    cmd-list)))
        (if cmd-list
            `(progn ,@(nreverse cmd-list) ,body)
            body)))

(defmacro multivalue-sync (&rest args)
    (multivalue-wrap-sync args nil))

(defmacro multivalue-data (mv &optional use-mode)
    (if (eql use-mode t)
       `(multivalue-data-array ,mv)
        (multivalue-wrap-sync
            (list (or use-mode :unknown) mv)
           `(multivalue-data-array ,mv))))

(defun multivalue-data-f (mv use-mode)
    (ecase use-mode
        (:read
            (multivalue-data mv :read))
        (:write-all
            (multivalue-data mv :write-all))))
