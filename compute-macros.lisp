;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defvar *compute-with-cuda* t)

(defmacro error-fallback (message fallback &body code)
  "Args: (message fallback &body code)"
  `(block handle
     (handler-bind
         ((condition #'(lambda (cond)
                         (format t ,message cond)
                         (return-from handle ,fallback))))
       ,@code)))

#-cuda
(defmacro compute (&whole original &rest args)
  (apply #'do-make-lisp-compute original args))

#+cuda
(defmacro compute (&whole original &rest args)
  (case *compute-with-cuda*
    ((nil)
     (apply #'do-make-lisp-compute original args))
    (:force
     (error-fallback
         "CUDA compilation failed:~%   ~A~%"
         (apply #'do-make-lisp-compute original args)
       (apply #'do-make-cuda-compute original args)))
    (otherwise
     (let ((lisp-code (apply #'do-make-lisp-compute original args)))
       (error-fallback
           "CUDA compilation failed:~%   ~A~%"
           lisp-code
         `(if (cuda:valid-context-p)
              ,(apply #'do-make-cuda-compute original args)
              ,lisp-code))))))

#-cuda
(define-compiler-macro compute (&whole original &rest args)
  (error-fallback
      "~%Fast C compilation failed:~%   ~A~%Reverting to ordinary lisp.~%"
      original
    (apply #'do-make-c-compute original args)))

#+cuda
(define-compiler-macro compute (&whole original &rest args)
  (case *compute-with-cuda*
    ((nil)
     (error-fallback
         "~%Fast C compilation failed:~%   ~A~%Reverting to ordinary lisp.~%"
         original
       (apply #'do-make-c-compute original args)))
    (:force
     (error-fallback
         "CUDA compilation failed:~%   ~A~%"
         (apply #'do-make-lisp-compute original args)
       (apply #'do-make-cuda-compute original args)))
    (otherwise
     (let ((lisp-code
            (error-fallback
                "~%Fast C compilation failed:~%   ~A~%Reverting to ordinary lisp.~%"
                (apply #'do-make-lisp-compute original args)
              (apply #'do-make-c-compute original args))))
       (error-fallback
           "CUDA compilation failed:~%   ~A~%"
           lisp-code
         `(if (cuda:valid-context-p)
              ,(apply #'do-make-cuda-compute original args)
              ,lisp-code))))))

#-cuda
(defmacro compute-batch (&body code)
  `(progn ,@code))

#+cuda
(defmacro compute-batch (&body code)
  `(unwind-protect
        (cuda:with-async ,@code)
     (cuda:synchronize)))
