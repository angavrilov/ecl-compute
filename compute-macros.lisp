;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defmacro error-fallback (message fallback &body code)
   `(block handle
        (handler-bind ((condition
                           #'(lambda (cond)
                                 (format t ,message cond)
                                 (return-from handle ,fallback))))
            ,@code)))

#-cuda
(defmacro compute (&whole original &rest args)
    (apply #'do-make-lisp-compute original args))

#+cuda
(defmacro compute (&whole original &rest args)
    (let ((lisp-code
              (apply #'do-make-lisp-compute original args)))
        (error-fallback
                "CUDA compilation failed:~%   ~A~%"
                lisp-code
            `(if (cuda:valid-context-p)
                 ,(apply #'do-make-cuda-compute original args)
                 ,lisp-code))))

#-cuda
(define-compiler-macro compute (&whole original &rest args)
    (error-fallback
            "~%Fast C compilation failed:~%   ~A~%Reverting to ordinary lisp.~%"
            original
        (apply #'do-make-c-compute original args)))

#+cuda
(define-compiler-macro compute (&whole original &rest args)
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
                 ,lisp-code))))