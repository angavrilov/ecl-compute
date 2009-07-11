;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *cg-full-expr* nil)
(defparameter *cg-type-table* nil)

(defun get-inline-tag (idx)
    (aref #("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
            "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
            "u" "v" "w" "x" "y" "z") idx))

(defun get-factored-cg-type (expr)
    (gethash (unwrap-factored expr) *cg-type-table*))

(defun do-make-form-compiler (form-arg args body)
   `(let ((,form-arg cg-form)
          ,@(mapcar
                #'(lambda (sym)
                      `(,sym
                           ,(if (equal (symbol-name sym)
                                    (format nil "~A-TYPE" form-arg))
                                '(if *cg-type-table*
                                     (gethash cg-form *cg-type-table*))
                                'nil)))
                args))
        (declare (ignorable ,form-arg))
        ,@(if args
              `((do ((aptr cg-flags (cddr aptr)))
                    ((null aptr))
                    (case (car aptr)
                        ,@(mapcar
                              #'(lambda (sym)
                                    `(,(intern (symbol-name sym) "KEYWORD")
                                         (setf ,sym (cadr aptr))))
                              args)))))
        (macrolet
                ((text (pattern &rest args)
                     (if (and (stringp pattern)
                             (or args
                                 (position #\~ pattern)))
                         `(format cg-output (formatter ,pattern) ,@args)
                         (progn
                             (assert (null args))
                             `(write-string ,pattern cg-output))))
                 (recurse (expr &rest flags)
                     (let ((use-stack 'cg-full-stack))
                         (when (find (car flags)
                                   '(:use-stack :switch-stack))
                             (setf use-stack (cadr flags))
                             (setf flags (cddr flags)))
                        `(let ((stack ,use-stack))
                             (funcall (car stack)
                                 stack (cdr stack)
                                 cg-output ,expr (list ,@flags)))))
                 (recurse-str (&rest args)
                     `(with-output-to-string (cg-output)
                          (recurse ,@args))))
            (match cg-form
                ,@body
                (_
                    (if cg-cur-stack
                        (funcall (car cg-cur-stack)
                            cg-full-stack (cdr cg-cur-stack)
                            cg-output cg-form cg-flags)
                        (error "Cannot compile, unrecognized form: ~A" cg-form)))))))

(defmacro form-compiler (arg-set &body body)
    (assert (consp arg-set))
    `#'(lambda (cg-full-stack cg-cur-stack cg-output cg-form cg-flags)
           ,(do-make-form-compiler (car arg-set) (cdr arg-set) body)))

(defmacro def-form-compiler (name arg-set &body body)
    (assert (consp arg-set))
    `(defun ,name (cg-full-stack cg-cur-stack cg-output cg-form cg-flags)
         ,(do-make-form-compiler (car arg-set) (cdr arg-set) body)))

(defun call-form-compilers (stack form &rest flags)
    (with-output-to-string (out)
        (funcall (car stack)
            stack (cdr stack) out
            form flags)))
