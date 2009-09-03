;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

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
  (let* ((text-sym        (intern "TEXT"))
         (recurse-sym     (intern "RECURSE"))
         (recurse-str-sym (intern "RECURSE-STR"))
         (arg-keys (mapcar #'(lambda (sym)
                               (if (and (symbolp sym)
                                        (equal (symbol-name sym)
                                               (format nil "~A-TYPE" form-arg)))
                                   `(,sym (if *cg-type-table*
                                              (gethash cg-form *cg-type-table*)))
                                   sym))
                           args)))
    (values `(cg-full-stack cg-cur-stack cg-output cg-form &rest cg-flags
                            ,@(if arg-keys
                                  `(&key ,@arg-keys &allow-other-keys)))
            `(let ((,form-arg cg-form))
               (declare (ignorable ,form-arg))
               (macrolet
                   ((,text-sym (pattern &rest args)
                      (if (and (stringp pattern)
                               (or args (position #\~ pattern)))
                          `(format cg-output (formatter ,pattern) ,@args)
                          (progn
                            (assert (null args))
                            `(write-string ,pattern cg-output))))
                    (,recurse-sym (expr &rest flags)
                      (let ((use-stack 'cg-full-stack))
                        (when (find (car flags) '(:use-stack :switch-stack))
                          (setf use-stack (cadr flags))
                          (setf flags (cddr flags)))
                        `(let ((stack ,use-stack))
                           (funcall (car stack)
                                    stack (cdr stack)
                                    cg-output ,expr ,@flags))))
                    (,recurse-str-sym (&rest args)
                      `(with-output-to-string (cg-output)
                         (,',recurse-sym ,@args))))
                 (match cg-form
                   ,@body
                   (_
                     (if cg-cur-stack
                         (apply (car cg-cur-stack)
                                cg-full-stack (cdr cg-cur-stack)
                                cg-output cg-form cg-flags)
                         (error "Cannot compile, unrecognized form: ~A" cg-form)))))))))

(defmacro form-compiler (arg-set &body body)
  (assert (consp arg-set))
  (multiple-value-bind (arg-spec code)
      (do-make-form-compiler (car arg-set) (cdr arg-set) body)
    `#'(lambda ,arg-spec ,code)))

(defmacro def-form-compiler (name arg-set &body body)
  (assert (consp arg-set))
  (multiple-value-bind (arg-spec code)
      (do-make-form-compiler (car arg-set) (cdr arg-set) body)
    `(defun ,name ,arg-spec ,code)))

(defun call-form-compilers (stack form &rest flags)
  (with-output-to-string (out)
    (apply (car stack)
           stack (cdr stack) out
           form flags)))
