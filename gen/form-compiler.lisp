;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(use-std-readtable)

(defparameter *cg-full-expr* nil)
(defparameter *cg-type-table* nil)

(defun get-inline-tag (idx)
  (aref #("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
          "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
          "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
          "u" "v" "w" "x" "y" "z") idx))

(defun get-factored-cg-type (expr)
  (gethash (unwrap-factored expr) *cg-type-table*))

(defun make-form-compiler$code (arg text-sym recurse-sym)
  (match arg
    ((or (type string _)
         `(,(type string _) ,@_))
      `(,text-sym ,@(ensure-list arg)))
    (`(:text ,@rest)
      `(,text-sym ,@rest))
    (`(:when ,cond ,@rest)
      `(when ,cond
         ,(make-form-compiler$code (if (cdr rest) rest (car rest))
                                   text-sym recurse-sym)))
    (`(:unless ,cond ,@rest)
      `(unless ,cond
         ,(make-form-compiler$code (if (cdr rest) rest (car rest))
                                   text-sym recurse-sym)))
    (`(:recurse ,@rest)
      `(,recurse-sym ,@rest))
    (_
      `(,recurse-sym ,arg))))

(defun do-make-form-compiler (form-arg args body)
  (let* ((text-sym        (intern "TEXT"))
         (code-sym        (intern "CODE"))
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
                    (,code-sym (&rest args)
                      `(progn
                         ,@(mapcar #f(make-form-compiler$code
                                      _ ',text-sym ',recurse-sym)
                                   args)))
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
  "Args: (name arg-list &body body)
Defines a form compiler rule set. The argument set must be a
non-empty list of symbols. During execution the first parameter
is set to the form being evaluated, while the rest are matched
to the optional key parameters. As a special case, the parameter
named as the first one plus '-type' is by default set to the
type of the form as supplied via *cg-type-table*.
  The body is a list of match forms. Inside the body the following
macros can be used:

  (text string arg...)
    Outputs the text via format or write-string. Specifically,
    it uses format if the string is a constant, and either any
    additional args are given, or the string contains '~'.

  (recurse form [:use-stack stack] keys...)
    Recursively processes the form. A special :use-stack key,
    which must be specified first, can be used to switch the
    compiler rule set stack. Other key parameters are passed
    to the rules. No key validity checking is done.

  (recurse-str ...)
    Like recurse, but captures the output and returns it.

  (code ...)
    A convenience wrapper around text and recurse. String
    constants and lists beginning with a string constant
    are converted to (text...), other forms to (recurse...).
    Special cases:
      (:text ...)        - explicit (text)
      (:recurse ...)     - explicit (recurse)
      (:when cond ...)
      (:unless cond ...) - guard conditions
    Converted arguments are grouped using (progn...)
"
  (assert (consp arg-set))
  (multiple-value-bind (arg-spec code)
      (do-make-form-compiler (car arg-set) (cdr arg-set) body)
    `(defun ,name ,arg-spec ,code)))

(defun call-form-compilers (stack form &rest flags)
  (with-output-to-string (out)
    (apply (car stack)
           stack (cdr stack) out
           form flags)))
