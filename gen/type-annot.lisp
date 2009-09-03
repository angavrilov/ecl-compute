;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(def-rewrite-pass do-annotate-types
    ((types) :recurse-function #'map-skipping-structure)
  (`(index ,@_)
    old-expr)
  (`(,(or 'let 'let* 'symbol-macrolet) ,@_)
    nil)
  (`(multivalue-data ,@_)
    `(the (array single-float) ,old-expr))
  (`(setf (the ,_ ,arg) ,tgt)
    `(setf ,arg ,tgt))
  (_
    (multiple-value-bind (type found) (gethash old-expr types)
      (if found
          (let ((tspec (match type
                         ('float 'single-float)
                         ('integer 'fixnum)
                         ('boolean 'boolean)
                         ('array 'array)
                         ('nil 'single-float)
                         (_ (error "Bad type ~A" type)))))
            `(the ,tspec ,expr))
          expr))))

(defun annotate-types (expr)
  (do-annotate-types expr (derive-types expr)))
