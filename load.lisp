(require 'standard-cl)
(require 'cl-match)

(cl-match:defpattern si:quasiquote (&rest args)
    (macroexpand-1 (cons 'si:quasiquote args)))

(labels ((load-interp (name)
             (load (merge-pathnames name *load-pathname*)))
         (load-compile (name)
             (unless (load
                         (merge-pathnames
                             (concatenate 'string name ".fas")
                             *load-pathname*)
                         :if-does-not-exist nil)
                 (compile-file
                     (merge-pathnames
                         (concatenate 'string name ".lisp")
                         *load-pathname*)
                     :load t))))

    (dolist (module-name
                '("formula.lisp"
                  "compute-pkg.lisp"
                  "expr-index.lisp"
                  "expr-optimize.lisp"
                  "expr-refactor.lisp"
                  "expr-types.lisp"
                  "multivalue.lisp"))
        (load-interp module-name))

    (load-compile "thread-core")

    (dolist (module-name
                '("threads.lisp"
                  "compute.lisp"
                  "compile-sse.lisp"))
        (load-interp module-name))

    (let ((c::*cc-flags*
              (concatenate 'string c::*cc-flags*
                  " -msse2")))
        (load-compile "utils")))
