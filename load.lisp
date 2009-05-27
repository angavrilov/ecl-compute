(dolist (module-name
            '("formula.lisp"
              "compute-pkg.lisp"
              "expr-index.lisp"
              "expr-refactor.lisp"
              "expr-types.lisp"
              "expr-optimize.lisp"
              "multivalue.lisp"
              "compute.lisp"
              "compile-sse.lisp"))
     (load (merge-pathnames module-name *load-pathname*)))

(unless (load
            (merge-pathnames "utils.fas" *load-pathname*)
            :if-does-not-exist nil)
    (compile-file
        (merge-pathnames "utils.lisp" *load-pathname*)
        :load t))
