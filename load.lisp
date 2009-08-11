(require 'standard-cl)
(require 'cl-match)
(require 'misc-extensions)
(require 'fset)

(cl-match:defpattern si:quasiquote (&rest args)
    (macroexpand-1 (cons 'si:quasiquote args)))

;; Must be done separately so that the change
;; to *features* propagates to #+cuda below.
(load (merge-pathnames "cuda/load-auto.lisp" *load-pathname*))

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
                  "util-misc.lisp"))
        (load-interp module-name))

    (load-compile "thread-core")

    (dolist (module-name
                '("expr/canonify.lisp"
                  "expr/rewrite.lisp"
                  "expr/form-defs.lisp"
                  "expr/simplify.lisp"
                  "expr/ranges.lisp"
                  "expr/loop-level.lisp"
                  "expr/opt-flatten.lisp"
                  "expr/opt-pullexpr.lisp"
                  "expr/opt-treeify.lisp"
                  "expr/let-utils.lisp"
                  "expr/cse.lisp"
                  "expr/types.lisp"
                  "expr/ref-info.lisp"
                  "logic/multivalue-obj.lisp"
                  "logic/multivalue.lisp"
                  "logic/checks.lisp"
                  "logic/iref.lisp"
                  "logic/loops.lisp"
                  "logic/parallel.lisp"
                  "logic/make-carry.lisp"
                  "logic/make-cluster.lisp"
                  "logic/compute.lisp"
                  "gen/type-annot.lisp"
                  "gen/expand-aref.lisp"
                  "gen/expand-macros.lisp"
                  "gen/form-compiler.lisp"
                  "gen/tgt-lisp.lisp"
                  "gen/tgt-generic-c.lisp"
                  "gen/tgt-sse-intrin.lisp"
                  "gen/tgt-inline-c.lisp"
           #+cuda "gen/cuda-textures.lisp"
           #+cuda "gen/minimize-live.lisp"
           #+cuda "gen/splice-carried.lisp"
           #+cuda "gen/localize-temp.lisp"
           #+cuda "gen/tgt-cuda.lisp"
                  "compute-macros.lisp"))
        (load-interp module-name))

    (let ((c::*cc-flags*
              (concatenate 'string c::*cc-flags*
                  " -msse2")))
        (load-compile "util-arrays")
        (load-compile "util-denorm")))

