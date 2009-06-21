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

    (let ((c::*ld-flags*
              (concatenate 'string c::*ld-flags*
                  " -lcuda"))
          (c::*ld-shared-flags*
              (concatenate 'string c::*ld-shared-flags*
                  " -lcuda"))
          (c::*ld-bundle-flags*
              (concatenate 'string c::*ld-shared-flags*
                  " -lcuda"))
          (c::*cc-flags*
              (concatenate 'string c::*cc-flags*
                  " -I/usr/local/cuda/include")))
        (load-compile "driver-api")))
