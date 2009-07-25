;;; Load the code depending on whether CUDA is available.
(let ((rc (ignore-errors
              (ext:system "nvcc --version &>/dev/null"))))
    (if (eql rc 0)
        (load (merge-pathnames "load.lisp" *load-pathname*))
        (format t "Skipping CUDA: call to nvcc failed.")))
