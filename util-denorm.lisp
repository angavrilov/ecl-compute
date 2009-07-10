;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(ffi:clines "#include <xmmintrin.h>")

(defun allow-denormalized-floats (&optional (enable-p t))
    (= 0
        (ffi:c-inline ((if enable-p 1 0)) (:int) :int
            "{ unsigned status = _mm_getcsr();
               @(return) = (status & 0x8040);
               if (#0)
                  status &= ~0x8040;
               else
                  status |= 0x8040;
               _mm_setcsr(status);
             }")))
