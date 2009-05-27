(ffi:clines "#include <stdio.h>")

(defun dump-array (file arr)
    (unless (eql (array-element-type arr) 'single-float)
        (error "Can dump only float arrays"))
    (let ((size (reduce #'* (array-dimensions arr))))
        (ffi:c-inline (file size arr) (:cstring :int :object) :void
            "{ FILE *f = fopen(#0, \"wb\");
               fwrite(VECTORP(#2)?(#2)->vector.self.sf:(#2)->array.self.sf,
                      #1, sizeof(float), f);
               fclose(f); }")))
