;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(ffi:clines "#include <stdio.h>")

(defun dump-array (file arr)
    (ffi:c-inline
        (file
            (if (typep arr 'multivalue)
                (multivalue-data-f arr :read)
                arr))
        (:cstring :object)
        :void
        "{ FILE *f;
           int tmp,i,size;
           cl_object arr = #1;

           if (!ARRAYP(arr) && !VECTORP(arr))
             FEerror(\"Not an array: ~A\",1,arr);
           if ((VECTORP(arr)?arr->vector.elttype:arr->array.elttype) != aet_sf)
             FEerror(\"Not a float array: ~A\",1,arr);

           f = fopen(#0, \"wb\");

           if (VECTORP(arr)) {
             tmp = 1;
             fwrite(&tmp, 1, sizeof(int), f);
             tmp = size = arr->vector.dim;
             fwrite(&tmp, 1, sizeof(int), f);
           } else {
             tmp = arr->array.rank;
             fwrite(&tmp, 1, sizeof(int), f);
             size = 1;
             for (i = 0; i < arr->array.rank; i++) {
               tmp = arr->array.dims[i];
               size *= tmp;
               fwrite(&tmp, 1, sizeof(int), f);
             }
           }

           fwrite(VECTORP(arr)?arr->vector.self.sf:arr->array.self.sf,
                  size, sizeof(float), f);
           fclose(f); }"))

(defun restore-array (file arr)
    (ffi:c-inline
        (file
            (if (typep arr 'multivalue)
                (multivalue-data-f arr :write-all)
                arr)
            file)
        (:cstring :object :object)
        :void
        "{ FILE *f;
           int tmp,rank,dim,i,size;
           cl_object arr = #1;
           cl_object str = #2;

           if (!ARRAYP(arr) && !VECTORP(arr))
             FEerror(\"Not an array: ~A\",1,arr);
           if ((VECTORP(arr)?arr->vector.elttype:arr->array.elttype) != aet_sf)
             FEerror(\"Not a float array: ~A\",1,arr);

           f = fopen(#0, \"rb\");
           fread(&tmp, 1, sizeof(int), f);

           rank = (VECTORP(arr)?1:arr->array.rank);
           if (tmp != rank) {
             fclose(f);
             FEerror(\"File ~A has rank ~A, expected ~A\", 3,
                     str, MAKE_FIXNUM(tmp), MAKE_FIXNUM(rank));
           }

           size = 1;
           for (i = 0; i < rank; i++) {
             fread(&tmp, 1, sizeof(int), f);
             dim = (VECTORP(arr)?arr->vector.dim:arr->array.dims[i]);
             if (tmp != dim) {
               fclose(f);
               FEerror(\"File ~A has dim ~A at ~A, expected ~A\", 4,
                       str, MAKE_FIXNUM(tmp), MAKE_FIXNUM(i), MAKE_FIXNUM(dim));
             }
             size *= dim;
           }

           fread(VECTORP(arr)?arr->vector.self.sf:arr->array.self.sf,
                  size, sizeof(float), f);
           fclose(f); }"))

