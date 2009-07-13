;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(defpackage fast-compute
    (:documentation "Fast array computation library")
    (:use "COMMON-LISP" "STANDARD-CL" "CL-MATCH")
    (:export
        "MULTIVALUE" "MULTIVALUE-DATA" "MULTIVALUE-SYNC"
        "DEF-MULTIVALUE" "COPY-MULTIVALUE"
        "ALLOC-MULTIVALUES" "WITH-LOCAL-MULTIVALUES"
        "IREF" "ENABLE-EXPR-QUOTES" "LOOP-INDEXES"
        "*CURRENT-COMPUTE*" "COMPUTE"
        "LETV" "CALC" "_GRP"
        "SET-COMPUTE-THREAD-COUNT" "PARALLEL-LOOP"
        "*COMPUTE-WITH-CUDA*"
        "DUMP-ARRAY" "RESTORE-ARRAY"
        "ALLOW-DENORMALIZED-FLOATS"
    ))
