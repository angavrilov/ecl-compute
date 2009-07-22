;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(defpackage fast-compute
    (:documentation "Fast array computation library")
    (:use "COMMON-LISP" "STANDARD-CL" "CL-MATCH"
          "FSET" "GMAP" "NEW-LET" "LEXICAL-CONTEXTS")
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
    )
    (:shadowing-import-from "COMMON-LISP" "LET" "COND" "LAST")
    (:shadowing-import-from "FSET"
        ;; Shadowed type/constructor names
        "SET" "MAP"
        ;; STANDARD-CL conflicts
        "CONCAT" "RANGE"
        ;; Shadowed set operations
        "UNION" "INTERSECTION" "SET-DIFFERENCE" "COMPLEMENT"
        ;; Shadowed sequence operations
        "FIRST" "SUBSEQ" "REVERSE" "SORT" "STABLE-SORT"
        "REDUCE"
        "FIND" "FIND-IF" "FIND-IF-NOT"
        "COUNT" "COUNT-IF" "COUNT-IF-NOT"
        "POSITION" "POSITION-IF" "POSITION-IF-NOT"
        "REMOVE" "REMOVE-IF" "REMOVE-IF-NOT"
        "SUBSTITUTE" "SUBSTITUTE-IF" "SUBSTITUTE-IF-NOT"
        "SOME" "EVERY" "NOTANY" "NOTEVERY"
    ))
