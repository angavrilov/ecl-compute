;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(defpackage fast-compute
    (:documentation "Fast array computation library")
    (:use "COMMON-LISP" "CL-MATCH")
    (:export
        "DEF-MULTIVALUE" "ALLOC-MULTIVALUES"
        "WITH-LOCAL-MULTIVALUES" "IREF"
        "ENABLE-EXPR-QUOTES" "LOOP-INDEXES"
        "*CURRENT-COMPUTE*" "COMPUTE"
        "LETV" "CALC"
    ))
