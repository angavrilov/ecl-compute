;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(defpackage fast-compute
  (:documentation "Fast array computation library")
  (:use "COMMON-LISP" "CL-MATCH" "ALEXANDRIA"
        "FSET" "GMAP" "NEW-LET" "LEXICAL-CONTEXTS")
  (:export "MULTIVALUE" "MULTIVALUE-DATA" "MULTIVALUE-SYNC"
           "DEF-MULTIVALUE" "COPY-MULTIVALUE" "DEF-MULTIVALUE-MACRO"
           "ALLOC-MULTIVALUES" "WITH-LOCAL-MULTIVALUES"
           "IREF" "ENABLE-EXPR-QUOTES" "LOOP-INDEXES"
           "*CURRENT-COMPUTE*" "COMPUTE"
           "LETV" "CALC" "_GRP"
           "SET-COMPUTE-THREAD-COUNT" "PARALLEL-LOOP"
           "*COMPUTE-WITH-CUDA*"
           "DUMP-ARRAY" "RESTORE-ARRAY"
           "ALLOW-DENORMALIZED-FLOATS"
           "COMPUTE-BATCH")
  (:import-from "STANDARD-CL"
                "USE-STD-READTABLE" "DO-HASHTABLE"
                "SPLIT-LIST" "SUM" "WHILE" "UNTIL")
  (:shadowing-import-from "COMMON-LISP" "LET" "COND" "LAST")
  (:shadowing-import-from "FSET"
                          ;; Shadowed type/constructor names
                          "SET" "MAP"
                          ;; Alexandria conflicts
                          "REMOVEF" "UNIONF" "COMPOSE"
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
                          "SOME" "EVERY" "NOTANY" "NOTEVERY"))
