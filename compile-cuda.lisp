;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun replace-dim (expr old-expr)
    (match expr
        (`(arr-dim (multivalue-data ,mv ,@_) ,idx)
            `(multivalue-cuda-dimension ,mv ,idx))
        (`(arr-dim ,@_)
            (error "Bad dimension: ~A" expr))))

(defun has-atom (tree sym)
    (if (and tree (consp tree))
        (reduce #'(lambda (a b) (or a b))
            (mapcar #'(lambda (x) (has-atom x sym))
                tree))
        (eql tree sym)))

(defun compile-expr-cuda (block-dim full_expr)
    (let ((types (derive-types full_expr))
          (args  ())
          (top-found nil)
          (init-calcs nil)
          (dims  ()))
        (labels ((ref-arg (name expr &optional etype)
                     (let ((sym-type (or etype
                                         (gethash expr types))))
                         (push
                             (list
                                 (ecase sym-type
                                     (float :float)
                                     (integer :int))
                                 name
                                 (simplify-rec-once #'replace-dim
                                     (get-full-expr expr)))
                             args)))
                 (ref-array (name arr)
                     (match (get-full-expr arr)
                         (`(multivalue-data ,mv ,@_)
                             (push
                                  (list :float-ptr
                                      name
                                      `(multivalue-cuda-buffer ,mv))
                                  args))
                         (x
                             (error "Invalid array expression: ~A" x)))))
            (let*
                ((block-compiler
                    (form-compiler (form)
                        ((type symbol sym)
                            (text (temp-symbol-name sym)))
                        (`(loop-range
                              (ranging ,arg ,min ,max ,delta nil 0 ,@_)
                              ,@body)
                            (multiple-value-bind
                                    (rgv base minv maxv)
                                    (get-range-value min max)
                                (unless (and rgv
                                            (= block-dim
                                                (1+ (/ rgv (abs delta)))))
                                    (error "Bad inner loop dimension: ~A"
                                        (second form)))
                                (text "{~%int ~A = (" arg)
                                (recurse base)
                                (text ") + threadIdx.x*~A;~%" (abs delta))
                                (text "if (")
                                (when minv
                                    (text "~A >= (" arg)
                                    (recurse minv)
                                    (text ")"))
                                (when (and minv maxv)
                                    (text " && "))
                                (when maxv
                                    (text "~A <= (" arg)
                                    (recurse maxv)
                                    (text ")"))
                                (text ") {~%")
                                (dolist (stmt body)
                                    (recurse stmt :stmt-p t))
                                (text "}}~%")))
                        (`(loop-range
                              (ranging ,arg ,min ,max ,delta ,_ 0 ,@_)
                              ,@body)
                            (multiple-value-bind
                                    (rgv base minv maxv)
                                    (get-range-value min max)
                                (unless (and rgv
                                            (= block-dim
                                                (1+ (/ rgv (abs delta)))))
                                    (error "Bad inner loop dimension: ~A"
                                        (second form)))
                                (text "__syncthreads();~%")
                                (text "{~%int ~A = (" arg)
                                (recurse base)
                                (text ") + threadIdx.x*~A;~%" (abs delta))
                                (text "int _ix;~%for(_ix = ")
                                (recurse (if (> delta 0) min max))
                                (text "; _ix ~A " (if (> delta 0) "<=" ">="))
                                (recurse (if (> delta 0) max min))
                                (text "; _ix += ~A) {~%" delta)
                                (text "if (~A == _ix) {~%" arg)
                                (dolist (stmt body)
                                    (recurse stmt :stmt-p t))
                                (text "}~%__syncthreads();~%}}~%")))
                        ((when (> level 0)
                            `(loop-range
                                 (ranging ,arg ,min ,max ,delta ,_ ,level ,@_)
                                 ,@body))
                             (text "{~%int ~A;~%for(~A = " arg arg)
                             (recurse (if (> delta 0) min max))
                             (text "; ~A ~A " arg (if (> delta 0) "<=" ">="))
                             (recurse (if (> delta 0) max min))
                             (text "; ~A += ~A) {~%" arg delta)
                             (dolist (cmd body)
                                 (recurse cmd :stmt-p t))
                             (text "}}~%"))))
                 (grid-compiler
                    (form-compiler (form)
                        ((type symbol sym)
                            (text (temp-symbol-name sym)))
                        ;; Convert outer loops to block dimensions
                        ((when (> level 0)
                            `(loop-range
                                 (ranging ,arg ,min ,max ,step nil ,level ,@_)
                                  ,@body))
                            (push `(1+ (floor (- ,(get-full-expr max)
                                                  ,(get-full-expr min))
                                           ,step))
                                dims)
                            (text "{~%int ~A = (" (symbol-name arg))
                            (recurse min)
                            (text ") + ~A*~A;~%"
                                (ecase (length dims)
                                    (1 "blockIdx.x")
                                    (2 "blockIdx.y"))
                                step)
                            (if (or (/= (length body) 1)
                                    (>= (length dims) 2))
                                (dolist (stmt body)
                                    (recurse stmt
                                        :use-stack
                                        (list block-compiler
                                              #'compile-generic-c
                                              #'compile-generic
                                              #'compile-c-inline-temps)
                                        :stmt-p t))
                                (recurse (first body) :stmt-p t))
                            (text "}~%"))
                        ;; Otherwise jump to the in-block level
                        (`(loop-range ,@_)
                            (recurse form
                                :use-stack
                                (list block-compiler
                                      #'compile-generic-c
                                      #'compile-generic
                                      #'compile-c-inline-temps)
                                :stmt-p t))))
                 (args-compiler
                    (form-compiler (form)
                        ((type symbol sym)
                            (text
                                (if (get sym 'let-clause)
                                    (temp-symbol-name sym)
                                    (progn
                                        (ref-arg (symbol-name sym) sym)
                                        (symbol-name sym)))))
                        ;; Array pointers come from linear buffers
                        (`(setf-tmp ,var (arr-ptr ,arr))
                            (ref-array (temp-symbol-name var) arr))
                        (`(setf-tmp ,var ,(as dim `(arr-dim ,arr)))
                            (ref-arg (temp-symbol-name var) dim))
                        (`(arr-ptr ,arr)
                            (let ((sym (gensym "ARR")))
                                (setf (get sym 'let-clause) t)
                                (ref-array (temp-symbol-name sym) arr)
                                (text (temp-symbol-name sym))))
                        (`(arr-dim ,arr ,idx)
                            (let ((sym (gensym "DIM")))
                                (setf (get sym 'let-clause) t)
                                (ref-arg (temp-symbol-name sym) form)
                                (text (temp-symbol-name sym))))
                        ;; Array variables are skipped
                        ((when (eql (gethash expr types) 'array)
                             `(setf-tmp ,_ ,expr))
                            nil)
                        ;; Float and integer variables are converted into args
                        ((when (and
                                   (find (gethash expr types)
                                       '(float integer))
                                   (not (has-atom (get-full-expr expr)
                                            'arr-ptr)))
                             `(setf-tmp ,var ,expr))
                            (let ((name (temp-symbol-name var)))
                                (ref-arg name expr)
                                (when (> (or (get var 'fdiv-users) 0) 1)
                                    (ref-arg (format nil "~A_fdiv" name)
                                        `(/ 1.0 ,expr) 'float))))
                        ;; Temporaries are allocated in shared memory
                        ((when (every #'numberp dims)
                            `(setf-tmp ,var (temporary ',name ,dims ,@_)))
                            (let* ((const-dim (if dims (reduce #'* dims))))
                                (if (and const-dim (> const-dim 1024))
                                    (error "Temporary is too big: ~A" form))
                                (text "__shared__ float ~A"
                                    (temp-symbol-name var))
                                (when const-dim
                                    (text "[~A]" const-dim))
                                (text ";~%")))
                        ;; Shifted buffer ptrs
                        ((when (eql (gethash expr types) 'float-ptr)
                             `(setf-tmp ,var ,expr))
                            (text "__shared__ float *~A;~%"
                                (temp-symbol-name var))
                            (text "if (threadIdx.x == 0) { ~A = ("
                                (temp-symbol-name var))
                            (setf init-calcs t)
                            (recurse expr)
                            (text "); }~%"))
                        ((when (eql (gethash expr types) 'float)
                             `(setf-tmp ,var ,expr))
                            (text "__shared__ float ~A;~%"
                                (temp-symbol-name var))
                            (when (> (or (get var 'fdiv-users) 0) 1)
                                (text "__shared__ float ~A_fdiv;~%"
                                    (temp-symbol-name var)))
                            (text "if (threadIdx.x == 0) { ~A = ("
                                (temp-symbol-name var))
                            (setf init-calcs t)
                            (recurse expr)
                            (text "); ")
                            (when (> (or (get var 'fdiv-users) 0) 1)
                                (text "~A_fdiv = 1.0f/~A; "
                                    (temp-symbol-name var)
                                    (temp-symbol-name var)))
                            (text "}~%"))
                        ;; Loops switch to the next mode
                        (`(loop-range ,@_)
                            (when top-found
                                (error "Multiple top-level loops"))
                            (setf top-found t)
                            (when init-calcs
                                (text "__syncthreads();~%"))
                            (recurse form
                                :use-stack
                                (list grid-compiler
                                      #'compile-generic-c
                                      #'compile-generic
                                      #'compile-c-inline-temps)
                                :stmt-p t))))

                 (*cg-type-table* types)
                 (*cg-full-expr* full_expr)
                 (code (call-form-compilers
                           (list args-compiler
                                 #'compile-generic-c
                                 #'compile-generic)
                           full_expr :stmt-p t)))
                 `(cuda:kernel
                      ,(nreverse args)
                      ,code
                      :grid-size ,(subseq
                                      (concatenate 'list
                                          (nreverse dims) '(1 1))
                                      0 2)
                      :block-size (,block-dim 1 1))))))

(defun do-make-cuda-compute (original name idxspec expr
                                &key with where carrying parallel cluster-cache)
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*consistency-checks* (make-hash-table :test #'equal))
           (*loop-cluster-size* 64))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with
                    where carrying cluster-cache
                    :force-cluster t)
            (let* ((nomacro-expr (expand-macros loop-expr))
                   (nolet-expr   (expand-let nomacro-expr))
                   (noiref-expr (simplify-iref nolet-expr))
                   (ref-list    (collect-arefs noiref-expr))
                  ; (opt-expr    (optimize-tree noiref-expr))
                   (noaref-expr (expand-aref noiref-expr)))
                (let ((c-levels (remove nil (get-check-level-set))))
                    (unless (null c-levels)
                        (error "Safety checks not supported by CUDA:~%  ~A"
                            (mapcan #'get-checks-for-level c-levels))))
                (wrap-compute-sync-data :cuda-device ref-list
                    `(let ((*current-compute* ',original))
                         ,(insert-checks nil)
                         ,(compile-expr-cuda *loop-cluster-size*
                              (code-motion noaref-expr :pull-symbols t))))))))
