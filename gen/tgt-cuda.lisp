;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defvar *realign-cuda-reads* nil)

(defun replace-dim (expr old-expr)
  (match expr
    (`(arr-dim (multivalue-data ,mv ,@_) ,idx ,rank)
      (if (= idx (1- rank))
          `(the fixnum
             (/ (the fixnum
                  (cuda:linear-pitch (multivalue-cuda-buffer ,mv)))
                4))
          `(array-dimension (multivalue-data-array ,mv)
                            ,idx)))
    (`(arr-dim ,@_)
      (error "Bad dimension: ~A" expr))))

(defvar *cg-shared-setfs* nil)

(defmacro flush-shared-setfs (body)
  `(when *cg-shared-setfs*
     (let* ((setfs *cg-shared-setfs*)
            (body-syms (get-symbol-set ,body)))
       (setf *cg-shared-setfs* nil)
       (dolist (sv (nreverse setfs))
         (recurse sv :base-struct-p body-syms))
       (text "if (threadIdx.x == 0) {~%")
       (dolist (sv (nreverse *cg-shared-setfs*))
         (recurse sv :stmt-p t))
       (setf *cg-shared-setfs* nil)
       (setf *cg-shared-body* nil)
       (text "}~%__syncthreads();~%"))))

(defun get-cuda-type-string (expr)
  (let ((var-type (gethash expr *cg-type-table*)))
    (match var-type
      ('float "float")
      ('float-ptr "float*")
      ('integer "int")
      ('boolean "int")
      (_ (error "Bad type ~A of ~A in ~A"
                var-type expr *cg-full-expr*)))))

(def-form-compiler compile-shared-temps (form base-struct-p)
  ((when base-struct-p
     `(let* ,assns ,@body))
    (dolist (assn assns)
      (recurse `(setf-tmp ,(first assn) ,(second assn))
               :stmt-p t :base-struct-p t))
    (dolist (cmd body)
      (recurse cmd :stmt-p t :base-struct-p t)))

  ((when (and (hash-table-p base-struct-p)
              (gethash var base-struct-p))
     `(setf-tmp ,var ,expr))
    (let ((var-name (temp-symbol-name var))
          (var-fdiv (or (get var 'fdiv-users) 0)))
      (text "__shared__ ~A ~A;~%"
            (get-cuda-type-string expr) var-name)
      (push `(setf ,var ,expr) *cg-shared-setfs*)
      (when (> var-fdiv 1)
        (let ((sym (make-symbol
                    (format nil "~A_fdiv" var)))
              (dexpr `(/ ,var)))
          (setf (get sym 'let-clause) t)
          (setf (gethash dexpr *cg-type-table*) 'float)
          (text "__shared__ float ~A_fdiv;~%" var-name)
          (push `(setf ,sym ,dexpr) *cg-shared-setfs*)))))

  ((when base-struct-p
     `(,(or 'setf 'setf-tmp 'inline-strs) ,@_))
    (push form *cg-shared-setfs*))

  (`(setf-tmp ,var ,expr)
    (let ((var-name (temp-symbol-name var))
          (var-fdiv (or (get var 'fdiv-users) 0)))
      (text "~A ~A = ("
            (get-cuda-type-string expr) var-name)
      (code expr ");~%")
      (when (> var-fdiv 1)
        (text "float ~A_fdiv = (1.0f/~A);~%"
              var-name var-name))))

  ((when base-struct-p
     `(progn ,@body))
    (dolist (cmd body)
      (recurse cmd :stmt-p t :base-struct-p t)))

  ((when base-struct-p _)
    (error "Invalid base structure entry: ~A" form)))

(defun compile-expr-cuda (kernel-flags block-dim spill-to-shared
                          range-list full_expr)
  (let ((types (derive-types full_expr))
        ;; Limit on the dimensions usable for blocks.
        ;; Break on ordered loops or static limit of 2.
        (max-dims (do ((cnt 0 (1+ cnt))
                       (rlst range-list (rest rlst)))
                      ((or (>= cnt 2)
                           (ranging-order-flag (first rlst)))
                       cnt)))
        (args  ())
        (top-found nil)
        (needs-scratch nil)
        (dims  ())
        (temp-handler (if spill-to-shared
                          #'compile-shared-temps
                          #'compile-c-inline-temps)))
    (labels ((ref-arg (name expr &optional etype)
               (let ((sym-type (or etype
                                   (gethash expr types))))
                 (push (list (ecase sym-type
                               (float :float)
                               (integer :int))
                             name
                             (simplify-rec-once #'replace-dim
                                                (get-full-expr expr)))
                       args)))
             (ref-array (name arr)
               (match (get-full-expr arr)
                 (`(multivalue-data ,mv ,@_)
                   (push (list :float-ptr
                               name
                               `(multivalue-cuda-buffer ,mv))
                         args))
                 (x
                   (error "Invalid array expression: ~A" x)))))
      (let*
          ((align-compiler
            (form-compiler (form stmt-p)
              ((type symbol sym)
                (text (temp-symbol-name sym)))
              ((when *realign-cuda-reads*
                 `(ptr-deref ,ptr))
                (let* ((istep (get-inner-step ptr))
                       (delta (get-inner-delta ptr))
                       (misalignment (mod delta 16)))
                  (if (or (not (eql istep 1))
                          (eql misalignment 0))
                      (code "*(" ptr ")")
                      (progn
                        (setf needs-scratch t)
                        (text "(__scratch_ptr=(")
                        (recurse ptr)
                        (text "),~%(threadIdx.x<~A?" misalignment)
                        (text "__scratch[threadIdx.x+~A]=*(__scratch_ptr+~A):"
                              (- block-dim misalignment) (- block-dim misalignment))
                        (text "__scratch[threadIdx.x-~A]=*(__scratch_ptr-~A)),~%"
                              misalignment misalignment)
                        (text "__syncthreads(),__scratch[threadIdx.x])")))))
              (`(setf (ptr-deref ,ptr) ,expr)
                (code "*(" ptr ")=(" expr ")" (:when stmt-p ";~%")))))
           (block-compiler
            (form-compiler (form)
              ((type symbol sym)
                (text (temp-symbol-name sym)))
              (`(loop-range
                   ,(ranging-spec arg :min min :max max :delta delta
                                  :ordered-p ordered :loop-level 0)
                   ,@body)
                (multiple-value-bind (rgv base minv maxv)
                    (get-range-value min max)
                  (unless (and rgv
                               (= block-dim
                                  (1+ (/ rgv (abs delta)))))
                    (error "Bad inner loop dimension ~A: ~A ~A" rgv
                           (get-full-expr (second form)) (second form)))
                  (when ordered
                    (code "__syncthreads();~%"))
                  (code ("{~%int ~A = (" arg) base (") + threadIdx.x*~A;~%" (abs delta)))
                  (code "if (")
                  (when minv
                    (code ("~A >= (" arg) minv ")"))
                  (when (and minv maxv)
                    (code " && "))
                  (when maxv
                    (code ("~A <= (" arg) maxv ")"))
                  (code ") {~%")
                  (if ordered
                      (multiple-value-bind (preamble inner escape-vars final)
                          (make-flattened-loop `(progn ,@body) *cg-type-table*)
                        (text "/*preamble*/~%")
                        (dolist (stmt preamble)
                          (recurse stmt
                                   :use-stack
                                   (list align-compiler
                                         #'compile-generic-c
                                         #'compile-generic
                                         #'compile-c-inline-temps)
                                   :stmt-p t))
                        (dolist (var escape-vars)
                          (text "~A ~A;~%"
                                (ecase (get-factored-cg-type var)
                                  (float "float")
                                  (integer "int"))
                                (temp-symbol-name var)))
                        (when inner
                          (text "{int _ix;~%for(_ix = ")
                          (recurse (if (> delta 0) min max))
                          (text "; _ix ~A " (if (> delta 0) "<=" ">="))
                          (recurse (if (> delta 0) max min))
                          (text "; _ix += ~A) {~%" delta)
                          (text "if (~A == _ix) {~%" arg)
                          (dolist (stmt inner)
                            (recurse stmt :stmt-p t))
                          (text "}~%__syncthreads();~%}}~%"))
                        (text "/*finalize*/~%")
                        (dolist (stmt final)
                          (recurse stmt
                                   :use-stack
                                   (list align-compiler
                                         #'compile-generic-c
                                         #'compile-generic
                                         #'compile-c-inline-temps)
                                   :stmt-p t)))
                      (let* ((flattened (flatten-inner-loop
                                         `(progn ,@body) *cg-type-table*
                                         :split-all t))
                             (optimized (minimize-live-vars flattened)))
                        (dolist (stmt optimized)
                          (recurse stmt
                                   :use-stack
                                   (list align-compiler
                                         #'compile-generic-c
                                         #'compile-generic
                                         #'compile-c-inline-temps)
                                   :stmt-p t))))
                  (text "}}~%")))
              ((when (> level 0)
                 `(loop-range
                     ,(ranging-spec arg :min min :max max :delta delta
                                    :loop-level level)
                     ,@body))
                (text "{~%int ~A;~%for(~A = " arg arg)
                (recurse (if (> delta 0) min max))
                (text "; ~A ~A " arg (if (> delta 0) "<=" ">="))
                (recurse (if (> delta 0) max min))
                (text "; ~A += ~A) {~%" arg delta)
                (dolist (cmd body)
                  (recurse cmd :stmt-p t :base-struct-p t))
                (text "}}~%"))))
           (arr-arg-compiler
            (form-compiler (form)
              ;; Array pointers come from linear buffers
              (`(arr-ptr ,arr)
                (let ((sym (get-new-symbol :stem "ARR")))
                  (setf (get sym 'let-clause) t)
                  (ref-array (temp-symbol-name sym) arr)
                  (text (temp-symbol-name sym))))
              (`(arr-dim ,_ ,_ ,_)
                (let ((sym (get-new-symbol :stem "DIM")))
                  (setf (get sym 'let-clause) t)
                  (ref-arg (temp-symbol-name sym) form)
                  (text (temp-symbol-name sym))))))
           (grid-compiler
            (form-compiler (form)
              ((type symbol sym)
                (text (temp-symbol-name sym)))
              ;; Convert outer loops to block dimensions
              ((when (and (< (length dims) max-dims)
                          (> level 0))
                 `(loop-range
                     ,(ranging-spec arg :min min :max max :delta step
                                    :ordered-p nil :loop-level level)
                     ,@body))
                (push `(1+ (floor (- ,(get-full-expr max)
                                     ,(get-full-expr min))
                                  ,step))
                      dims)
                (let ((instmt
                       `(inline-strs ,(symbol-name arg)
                                     " = (" ,min ") + "
                                     ,(format nil "~A*~A;~%"
                                              (ecase (length dims)
                                                (1 "blockIdx.x")
                                                (2 "blockIdx.y"))
                                              step))))
                  (if (and spill-to-shared
                           (or *cg-shared-setfs*
                               (eql (first (first body)) 'let*)))
                      (progn
                        (text "__shared__ int ~A;~%"
                              (symbol-name arg))
                        (push instmt *cg-shared-setfs*))
                      (progn
                        (text "int ")
                        (recurse instmt))))
                (dolist (stmt body)
                  (recurse stmt
                           :stmt-p t :base-struct-p t))
                (flush-shared-setfs nil))
              ;; Otherwise jump to the in-block level
              (`(loop-range ,@_)
                (flush-shared-setfs form)
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
                (text (if (get sym 'let-clause)
                          (temp-symbol-name sym)
                          (progn
                            (ref-arg (symbol-name sym) sym)
                            (symbol-name sym)))))
              ;; Array pointers come from linear buffers
              (`(setf-tmp ,var (arr-ptr ,arr))
                (ref-array (temp-symbol-name var) arr))
              (`(setf-tmp ,var ,(as dim `(arr-dim ,_ ,_ ,_)))
                (ref-arg (temp-symbol-name var) dim))
              ;; Temporaries are allocated in shared memory,
              ;; unless they are explicitly localized.
              (`(setf-tmp ,var (temporary ',name nil ,_ :local))
                (text "float ~A;~%"
                      (temp-symbol-name var)))
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
              ;; Array variables are skipped
              ((when (eql (gethash expr types) 'array)
                 `(setf-tmp ,_ ,expr))
                nil)
              ;; Float and integer variables are converted into args
              ((when (and (find (gethash expr types)
                                '(float integer))
                          (not (has-atom (get-full-expr expr)
                                         'arr-ptr 'texture-ref
                                         'texture-ref-int)))
                 `(setf-tmp ,var ,expr))
                (let ((name (temp-symbol-name var)))
                  (ref-arg name expr)
                  (when (> (or (get var 'fdiv-users) 0) 1)
                    (ref-arg (format nil "~A_fdiv" name)
                             `(/ 1.0 ,expr) 'float))))
              ;; Loops switch to the next mode
              (`(loop-range ,@_)
                (when top-found
                  (error "Multiple top-level loops"))
                (setf top-found t)
                (recurse form
                         :use-stack
                         (list grid-compiler
                               arr-arg-compiler
                               temp-handler
                               #'compile-generic-c
                               #'compile-generic
                               #'compile-c-inline-temps)
                         :stmt-p t
                         :base-struct-p t))))

           (*cg-type-table* types)
           (*cg-full-expr* full_expr)
           (*cg-shared-setfs* nil)
           (code (call-form-compilers (list args-compiler
                                            arr-arg-compiler
                                            temp-handler
                                            #'compile-generic-c
                                            #'compile-generic)
                                      full_expr
                                      :stmt-p t
                                      :base-struct-p t)))
        (when needs-scratch
          (setf code
                (format nil "float *__scratch_ptr;~%__shared__ float __scratch[~A];~%~A"
                        block-dim code)))
        `(cuda:kernel ,(nreverse args)
                      ,code
                      ,@kernel-flags
                      :grid-size ,(subseq (concatenate 'list
                                                       (nreverse dims) '(1 1))
                                          0 2)
                      :block-size (,block-dim 1 1))))))

(defun do-make-cuda-compute (original name idxspec expr
                             &key with where carrying parallel precompute cuda-flags)
  (destructuring-bind (&key (kernel-name (format nil "compute_~A" name))
                            (block-size 128)
                            (max-registers nil)
                            (textures nil)
                            (spill-to-shared t))
      cuda-flags
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*canonify-cache* (make-canonify-cache))
           (*consistency-checks* (make-hash-table :test #'equal))
           (*loop-cluster-size* block-size)
           (*align-cluster* 16))
      (multiple-value-bind (loop-expr loop-list range-list)
          (make-compute-loops name idxspec expr with
                              where carrying precompute
                              :force-cluster t)
        ;; Apply optimizations
        (let* ((noiref-expr (pipeline loop-expr
                              expand-macros expand-let make-canonic
                              simplify-iref))
               ;; A table of all array references
               (ref-list    (collect-arefs noiref-expr))
               ;; Convert temporary arrays to registers where possible
               (ltemp-expr  (localize-temps noiref-expr ref-list range-list)))
          (nlet (;; Use textures where requested
                 (tex-expr tex-list (use-textures (convert 'set textures) ltemp-expr))
                 ;; Apply final transformations
                 ((res-expr (pipeline tex-expr
                              expand-aref optimize-tree
                              (code-motion _ :pull-symbols t)))
                  ;; Inner check levels
                  (c-levels (remove nil (get-check-level-set)))))
            ;; Only top-level safety checks
            (unless (null c-levels)
              (error "Safety checks not supported by CUDA:~%  ~A"
                     (mapcan #'get-checks-for-level c-levels)))
            ;; Generate the kernel call
            (wrap-compute-sync-data :cuda-device ref-list
              `(let ((*current-compute* ',original))
                 ,(insert-checks nil)
                 ,(compile-expr-cuda (cond-list (t :name kernel-name)
                                                (tex-list :textures tex-list)
                                                (max-registers :max-registers max-registers))
                                     *loop-cluster-size* spill-to-shared
                                     range-list res-expr)))))))))
