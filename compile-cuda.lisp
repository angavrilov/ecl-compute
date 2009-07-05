;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun replace-dim (expr old-expr)
    (match expr
        (`(arr-dim (multivalue-data ,mv ,@_) ,idx ,rank)
            (if (= idx (1- rank))
               `(/ (cuda:linear-pitch
                       (multivalue-cuda-buffer ,mv))
                    4)
               `(array-dimension
                    (multivalue-data-array ,mv)
                    ,idx)))
        (`(arr-dim ,@_)
            (error "Bad dimension: ~A" expr))))

(defun has-atom (tree sym)
    (if (and tree (consp tree))
        (reduce #'(lambda (a b) (or a b))
            (mapcar #'(lambda (x) (has-atom x sym))
                tree))
        (eql tree sym)))

(defun get-ref-root (expr)
    (match expr
        ((type symbol sym)
            sym)
        (`(multivalue-data ,@_)
            expr)
        (`(temporary ,@_)
            expr)
        (`(tmp-ref ,tmp)
            (recurse-factored #'get-ref-root tmp))
        (`(ptr-deref ,ptr)
            (recurse-factored #'get-ref-root ptr))
        (`(ptr+ ,ptr ,_)
            (recurse-factored #'get-ref-root ptr))
        (`(arr-ptr ,arr)
            (recurse-factored #'get-ref-root arr))
        (_
            (error "Invalid reference: ~A" expr))))

(defun collect-refs (lvals rvals expr)
    (match expr
        (`(setf ,ref ,rhs)
            (setf (gethash (get-ref-root ref) lvals) t)
            (collect-refs lvals rvals rhs))
        ((or
             (type symbol e)
             `(tmp-ref ,e)
             `(ptr-deref ,e))
            (setf (gethash (get-ref-root expr) rvals) t)
            (when (consp e)
                (collect-refs lvals rvals e)))
        ((type list e)
            (dolist (sub e)
                (collect-refs lvals rvals sub)))))

(defun mark-carried-low (expr low-table crefs carried-p)
    (multiple-value-bind
            (cur-carried found)
            (gethash expr low-table)
        (when (or (not found)
                  (and carried-p (not cur-carried)))
            (setf (gethash expr low-table) carried-p)
            (match expr
                (`(setf ,ref ,rhs)
                    (let ((new-c
                              (gethash (get-ref-root ref) crefs)))
                        (when new-c
                            (setf (gethash expr low-table) t))
                        (recurse-factored #'mark-carried-low
                            ref low-table crefs (or new-c carried-p))
                        (recurse-factored #'mark-carried-low
                            rhs low-table crefs (or new-c carried-p))))
                ((type list e)
                    (dolist (sub e)
                        (recurse-factored #'mark-carried-low
                            sub low-table crefs carried-p)))))))

(defun mark-carried-high (expr high-table crefs)
    (use-cache (expr high-table)
        (match expr
            ((type symbol e)
                (gethash (get-ref-root e) crefs))
            ((or
                 `(tmp-ref ,e)
                 `(ptr-deref ,e))
                (or
                    (recurse-factored #'mark-carried-high
                        e high-table crefs)
                    (gethash (get-ref-root expr) crefs)))
            ((type list e)
                (some
                    #'identity
                    ;; Force full evaluation
                    (mapcar
                        #'(lambda (sub)
                              (recurse-factored #'mark-carried-high
                                  sub high-table crefs))
                        expr))))))

(defun copy-hash-data (new old &rest tables)
    (unless (eql new old)
        (dolist (tbl tables)
            (multiple-value-bind
                (oval found) (gethash new tbl)
                (unless found
                    (setf (gethash new tbl)
                        (gethash old tbl)))))))

(defun splice-carried (expr types low-table high-table)
    (labels
        ((copy-tags (old new)
             (copy-hash-data new old
                 types low-table high-table)
             new)
         (wrap-let (expr l-esc)
             (let* ((sym (gensym))
                    (clause (list sym expr)))
                 (setf (get sym 'let-clause) clause)
                 (setf (get sym 'lower-escape) l-esc)
                 (copy-hash-data sym expr types)
                 (copy-tags expr
                     `(let* (,clause) ,sym))))
         (wrap-item (item cur-high cur-low)
             (let ((expr (unwrap-factored item)))
                 (cond
                    ((and (not cur-low)
                          (gethash expr low-table))
                         (match item
                             ((when (get item 'let-clause)
                                  (type symbol _))
                                 (setf (get item 'lower-escape) t)
                                 item)
                             (`(ranging ,@_)
                                 item)
                             ((type list _)
                                 (wrap-let item t))
                             (_
                                 item)))
                    ((and cur-high
                        (not (gethash expr high-table)))
                         (match item
                             (`(ranging ,@_)
                                 item)
                             ((type list _)
                                 (wrap-let item nil))
                             (_
                                 item)))
                    (t
                        item)))))
        (simplify-rec-once-struct
            #'(lambda (expr old-expr)
                  (copy-tags old-expr
                      (let ((cur-high
                                (gethash old-expr high-table))
                            (cur-low
                                (gethash old-expr low-table)))
                          (match expr
                              (`(let* ,assns ,@code)
                                  `(let* ,assns ,@(butlast code)
                                       ,(wrap-item (car (last code))
                                            cur-high cur-low)))
                              (`(progn ,@code)
                                  `(progn
                                       ,@(butlast code)
                                       ,(wrap-item (car (last code))
                                            cur-high cur-low)))
                              (`(setf ,lhs ,rhs)
                                  `(setf ,lhs
                                       ,(wrap-item rhs cur-high cur-low)))
                              ((type list _)
                                  (mapcar-save-old
                                      #'(lambda (item)
                                            (wrap-item item cur-high cur-low))
                                      expr))
                              (_
                                  expr)))))
            expr)))

(defun dump-tbl (name hash)
    (format t "~%~%~A:~%" name)
    (maphash
        #'(lambda (k v)
              (when v
                  (print k)))
        hash))

(defun splice-inner-loop (expr types)
    (let* ((lvals (make-hash-table))
           (rvals (make-hash-table))
           (low-table (make-hash-table))
           (high-table (make-hash-table)))
        (collect-refs lvals rvals expr)
        (maphash
            #'(lambda (r v)
                  (unless (gethash r rvals)
                      (remhash r lvals)))
            lvals)
        (mark-carried-low expr
            low-table lvals nil)
        (mark-carried-high expr
            high-table lvals)
        (values
            (splice-carried expr types
                low-table high-table)
            low-table high-table)))

(defun flatten-inner-loop (expr low-table high-table types)
    (let* ((preamble nil)
           (inner nil)
           (out-vars nil)
           (finalize nil))
        (labels
            ((copy-tags (old new)
                 (copy-hash-data new old
                     types low-table high-table)
                 new)
             (dispatch-assn (var expr)
                 (cond
                     ((not (gethash expr high-table))
                         (push `(setf-tmp ,var ,expr) preamble))
                     ((and
                          (get var 'lower-escape)
                          (gethash expr low-table))
                         (push var out-vars)
                         (push `(setf ,var ,expr) inner))
                     ((gethash expr low-table)
                         (push `(setf-tmp ,var ,expr) inner))
                     (t
                         (push `(setf-tmp ,var ,expr) finalize))))
             (dispatch (stmt)
                 (cond
                     ((atom stmt) nil)
                     ((not (gethash stmt high-table))
                         (push stmt preamble))
                     ((gethash stmt low-table)
                         (push stmt inner))
                     (t
                         (push stmt finalize))))
             (unwrap (expr &optional (evals t))
                 (copy-tags expr
                     (match expr
                         ((type atom _)
                             expr)
                         (`(progn ,@code)
                             (dolist (stmt (butlast code))
                                 (dispatch (unwrap stmt nil)))
                             (unwrap (car (last code)) evals))
                         ((when (not evals)
                             `(let* ((,x ,v)) ,x))
                             (unwrap v nil))
                         (`(let* ,assns ,@code)
                             (dolist (assn assns)
                                 (dispatch-assn
                                     (first assn)
                                     (unwrap (second assn))))
                             (dolist (stmt (butlast code))
                                 (dispatch (unwrap stmt nil)))
                             (unwrap (car (last code)) evals))
                         (_
                             (mapcar-save-old #'unwrap expr))))))
            (dispatch (unwrap expr nil))
            (values
                (nreverse preamble)
                (nreverse inner)
                out-vars
                (nreverse finalize)))))

(defun make-flattened-loop (expr types)
    (multiple-value-call
        #'flatten-inner-loop
        (splice-inner-loop expr types)
        types))

(defvar *cg-shared-setfs* nil)

(defmacro flush-shared-setfs ()
   `(when *cg-shared-setfs*
        (text "if (threadIdx.x == 0) {~%")
        (dolist (sv (nreverse *cg-shared-setfs*))
            (recurse sv :stmt-p t))
        (text "}~%__syncthreads();~%")
        (setf *cg-shared-setfs* nil)))

(def-form-compiler compile-shared-temps (form stmt-p)
    (`(let* ,assns ,@body)
        (unless stmt-p
            (error "Let in a non-stmt context: ~A" form))
        (text "{~%")
        (dolist (assn assns)
            (recurse
                `(setf-tmp ,(first assn) ,(second assn))))
        (flush-shared-setfs)
        (dolist (cmd body)
            (recurse cmd :stmt-p t))
        (text "}~%"))

    (`(setf-tmp ,var ,expr)
        (let ((var-type (gethash expr *cg-type-table*))
              (var-name (temp-symbol-name var))
              (var-fdiv (or (get var 'fdiv-users) 0)))
            (text "__shared__ ~A ~A;~%"
                (match var-type
                    ('float "float")
                    ('float-ptr "float*")
                    ('integer "int")
                    ('boolean "int")
                    (_ (error "Bad type ~A of ~A in ~A"
                           var-type expr *cg-full-expr*)))
                var-name)
            (push `(setf ,var ,expr) *cg-shared-setfs*)
            (when (> var-fdiv 1)
                (let ((sym (make-symbol
                               (format nil "~A_fdiv" var)))
                      (dexpr `(/ ,var)))
                    (setf (get sym 'let-clause) t)
                    (setf (gethash dexpr *cg-type-table*) 'float)
                    (text "__shared__ float ~A_fdiv;~%" var-name)
                    (push `(setf ,sym ,dexpr) *cg-shared-setfs*))))))

(defun compile-expr-cuda (name block-dim full_expr)
    (let ((types (derive-types full_expr))
          (args  ())
          (top-found nil)
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
                              (ranging ,arg ,min ,max ,delta ,ordered 0 ,@_)
                              ,@body)
                            (multiple-value-bind
                                    (rgv base minv maxv)
                                    (get-range-value min max)
                                (unless (and rgv
                                            (= block-dim
                                                (1+ (/ rgv (abs delta)))))
                                    (error "Bad inner loop dimension ~A: ~A ~A" rgv
                                        (get-full-expr (second form)) (second form)))
                                (when ordered
                                    (text "__syncthreads();~%"))
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
                                (if ordered
                                    (multiple-value-bind
                                            (preamble inner escape-vars final)
                                            (make-flattened-loop
                                                `(progn ,@body) *cg-type-table*)
                                        (text "/*preamble*/~%")
                                        (dolist (stmt preamble)
                                            (recurse stmt :stmt-p t))
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
                                            (recurse stmt :stmt-p t)))
                                    (dolist (stmt body)
                                        (recurse stmt :stmt-p t)))
                                (text "}}~%")))
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
                        ((when (and (< (length dims) 2)
                                    (> level 0))
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
                            (if (/= (length body) 1)
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
                        (`(setf-tmp ,var ,(as dim `(arr-dim ,_ ,_ ,_)))
                            (ref-arg (temp-symbol-name var) dim))
                        (`(arr-ptr ,arr)
                            (let ((sym (gensym "ARR")))
                                (setf (get sym 'let-clause) t)
                                (ref-array (temp-symbol-name sym) arr)
                                (text (temp-symbol-name sym))))
                        (`(arr-dim ,_ ,_ ,_)
                            (let ((sym (gensym "DIM")))
                                (setf (get sym 'let-clause) t)
                                (ref-arg (temp-symbol-name sym) form)
                                (text (temp-symbol-name sym))))
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
                        ;; Loops switch to the next mode
                        (`(loop-range ,@_)
                            (when top-found
                                (error "Multiple top-level loops"))
                            (setf top-found t)
                            (recurse form
                                :use-stack
                                (list grid-compiler
                                      #'compile-shared-temps
                                      #'compile-generic-c
                                      #'compile-generic
                                      #'compile-c-inline-temps)
                                :stmt-p t))))

                 (*cg-type-table* types)
                 (*cg-full-expr* full_expr)
                 (*cg-shared-setfs* nil)
                 (code (call-form-compilers
                           (list args-compiler
                                 #'compile-shared-temps
                                 #'compile-generic-c
                                 #'compile-generic)
                           full_expr :stmt-p t)))
                 `(cuda:kernel
                      ,(nreverse args)
                      ,code
                      :name ,name
                      :grid-size ,(subseq
                                      (concatenate 'list
                                          (nreverse dims) '(1 1))
                                      0 2)
                      :block-size (,block-dim 1 1))))))

(defun localize-temps (expr ref-list range-list)
    (let* ((inner-index
               (second (car (last range-list))))
           (cluster-level
               (mapcar #'ranging-loop-level
                   (remove-if-not
                       #'(lambda (rg)
                             (and
                                 (eql inner-index
                                     (get (second rg) 'band-master))
                                 (get (second rg) 'is-cluster)))
                       range-list)))
           (local-temps
              (remove-if-not
                  #'(lambda (ref-entry)
                        (ifmatch
                                `(temporary ,_ ,dims 0 ,@_)
                                (first ref-entry)
                            (and
                                (= (length dims) 1)
                                (null
                                    (set-difference
                                        (third ref-entry) (second ref-entry)
                                        :test #'equal))
                                (every
                                    #'(lambda (x)
                                          (and
                                              (= (length x) 1)
                                              (equal
                                                  (set-difference
                                                      (get-loop-levels (first x))
                                                      cluster-level)
                                                  '(0))))
                                    (append
                                        (third ref-entry) (second ref-entry))))))
                  ref-list))
           (replacement-tbl
               (make-hash-table :test #'equal)))
        (if (null local-temps)
            expr
            (progn
                (dolist (ref-entry local-temps)
                    (setf
                        (gethash
                            (first ref-entry) replacement-tbl)
                        `(temporary
                             ,(second (first ref-entry))
                             nil 0 :local)))
                (simplify-rec-once
                    #'(lambda (form old-form)
                          (match form
                              ((when (gethash temp replacement-tbl)
                                  `(tmp-ref ,temp ,@_))
                                  `(tmp-ref ,(gethash temp replacement-tbl)))))
                    expr)))))

(defun do-make-cuda-compute (original name idxspec expr
                                &key with where carrying parallel cluster-cache)
    (let* ((*current-compute* original)
           (*simplify-cache* (make-hash-table))
           (*range-cache* (make-hash-table))
           (*minlevel-cache* (make-hash-table))
           (*consistency-checks* (make-hash-table :test #'equal))
           (*loop-cluster-size* 64)
           (*align-cluster* 16))
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
                   (ltemp-expr (localize-temps noiref-expr ref-list range-list))
                   (noaref-expr (expand-aref ltemp-expr)))
                (let ((c-levels (remove nil (get-check-level-set))))
                    (unless (null c-levels)
                        (error "Safety checks not supported by CUDA:~%  ~A"
                            (mapcan #'get-checks-for-level c-levels))))
                (wrap-compute-sync-data :cuda-device ref-list
                    `(let ((*current-compute* ',original))
                         ,(insert-checks nil)
                         ,(compile-expr-cuda
                              (format nil "compute_~A" name)
                              *loop-cluster-size*
                              (code-motion noaref-expr :pull-symbols t))))))))
