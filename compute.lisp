;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *current-compute* nil)
(defparameter *current-compute-body* nil)

(defun get-index-var (idx-spec)
    (if (or (atom idx-spec)
            (index-expr-p idx-spec))
        idx-spec
        (car idx-spec)))

(defun get-iter-spec (spec-pair)
    (let ((idx-name (caar spec-pair))
          (idx-spec (cdr spec-pair)))
        (if (index-expr-p idx-spec)
            nil
            (list
                (cons idx-name
                    (if (atom idx-spec)
                        (list idx-spec)
                        idx-spec))))))

(defparameter *consistency-checks* nil)

(defun check-index-alignment (expr iref-expr aref-expr)
    (match expr
        (`(ranging ,_ ,minv ,maxv ,step ,@_)
            (cons step
                (or (force-integer
                        (simplify-index `(rem ,minv ,step)))
                    (force-integer
                        (simplify-index `(rem ,maxv ,step))))))
        (`(* ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (* (or (car alignment) 0) b)
                      (* (or (cdr alignment) 1) b))))
        (`(+ ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (car alignment)
                      (if (cdr alignment) (+ (cdr alignment) b)))))
        (`(- ,a ,(type number b))
            (let ((alignment (check-index-alignment a iref-expr aref-expr)))
                (cons (car alignment)
                      (if (cdr alignment) (- (cdr alignment) b)))))
        (`(/ ,a ,(type number b))
            (let* ((alignment (check-index-alignment a iref-expr aref-expr))
                   (step-ref  (/ (or (car alignment) 0) b))
                   (ofs-ref   (/ (or (cdr alignment) 1) b)))
                (unless (integerp step-ref)
                    (error "Too dense index iteration: ~A (~A,~A)~% in ~A~% orig ~A~%"
                        (remove-ranges expr) step-ref ofs-ref
                        (remove-ranges iref-expr)
                        (remove-ranges aref-expr)))
                (unless (integerp ofs-ref)
                    (if (/= step-ref 0)
                        (error "Misaligned reference: ~A (~A,~A)~% in ~A~% orig ~A~%"
                            (remove-ranges expr) step-ref ofs-ref
                            (remove-ranges iref-expr)
                            (remove-ranges aref-expr))
                        (when (or (null *consistency-checks*)
                                  (not (null (min-loop-level expr)))
                                  (= 1 (incf-nil
                                               (gethash `(= (rem ,expr ,b) 0)
                                                   *consistency-checks*))))
                            (format t
                                "~%Possibly misaligned reference: ~A (~A,~A)~% in ~A~% orig ~A~%"
                                (remove-ranges expr) step-ref ofs-ref
                                (remove-ranges aref-expr)
                                (remove-ranges iref-expr)))))
                (cons (force-integer step-ref)
                      (force-integer ofs-ref))))
        (`(,_ ,@rest)
            (dolist (arg rest) (check-index-alignment arg iref-expr aref-expr))
            nil)))

(defun simplify-iref-1 (expr old-expr)
    (match expr
        (`(iref ,name ,@idxvals)
            (multiple-value-bind
                (rexpr mv-p checks) (expand-iref name idxvals :verbose-p *consistency-checks*)
                (unless mv-p
                    (error "Not a multivalue reference: ~A" expr))
                (check-index-alignment rexpr expr rexpr)
                (when *consistency-checks*
                    ;; Remember bound consistency checks
                    (dolist (check checks)
                        (incf-nil (gethash check *consistency-checks*)))
                    ;; Create dimension consistency checks
                    (do ((dims (get name 'mv-dimensions) (cdr dims))
                         (idx 0 (1+ idx)))
                        ((null dims) nil)
                        (incf-nil (gethash `(<= ,(car dims) (arr-dim ,name ,idx))
                                      *consistency-checks*))))
                ;; Return the expression
                rexpr))
        (_ nil)))

(defun simplify-iref (expr)
    (simplify-rec-once #'simplify-iref-1 expr))

(defun get-checks-for-level (level)
    (let ((clst nil))
        (maphash
            #'(lambda (expr cnt)
                  (when (eql (min-loop-level expr) level)
                      (push (list expr expr) clst)))
            *consistency-checks*)
        clst))

(defmacro safety-check (checks &body body)
    (let ((check-code (mapcar
                           #'(lambda (expr)
                                 `(unless ,(first expr)
                                      (error "Safety check failed: ~A" (quote ,(second expr)))))
                           checks)))
        `(progn ,@check-code ,@body)))

(defun insert-checks-1 (expr old-expr)
    (match expr
        (`(loop-range ,range ,@body)
            (let ((checks (get-checks-for-level (ranging-loop-level range))))
                (if checks
                    `(loop-range ,range (safety-check ,checks ,@body)))))
        (_ nil)))

(defun insert-checks (expr)
    (let ((new-expr (simplify-rec-once #'insert-checks-1 expr))
          (checks   (get-checks-for-level nil)))
        (if checks
            `(safety-check ,checks ,new-expr)
            new-expr)))

(defun wrap-compute-parallel (parallel loop-list code &optional (gen-func #'identity))
    (if (null parallel)
        (funcall gen-func code)
        (let ((range (if (eql parallel t)
                         (let ((loops1 (remove-if #'ranging-order-flag loop-list)))
                             (when (null loops1)
                                 (error "Cannot find a parallelizable loop: ~A" loop-list))
                             (car loops1))
                         (let ((range (find parallel loop-list :key #'second)))
                             (when (null range)
                                 (error "Cannot find loop ~A in: ~A" parallel loop-list))
                             (when (ranging-order-flag range)
                                 (error "Loop ~A not parallelizable: ~A" parallel loop-list))
                             range))))
            (wrap-parallel range code :gen-func gen-func))))

(defmacro temporary (name dims level)
    (if (null dims)
        0.0
        `(the (array single-float)
             (make-array ,dims
                 :element-type 'single-float
                 :initial-element 0.0))))

(defmacro tmp-ref (temp &rest dims)
    (if (null dims)
        temp
        `(aref ,temp ,@dims)))

(defun range-band-master (range)
    (let ((idx (second range)))
        (or (get idx 'band-master)
            idx)))

(defun prepend-loop-item (rloop entry)
    (setf (cddr rloop)
        (cons entry (cddr rloop))))

(defun append-loop-item (rloop entry)
    (nconc rloop (list entry)))

(defun create-carry (index carrying range-list loop-list with in-with replace-tbl)
    (let* ((pos       (or (position index range-list :key #'second)
                          (error "Invalid carry index: ~A" index)))
           (range     (nth pos range-list))
           ;; Inner dimensions
           (iranges   (nthcdr (1+ pos) range-list))
           (act-ranges (mapcar #'compute-range
                           (remove-if #'(lambda (rg)
                                            (get (second rg) 'is-cluster))
                               iranges)))
           (idims     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(+ (- ,(fourth rg) ,(third rg)) 1)))
                          act-ranges))
           (irefs     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(- ,rg ,(third rg))))
                          act-ranges))
           ;; Modification points
           (out-pos    (position index range-list :key #'range-band-master))
           (out-loop   (nth out-pos loop-list))
           (last-loop  (car (last loop-list)))
           ;; Carry variables
           (carry-list (remove-if-not
                           #'(lambda (ce)
                                 (eql (first ce) index))
                           carrying))
           (init-code (do-wrap-loops
                          (list (wrap-with-let with
                                    (list* 'progn
                                        (mapcar
                                            #'(lambda (iexpr)
                                                  `(setf ,(second iexpr)
                                                         ,(third iexpr)))
                                            carry-list))))
                          iranges replace-tbl))
           (alter-code  (replace-unquoted
                            (wrap-with-let in-with
                                (list* 'progn
                                    (mapcar
                                        #'(lambda (iexpr)
                                              `(setf ,(second iexpr)
                                                     ,(fourth iexpr)))
                                        carry-list)))
                            replace-tbl))
           (name-table (mapcar
                           #'(lambda (iexpr)
                                 (list (second iexpr)
                                     `(tmp-ref
                                          (temporary ',(second iexpr) ,idims 0)
                                          ,@irefs)))
                           carry-list)))
        (unless (ranging-order-flag range)
            (error "Cannot carry over unordered index ~A" index))
        ;; Splice in the new imperative code
        (prepend-loop-item out-loop init-code)
        (append-loop-item last-loop alter-code)
        ;; Return the new names
        name-table))

(defun wrap-symbol-macrolet (with body)
    (if with
        `(symbol-macrolet ,with ,body)
        body))

(defun make-compute-carry (carrying loop-expr loop-list range-list with in-with replace-tbl)
    (let* ((carry-indices (if carrying
                              (reduce #'nunion
                                  (mapcar #'list
                                      (mapcar #'first
                                          carrying)))))
           (carry-body `(progn nil ,loop-expr))
           (carry-table
               (mapcan
                   #'(lambda (idx)
                         (create-carry
                             idx carrying range-list
                             (cons carry-body loop-list)
                             with in-with replace-tbl))
                   carry-indices)))
        (if (null carry-table)
            loop-expr
            (wrap-symbol-macrolet carry-table carry-body))))

(defun cluster-loop (range-list range size)
    (let* ((pos     (position range range-list))
           (index   (second range))
           (minv    (third range))
           (maxv    (fourth range))
           (delta   (fifth range))
           (size    (- size (mod size (abs delta))))
           (clidx   (gensym (symbol-name index)))
           (nrange `(ranging ,clidx ,minv ,maxv
                        ,(if (> delta 0) size (- size))
                        ,(ranging-order-flag range) nil)))
        (setf (get clidx 'band-master) index)
        (setf (get clidx 'is-cluster) t)
        (setf (third range)
            (if (> delta 0) nrange
                `(max ,minv (- ,nrange ,(1- size)))))
        (setf (fourth range)
            (if (< delta 0) nrange
                `(min ,maxv (+ ,nrange ,(1- size)))))
        (values
            (nconc
                (subseq range-list 0 pos)
                (list nrange)
                (subseq range-list pos))
            nrange)))

(defparameter *loop-cluster-size* 1024)

(defun make-cluster-refs (range-list vars replace-tbl with)
    (if (null vars)
        (values range-list with nil nil)
        (let* ((index (car (last range-list)))
               (index-var (second index))
               ;; Cluster the loop (alters ranges)
               (range-list
                   (cluster-loop range-list index *loop-cluster-size*))
               ;; Build a let map fragment
               (cache-index (copy-list index))
               (cluster-base (third index))
               (symtbl
                   (mapcar
                       #'(lambda (name)
                             `(,(gensym (symbol-name name))
                                  (tmp-ref
                                      (temporary ',name
                                          (,*loop-cluster-size*) 0)
                                      (- ,index ,cluster-base))))
                       vars))
               (reftbl
                   (mapcar
                       #'(lambda (name symdef)
                             (list name (first symdef)))
                       vars symtbl))
               ;; Build a loop to compute values
               (calc-loop
                   `(loop-range ,cache-index
                       ,(replace-unquoted
                            (wrap-with-let with
                                `(progn
                                     ,@(mapcar
                                           #'(lambda (name symdef)
                                                 `(setf ,(first symdef) ,name))
                                           vars symtbl)))
                            (subst cache-index index replace-tbl))))
               ;; Build a with map with vars replaced with temp refs
               (in-with (append
                            reftbl
                            (remove-if #'(lambda (x) (find x vars))
                                with :key #'first))))
            (setf (fifth cache-index) (abs (fifth cache-index))) ; delta
            (setf (sixth cache-index) nil) ;order
            (setf (seventh cache-index) 0) ;level
            (values range-list in-with calc-loop symtbl))))

(defun make-compute-loops (name idxspec expr in-with carrying cluster-cache)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (with      (convert-letv-exprs-auto in-with)))
            (multiple-value-bind
                    (range-list replace-tbl)
                    (build-loop-list name indexes idxlist)
                ;; Don't cluster unless the last loop is ordered
                (unless (and cluster-cache range-list
                        (ranging-order-flag (car (last range-list))))
                    (setf cluster-cache nil))
                ;; Apply clustering (alters ranges)
                (multiple-value-bind
                        (range-list in-with cluster-loop cluster-syms)
                        (make-cluster-refs
                            range-list cluster-cache replace-tbl with)
                    ;; Set the level tags
                    (correct-loop-levels range-list 0)
                    ;; Create actual loops
                    (multiple-value-bind
                            (loop-expr loop-list)
                            (do-wrap-loops
                                (list
                                    (wrap-with-let in-with
                                        `(setf (iref ,name ,@idxvars) ,expr)))
                                range-list replace-tbl)
                        ;; Apply carry
                        (let* ((carry-expr
                                   (make-compute-carry
                                       carrying loop-expr loop-list
                                       range-list with in-with replace-tbl))
                               (loop-cnt (length loop-list))
                               (pre-last-loop (if (> loop-cnt 1)
                                                  (nth (- loop-cnt 2) loop-list))))
                            ;; Insert the cluster precalculation loop
                            (when cluster-loop
                                (if pre-last-loop
                                    (prepend-loop-item pre-last-loop cluster-loop)
                                    (setf carry-expr `(progn ,cluster-loop ,carry-expr))))
                            (values
                                (wrap-symbol-macrolet cluster-syms carry-expr)
                                loop-list range-list))))))))

(defmacro compute (&whole original name idxspec expr &key with carrying parallel cluster-cache)
    (let* ((*current-compute* original)
           (*consistency-checks* (make-hash-table :test #'equal)))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with carrying cluster-cache)
            (let* ((nolet-expr (expand-let loop-expr))
                   (noiref-expr (simplify-iref nolet-expr))
                   (check-expr  (insert-checks noiref-expr))
                   (motion-expr (code-motion check-expr))
                   (annot-expr  (annotate-types motion-expr)))
                (wrap-compute-parallel parallel range-list
                    `(let ((*current-compute* ',original)
                           (*current-compute-body* ',motion-expr))
                        (declare (optimize (safety 1) (debug 1)))
                        ,annot-expr))))))

(defmacro calc (exprs)
    (annotate-types (code-motion (simplify-iref (expand-let (macroexpand-1 `(letv ,exprs)))))))

