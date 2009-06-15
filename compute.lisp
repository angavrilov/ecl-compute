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

(defun create-carry (index carrying range-list loop-list with replace-tbl)
    (let* ((pos       (or (position index range-list :key #'second)
                          (error "Invalid carry index: ~A" index)))
           (range     (nth pos range-list))
           ;; Inner dimensions
           (iranges   (nthcdr (1+ pos) range-list))
           (idims     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(+ (- ,(fourth rg) ,(third rg)) 1)))
                          iranges))
           (irefs     (mapcar #'(lambda (rg)
                                    (simplify-index
                                        `(- ,rg ,(third rg))))
                          iranges))
           ;; Modification points
           (out-loop   (nth pos loop-list))
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
                            (wrap-with-let with
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
        ;; Skip the band loop, if found
        (when (and (second out-loop)
                   (eql
                       (get (second (second out-loop)) 'band-master)
                       index))
            (setf out-loop (nth (1- pos) loop-list)))
        ;; Splice in the new imperative code
        (setf (cddr out-loop)
            (cons init-code (cddr out-loop)))
        (nconc last-loop (list alter-code))
        ;; Return the new names
        name-table))

(defun make-compute-carry (carrying loop-expr loop-list range-list with replace-tbl)
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
                             with replace-tbl))
                   carry-indices)))
        (if (null carry-table)
            loop-expr
            `(symbol-macrolet ,carry-table ,@(cddr carry-body)))))

(defun make-compute-loops (name idxspec expr in-with carrying)
    (multiple-value-bind
            (indexes layout dimensions) (get-multivalue-info name)
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (with      (convert-letv-exprs-auto in-with))
               (full-expr (wrap-with-let with
                              `(setf (iref ,name ,@idxvars) ,expr))))
            (multiple-value-bind
                    (loop-expr loop-list range-list replace-tbl)
                    (wrap-idxloops name indexes idxlist
                        (list full-expr) :min-layer 0)
                (values
                    (make-compute-carry carrying loop-expr loop-list range-list with replace-tbl)
                    loop-list range-list)))))

(defmacro compute (&whole original name idxspec expr &key with carrying parallel)
    (let* ((*current-compute* original)
           (*consistency-checks* (make-hash-table :test #'equal)))
        (multiple-value-bind
                (loop-expr loop-list range-list)
                (make-compute-loops name idxspec expr with carrying)
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

