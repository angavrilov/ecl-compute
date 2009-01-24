;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

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

(defun expand-let-1 (expr old-expr)
    (match expr
        (`(let ,vars ,@body)
            (replace-unquoted (wrap-progn body)
                (mapcar
                    #'(lambda (x) (cons (first x) (second x)))
                    vars)))
        (`(let* () ,@body)
            (wrap-progn body))
        (`(let* (,vspec ,@vars) ,@body)
            `(let (,vspec) (let* (,@vars) ,@body)))
        (_ nil)))

(defun expand-let (expr)
    (simplify-rec #'expand-let-1 expr (make-hash-table)))

(defun simplify-iref-1 (expr old-expr)
    (match expr
        (`(iref ,name ,@idxvals)
            (expand-iref name idxvals :verbose-p t))
        (_ nil)))

(defun simplify-iref (expr)
    (simplify-rec-once #'simplify-iref-1 expr))

(defmacro compute (name idxspec expr &key with)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (when (null indexes)
            (error "Unknown multivalue ~A" name))
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (let-expr  (if with `(let* ,with ,expr) expr))
               (full-expr `(setf (iref ,name ,@idxvars) ,let-expr))
               (loop-expr (wrap-idxloops name indexes idxlist
                              (list full-expr) :min-layer 0))
               (nolet-expr (expand-let loop-expr))
               (noiref-expr (simplify-iref nolet-expr)))
            noiref-expr)))

(pprint (macroexpand-1 '(compute HFIFi (z k) (+ old 10) :with ((old (iref HFIFi z k))))))
(pprint (macroexpand-1 '(compute HFIFi (z (+ k)) (+ (iref HFIFi z k) 10))))


