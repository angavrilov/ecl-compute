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

(defmacro compute (name idxspec expr)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (when (null indexes)
            (error "Unknown multivalue ~A" name))
        (let* ((idxtab    (mapcar #'cons indexes idxspec))
               (idxord    (reorder idxtab layout #'caar))
               (idxlist   (mapcan #'get-iter-spec idxord))
               (idxvars   (mapcar #'get-index-var idxspec))
               (full-expr `(setf (iref ,name ,@idxvars) ,expr)))
            (wrap-idxloops name indexes idxlist (list full-expr) :min-layer 0))))

(pprint (macroexpand-1 '(compute HFIFi (z k) (+ (iref HFIFi z k) 10))))
(pprint (macroexpand-1 '(compute HFIFi ((+ z) k) (+ (iref HFIFi z k) 10))))


