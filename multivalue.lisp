;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun unsymbol (x)
    (if (symbolp x) (symbol-name x) x))

(defun symcat (&rest items)
    (intern
        (apply #'concatenate 'string
            (mapcar #'unsymbol items))))

(defun index-dimension (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (let ((fullrange `(/ (- ,maxv ,minv) ,by)))
            (simplify-index
                (if (> bands 1)
                    (list bands `(/ (1+ ,fullrange) ,bands))
                    (list `(1+ ,fullrange)))))))

(defun index-refexpr (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) (car item)
        (let ((value `(/ (- ,(cdr item) ,minv) ,by)))
            (simplify-index
                (if (> bands 1)
                    (list `(mod ,value ,bands) `(floor ,value ,bands))
                    (list value))))))

(defun allocator-symbol (mv-name)
    (symcat "ALLOC-" mv-name))

(defun deallocator-symbol (mv-name)
    (symcat "DEALLOC-" mv-name))

(defun reorder (table order key)
    (mapcar #'(lambda (iname)
                  (let ((item (find iname table :key key)))
                      (when (null item)
                          (error "Cannot find multivalue index '~A' in supplied args ~A" iname table))
                      item))
        order))

(defun set-prop-nochange (sym tag value)
    (let ((old-value (get sym tag)))
        (if (or (null old-value) (equal old-value value))
            (setf (get sym tag) value)
            (error "Cannot redefine ~A for ~A to: ~A~% - already set to: ~A"
                tag sym value old-value))))

(defmacro def-multivalue (&whole defspec name indexes
                             &key (layout (mapcar #'car indexes)))
    (when (/= (length indexes) (length layout))
        (error "Cannot layout ~A as ~A" indexes layout))
    (let* ((index-reordered (reorder indexes layout #'car))
           (index-dims (mapcan #'index-dimension index-reordered))
           (decl-dims (mapcar #'(lambda (x) '*) index-dims)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defvar ,name nil)
             (declaim (type (array single-float ,decl-dims) ,name))
             (set-prop-nochange ',name 'mv-indexes ',indexes)
             (set-prop-nochange ',name 'mv-layout ',layout)
             (setf (get ',name 'mv-dimensions) ',index-dims)
             (setf (get ',name 'mv-definition) ',defspec)
             (let ((reuse-list nil)
                   (reuse-mutex (mp:make-lock)))
                 (defun ,(allocator-symbol name) ()
                     (with-lock-spin (reuse-mutex)
                         (let ((dims (list ,@index-dims))
                               (reusable (pop reuse-list)))
                             (if (and reusable
                                      (equal dims
                                          (array-dimensions reusable)))
                                 reusable
                                 (progn
                                     (setf reuse-list nil)
                                     (make-array dims
                                         :element-type 'single-float
                                         :initial-element 0.0))))))
                 (defun ,(deallocator-symbol name) (item)
                     (unless (arrayp item)
                         (error "Trying to deallocate a non-array multivalue"))
                     (with-lock-spin (reuse-mutex)
                         (push item reuse-list)))))))

(defmacro copy-multivalue (name aliases)
    (let ((definition (get name 'mv-definition)))
        (unless (and definition
                     (eql (car definition) 'def-multivalue))
            (error "Not a multivalue: ~A" name))
        `(progn
             ,@(mapcar #'(lambda (new-name)
                             `(def-multivalue ,new-name ,@(cddr definition)))
                   aliases))))

(defmacro alloc-multivalues (&rest names)
    (let ((commands (mapcar
                        #'(lambda (name)
                              `(setf ,name (,(allocator-symbol name))))
                        names)))
        `(progn ,@commands)))

(defmacro with-local-multivalues (names &body code)
    (let ((maps (mapcar
                    #'(lambda (name)
                        `(,name (,(allocator-symbol name))))
                    names))
          (unmaps (mapcar
                      #'(lambda (name)
                          `(,(deallocator-symbol name) ,name))
                      names)))
        `(let (,@maps)
             (prog1
                 (progn ,@code)
                 ,@unmaps))))

(defun get-multivalue-info (name)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (when (null indexes)
            (error "Unknown multivalue ~A" name))
        (values indexes layout dimensions)))

(defun check-dimension (expr dim verbose-p)
    (let* ((range (match (compute-range expr)
                      ((type number val)
                          (cons val val))
                      (`(ranging ,_ ,min ,max ,@_)
                          (cons min max))
                      (_ (when verbose-p
                             (format t "Cannot determine range for: ~A~%" expr))
                          nil)))
           (min-val (car range))
           (max-val (cdr range))
           (checks  nil))
        (case (compare-indexes (or min-val expr) 0)
            (<
                (error "Index ~A can be less than the 0 limit" expr))
            ((> =) nil)
            (t
                (when verbose-p
                    (format t "Cannot compare ~A~%  as min value ~A with 0~%" expr min-val))
                (push `(>= ,(or min-val expr) 0) checks)))
        (case (compare-indexes (or max-val expr) dim)
            (< nil)
            ((> =)
                (error "Index ~A can reach the top limit of ~A" expr dim))
            (t
                (when verbose-p
                    (format t "Cannot compare ~A~%  as max value ~A with ~A~%" expr max-val dim))
                (push `(< ,(or max-val expr) ,dim) checks)))
        checks))

(defun expand-iref (name idxvals &key verbose-p)
    (let ((indexes    (get name 'mv-indexes))
          (layout     (get name 'mv-layout))
          (dimensions (get name 'mv-dimensions)))
        (if (null indexes)
            `(aref ,name ,@idxvals)
            (let* ((idxtab (mapcar #'cons indexes idxvals))
                   (idxord (reorder idxtab layout #'caar))
                   (idxlst (mapcan #'index-refexpr idxord))
                   (dimchk (mapcan #'(lambda (iexpr dim)
                                         (check-dimension iexpr dim verbose-p))
                               idxlst dimensions)))
                (when (/= (length idxvals) (length indexes))
                    (error "Index count mismatch for ~A: ~A instead of ~A"
                        name idxvals indexes))
                (values `(aref ,name ,@idxlst) t dimchk)))))

(defparameter *iref-cache* (make-hash-table))

(defmacro iref (&whole form name &rest idxvals)
    (let ((cached (gethash form *iref-cache*)))
        (if cached cached
            (setf (gethash form *iref-cache*)
                (expand-iref name idxvals)))))

(defmacro enable-expr-quotes ()
   `(eval-when (:compile-toplevel :execute)
        (formula:enable-expr-quotes)
        (setf formula:*index-access-symbol* 'iref)))

(defun get-num-step (step)
    (match step
        (`(* ,x) x)
        (_ (or step 1))))

(defun get-ord-step (step)
    (match step
        (`(* ,_) nil)
        (_ step)))

(defun index-iterexpr (item iname &key (as iname) (var as) step (skip '(0 0))
                          (skip-low (car skip)) (skip-high (cadr skip)))
    (unless (symbolp var)
        (error "Expecting a symbol for dimension '~A', found: ~A" iname var))
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (if (> bands 1)
            ;; Multiple bands
            (let* ((dimension (cadr (index-dimension item)))
                   (num-step  (get-num-step step))
                   (one-step  (if (> num-step 0) 1 -1))
                   (band-step (if (>= (abs num-step) bands) bands num-step))
                   (line-step (if (>= (abs num-step) bands) (/ num-step bands) one-step))
                   (band-min  (if (< skip-low (abs band-step)) skip-low 0))
                   (band-max  (if (< skip-high (abs band-step)) skip-high 0))
                   (line-min  (if (>= skip-low bands) (/ skip-low bands) 0))
                   (line-max  (if (>= skip-high bands) (/ skip-high bands) 0))
                   (var-range `(ranging ,var
                                   ,line-min
                                   ,(simplify-index `(- (- ,dimension ,line-max) 1))
                                   ,line-step ,(get-ord-step step) nil)))
                (unless (and (integerp band-step) (integerp line-step)
                             (= (mod bands band-step) 0))
                    (error "~A: step ~A does not match band count ~A"
                        iname num-step bands))
                (unless (and (integerp line-min)
                            (= (+ (* bands line-min) band-min) skip-low))
                    (error "~A: cannot skip-low ~A with ~A bands and step ~A"
                        iname skip-low bands num-step))
                (unless (and (integerp line-max)
                            (= (+ (* bands line-max) band-max) skip-high))
                    (error "~A: cannot skip-high ~A with ~A bands and step ~A"
                        iname skip-high bands num-step))
                (when (>= (+ band-min band-max) (abs band-step))
                    (error "~A: cannot skip (~A + ~A) with ~A bands and step ~A"
                        iname skip-low skip-high bands num-step))
                (if (= bands (abs band-step))
                    (let* ((band-value (if (> one-step 0) band-min
                                           (- (- bands band-max) 1)))
                           (expr `(+ (* (+ (* ,var-range ,bands) ,band-value) ,by) ,minv)))
                        (values (cons var expr) (list var-range)))
                    (let* ((band-var    (gensym (symbol-name var)))
                           (band-range `(ranging ,band-var
                                            ,band-min ,(- (- bands band-max) 1)
                                            ,band-step ,band-step
                                            nil))
                           (expr `(+ (* (+ (* ,var-range ,bands) ,band-range) ,by) ,minv)))
                        (setf (get band-var 'band-master) var)
                        (values (cons var expr) (list band-range var-range)))))
            ;; Single band
            (let* ((dimension (car (index-dimension item)))
                   (num-step  (get-num-step step))
                   (var-range `(ranging ,var
                                   ,skip-low
                                   ,(simplify-index `(- (- ,dimension ,skip-high) 1))
                                   ,num-step ,(get-ord-step step) nil))
                   (expr `(+ (* ,var-range ,by) ,minv)))
                (values (cons var expr) (list var-range))))))

(defun apply-index-iterexpr (name indexes item)
    (let* ((wrap  (if (atom item) (list item) item))
           (wrap2 (if (or (null (cdr wrap)) (keywordp (cadr wrap)))
                      wrap
                      (list* (car wrap) ':as (cdr wrap))))
           (idxobj (find (car wrap2) indexes :key #'car)))
        (unless idxobj
            (error "Unknown index '~A' for multivalue ~A: ~A" (car wrap2) name item))
        (apply #'index-iterexpr idxobj wrap2)))

(defun correct-loop-levels (range-list min-layer)
    (dolist (range (reverse range-list))
        (setf (seventh range) min-layer)
        (incf min-layer))
    (values range-list min-layer))

(defun build-loop-list (name indexes idxlist &key min-layer)
    (let ((replace-tbl nil)
          (loop-lst    nil))
        (dolist (item (reverse idxlist))
            (multiple-value-bind
                    (pair ranges)
                    (apply-index-iterexpr name indexes item)
                (push pair replace-tbl)
                (setf loop-lst (nconc ranges loop-lst))))
        (when min-layer
            (correct-loop-levels loop-lst min-layer))
        (values loop-lst replace-tbl)))

(defmacro loop-range (rangespec &body code)
    (destructuring-bind
        (rg var minv maxv stepv &rest x) rangespec
        (unless (eql rg 'ranging)
            (error "Invalid range spec: ~A" rangespec))
        (if (> stepv 0)
            `(do ((,var ,minv (+ ,var ,stepv)))
                 ((> ,var ,maxv) nil)
                 (declare (type fixnum ,var))
                 ,@code)
            `(do ((,var ,maxv (- ,var ,(- stepv))))
                 ((< ,var ,minv) nil)
                 (declare (type fixnum ,var))
                 ,@code))))

(defun do-wrap-loops (code ranges replace-tbl)
    (let ((loops nil)
          (cur-code (replace-unquoted code replace-tbl)))
        (dolist (item (reverse ranges))
            (let ((cloop `(loop-range ,item ,@cur-code)))
                (push cloop loops)
                (setf cur-code (list cloop))))
        (values
            (wrap-progn cur-code)
            loops ranges replace-tbl)))

(defun wrap-idxloops (name indexes idxlist code &key min-layer)
    (multiple-value-call
        #'do-wrap-loops
        code
        (build-loop-list
            name indexes idxlist
            :min-layer min-layer)))

(defmacro loop-indexes (name idxlist &body code)
    (let ((indexes      (get name 'mv-indexes)))
        (unless indexes
            (error "Unknown multivalue ~A" name))
        (wrap-idxloops name indexes idxlist code)))
