;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(use-package :cl-match)

(defun unsymbol (x)
    (if (symbolp x) (symbol-name x) x))

(defun symcat (&rest items)
    (intern
        (apply #'concatenate 'string
            (mapcar #'unsymbol items))))

(defmacro ranging (expr min max delta &optional ordered-p loop-level &rest tail)
    (declare (ignore min max delta ordered-p loop-level tail))
    expr)

(defun compute-range-1 (expr &optional (old-expr expr))
    (match expr
        (`(- (ranging ,arg ,min ,max ,delta ,@rest))
            `(ranging (- ,arg) (- ,max) (- ,min) (- ,delta) ,@rest))
        (`(,(as op (or '+ '-))
              (ranging ,arg ,min ,max ,@rest) ,@pv)
            `(ranging (,op ,arg ,@pv)
                 (,op ,min ,@pv) (,op ,max ,@pv) ,@rest))
        ((when (and (numberp mv) (>= mv 0))
            `(,(as op (or '* '/))
                 (ranging ,arg ,min ,max ,delta ,@rest) ,mv))
            `(ranging (,op ,arg ,mv)
                 (,op ,min ,mv) (,op ,max ,mv) (,op ,delta ,mv) ,@rest))
        ((when (and (numberp mv) (< mv 0))
            `(,(as op (or '* '/))
                 (ranging ,arg ,min ,max ,delta ,@rest) ,mv))
            `(ranging (,op ,arg ,mv)
                 (,op ,max ,mv) (,op ,min ,mv) (,op ,delta ,mv) ,@rest))
        (_ nil)))

(defun simplify-index-1 (expr &optional (old-expr expr))
    (match expr
        ;; Expand abbreviations
        (`(1+ ,x) `(+ ,x 1))
        (`(1- ,x) `(- ,x 1))
        ;; Expand trivial ranges
        ((or
             `(ranging ,(type number val) ,@_)
             `(ranging ,_ ,(type number val) ,val ,@_))
            val)
        ;; Collapse arithmetical no-ops
        ((or `(/ ,x 1) `(* ,x 1)
             `(+ ,x 0) `(- ,x 0) `(+ ,x)
             `(floor ,x 1) `(truncate ,x 1) `(ceiling ,x 1))
            x)
        ((or `(* ,_ 0) `(* 0 ,_) `(/ 0 ,_)
             `(mod 0 ,_) `(rem 0 ,_)
             `(mod ,_ 1) `(rem ,_ 1)
             `(floor 0 ,_) `(truncate 0 ,_) `(ceiling 0 ,_))
            0)
        ((or `(/ ,_ 0)
             `(floor ,_ 0) `(truncate ,_ 0))
            (error "Division by zero: ~A" old-expr))
        ;; Calculate fully numerical expressions
        ((when (every #'numberp args)
             `(,(or '+ '- '* '/ 'mod 'rem 'truncate 'floor 'ceiling) ,@args))
            (eval expr))
        ;; Normalize sign
        ((when (< v 0) `(+ ,x ,(type number v)))
            `(- ,x ,(- v)))
        ((when (< v 0) `(- ,x ,(type number v)))
            `(+ ,x ,(- v)))
        ;; Reorder the number in + and * to be last
        ((when (not (numberp b))
             `(,(as op (or '+ '*)) ,(type number a) ,b))
            (list op b a))
        ;; Simplify adjacent * & /
        (`(,(as op (or '* '/))
             (,op ,x ,(type integer i1))
             ,(type integer i2))
            (list op x (* i1 i2)))
        ;; Simplify obviously even division
        ((when (= (mod n1 n2) 0)
             `(,(or '/ 'floor 'ceiling 'truncate)
                  (* ,x ,(type integer n1))
                  ,(type integer n2)))
            `(* ,x ,(/ n1 n2)))
        ;; Simplify adjacent + & -
        (`(,(as op2 (or '+ '-))
             (,(as op1 (or '+ '-)) ,x ,(type integer i1))
             ,(type integer i2))
            (let ((iv (if (eql op1 op2) (+ i1 i2) (- i1 i2))))
                `(,op1 ,x ,iv)))
        ;; Pull constants out of /
        ((when (/= (truncate n1 n2) 0)
             `(/ (,(as op (or '+ '-)) ,x ,(type integer n1))
                  ,(type integer n2)))
            (multiple-value-bind (divr remr) (truncate n1 n2)
                `(,op (/ (,op ,x ,remr) ,n2) ,divr)))
        ;; Push constants into *
        ((when (/= (truncate n1 n2) 0)
             `(,(as op (or '+ '-))
                  (* ,x ,(type integer n2))
                  ,(type integer n1)))
            (multiple-value-bind (divr remr) (truncate n1 n2)
                `(,op (* (,op ,x ,divr) ,n2) ,remr)))
        ;; Remove trivial zero remainder case of mod & rem
        ((when (= (mod mulv divv) 0)
             `(,(or 'mod 'rem)
                  (* ,_ ,(type integer mulv))
                  ,(type integer divv)))
            0)
        ;; Split aligned multiplicative clauses from mod &c
        ((when (= (mod mulv divv) 0)
             `(,(as cmd (or 'mod 'floor 'ceiling)) ; no truncate & rem !
                  (,(as op (or '+ '-))
                      (* ,marg ,(type integer mulv))
                      ,remv)
                  ,(type integer divv)))
            `(+ (,cmd (* ,marg ,mulv) ,divv)
                (,cmd (,op ,remv) ,divv)))
        ;; Likewise for aligned constants
        ((when (= (mod remv divv) 0)
             `(,(as cmd (or 'mod 'floor 'ceiling)) ; no truncate & rem !
                  (,(as op (or '+ '-)) ,exv ,remv)
                  ,(type integer divv)))
            `(+ (,cmd ,exv ,divv)
                (,cmd (,op ,remv) ,divv)))
        ;; Strip mod if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (floor (car range) divv)
                                 (floor (cdr range) divv))))
             `(mod ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                `(- ,modv ,(* (floor (car range) divv) divv))))
        ;; Kill floor if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (floor (car range) divv)
                                 (floor (cdr range) divv))))
             `(floor ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                (floor (car range) divv)))
        ;; Kill ceiling if the value is in an aligned range
        ((when (let ((range (compute-num-range modv)))
                   (and range (= (ceiling (car range) divv)
                                 (ceiling (cdr range) divv))))
             `(ceiling ,modv ,(type integer divv)))
            (let ((range (compute-num-range modv)))
                (ceiling (car range) divv)))
        ;; Nothing to do
        (_ nil)))

(defun simplify-rec (engine expr cache)
    (if (or (atom expr) (gethash expr cache))
        expr
        (let* ((rec-res (mapcar
                            #'(lambda (sub) (simplify-rec engine sub cache))
                            expr))
               (subs-res (funcall engine rec-res expr)))
            (setf (gethash rec-res cache) t)
            (if (null subs-res)
                rec-res
                (simplify-rec engine subs-res cache)))))

(defun simplify-rec-once (engine expr)
    (if (atom expr)
        expr
        (let* ((rec-res (mapcar
                            #'(lambda (sub) (simplify-rec-once engine sub))
                            expr))
               (subs-res (funcall engine rec-res expr)))
            (if (null subs-res) rec-res subs-res))))

(defparameter *simplify-cache* (make-hash-table))

(defun simplify-index (expr)
    (simplify-rec #'simplify-index-1 expr *simplify-cache*))

(defparameter *range-cache* (make-hash-table))

(defun compute-range (expr)
    (let ((cached (gethash expr *range-cache*)))
        (if cached cached
            (setf (gethash expr *range-cache*)
                (simplify-index
                    (simplify-rec-once #'compute-range-1 expr))))))

(defun compare-indexes (expr1 expr2 &optional (delta 0))
    (match (cons expr1 expr2)
        (`((+ ,le ,(type number lv)) . ,_)
            (compare-indexes le expr2 (+ delta lv)))
        (`((- ,le ,(type number lv)) . ,_)
            (compare-indexes le expr2 (- delta lv)))
        (`(,_ . (+ ,re ,(type number rv)))
            (compare-indexes expr1 re (- delta rv)))
        (`(,_ . (- ,re ,(type number rv)))
            (compare-indexes expr1 re (+ delta rv)))
        (`((* ,le ,(type number lv)) . (* ,re ,lv))
            (compare-indexes le re (/ delta lv)))
        (`((/ ,le ,(type number lv)) . (/ ,re ,lv))
            (compare-indexes le re (* delta lv)))
        ((when (equal expr1 expr2))
            (cond ((< delta 0) '<)
                  ((> delta 0) '>)
                  (t '=)))
        (`(,(type number lv) . ,(type number rv))
            (compare-indexes 0 0 (+ delta (- lv rv))))
        (_ nil)))

(defun compute-num-range (expr)
    (match (compute-range expr)
        ((type number val)
            (cons val val))
        (`(ranging ,_ ,(type number min) ,(type number max) ,@_)
            (cons min max))
        (_ nil)))

(defun index-dimension (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (let ((fullrange `(/ (- ,maxv ,minv) ,by)))
            (simplify-index
                (if (> bands 1)
                    (list bands `(1+ (/ ,fullrange ,bands)))
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

(defun reorder (table order key)
    (mapcar #'(lambda (iname)
                  (let ((item (find iname table :key key)))
                      (when (null item)
                          (error "Cannot find index '~A' in ~A" iname table))
                      item))
        order))

(defun set-prop-nochange (sym tag value)
    (let ((old-value (get sym tag)))
        (if (or (null old-value) (equal old-value value))
            (setf (get sym tag) value)
            (error "Cannot redefine ~A for ~A to: ~A~% - already set to: ~A"
                tag sym value old-value))))

(defmacro def-multivalue (name indexes
                             &key (layout (mapcar #'car indexes)))
    (when (/= (length indexes) (length layout))
        (error "Cannot layout ~A as ~A" indexes layout))
    (let* ((index-reordered (reorder indexes layout #'car))
           (index-dims (mapcan #'index-dimension index-reordered))
           (decl-dims (mapcar #'(lambda (x) '*) index-dims)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defvar ,name nil)
             (declaim (type (array simple-float ,decl-dims) ,name))
             (set-prop-nochange ',name 'mv-indexes ',indexes)
             (set-prop-nochange ',name 'mv-layout ',layout)
             (setf (get ',name 'mv-dimensions) ',index-dims)
             (defun ,(allocator-symbol name) ()
                 (make-array (list ,@index-dims)
                     :element-type 'simple-float
                     :initial-element 0.0))
             )))

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
                    names)))
        `(let (,@maps) ,@code)))

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
        (case (compare-indexes min-val 0)
            (<
                (error "Index ~A can be less than the 0 limit" expr))
            ((> =) nil)
            (t
                (when verbose-p
                    (format t "Cannot compare ~A with 0~%" expr))
                (push `(>= ,min-val 0) checks)))
        (case (compare-indexes max-val dim)
            (< nil)
            ((> =)
                (error "Index ~A can reach the top limit of ~A" expr dim))
            (t
                (when verbose-p
                    (format t "Cannot compare ~A with ~A~%" expr dim))
                (push `(< ,max-val ,dim) checks)))
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

(defparameter *layer* nil)

(defun index-iterexpr (item iname &key (as iname) (var as) step (skip '(0 0))
                          (skip-low (car skip)) (skip-high (cadr skip)))
    (unless (symbolp var)
        (error "Expecting a symbol for dimension '~A', found: ~A" iname var))
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (if (> bands 1)
            ;; Multiple bands
            (let* ((dimension (cadr (index-dimension item)))
                   (num-step  (or step 1))
                   (one-step  (if (> num-step 0) 1 -1))
                   (band-step (if (>= (abs num-step) bands) bands num-step))
                   (line-step (if (>= (abs num-step) bands) (/ num-step bands) one-step))
                   (band-min  (if (< skip-low (abs band-step)) skip-low 0))
                   (band-max  (if (< skip-high (abs band-step)) skip-high 0))
                   (line-min  (if (>= skip-low bands) (/ skip-low bands) 0))
                   (line-max  (if (>= skip-high bands) (/ skip-high bands) 0))
                   (var-range `(ranging ,var
                                   ,line-min (- (- ,dimension ,line-max) 1)
                                   ,line-step ,step ,*layer*)))
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
                        (list var expr var-range))
                    (let* ((band-var    (gensym (symbol-name var)))
                           (band-range `(ranging ,band-var
                                            ,band-min ,(- (- bands band-max) 1)
                                            ,band-step ,band-step
                                            ,(if *layer* (1+ *layer*))))
                           (expr `(+ (* (+ (* ,var-range ,bands) ,band-range) ,by) ,minv)))
                        (list var expr band-range var-range))))
            ;; Single band
            (let* ((dimension (car (index-dimension item)))
                   (num-step  (or step 1))
                   (var-range `(ranging ,var
                                   ,skip-low (- (- ,dimension ,skip-high) 1)
                                   ,num-step ,step ,*layer*))
                   (expr `(+ (* ,var-range ,by) ,minv)))
                (list var expr var-range)))))

(defun apply-index-iterexpr (name indexes item)
    (let* ((wrap  (if (atom item) (list item) item))
           (wrap2 (if (or (null (cdr wrap)) (keywordp (cadr wrap)))
                      wrap
                      (list* (car wrap) ':as (cdr wrap))))
           (idxobj (find (car wrap2) indexes :key #'car)))
        (unless idxobj
            (error "Unknown index '~A' for multivalue ~A: ~A" (car wrap2) name item))
        (apply #'index-iterexpr idxobj wrap2)))

(defun replace-let (let-defs let-body replace-tbl)
    (let ((new-defs (mapcar
                        #'(lambda (item)
                              (cons
                                  (car item)
                                  (replace-unquoted (cdr item) replace-tbl)))
                        let-defs))
          (new-table (set-difference replace-tbl let-defs :key #'car)))
        (cons new-defs
            (mapcar
                #'(lambda (subexpr) (replace-unquoted subexpr new-table))
               let-body))))

(defun replace-let* (let-defs let-body replace-tbl)
    (let* ((new-table replace-tbl)
           (new-defs (mapcar
                         #'(lambda (item)
                               (let ((newv (replace-unquoted (cdr item) new-table)))
                                   (setf new-table
                                       (remove (car item) new-table :key #'car))
                                   (cons (car item) newv)))
                         let-defs)))
        (cons new-defs
            (mapcar
                #'(lambda (subexpr) (replace-unquoted subexpr new-table))
                let-body))))

(defun replace-unquoted (expr replace-tbl)
    (let ((target (cdr (assoc expr replace-tbl))))
        (cond
            (target target)
            ((atom expr) expr)
            ((null replace-tbl) expr)
            ((eql (first expr) 'quote) expr)
            ((eql (first expr) 'let)
                (cons 'let (replace-let (second expr) (cddr expr) replace-tbl)))
            ((eql (first expr) 'let*)
                (cons 'let* (replace-let* (second expr) (cddr expr) replace-tbl)))
            (t (cons
                   (replace-unquoted (car expr) replace-tbl)
                   (replace-unquoted (cdr expr) replace-tbl))))))

(defun build-loop-list (name indexes idxlist &key min-layer)
    (let ((replace-tbl nil)
          (loop-lst    nil)
          (*layer*     min-layer))
        (dolist (item (reverse idxlist))
            (let ((ie (apply-index-iterexpr name indexes item)))
                (push (cons (first ie) (second ie)) replace-tbl)
                (setf loop-lst
                    (concatenate 'list (cddr ie) loop-lst))
                (when *layer*
                    (incf *layer* (length (cddr ie))))))
        (values loop-lst replace-tbl)))

(defmacro loop-range (rangespec &body code)
    (destructuring-bind
        (var minv maxv stepv &rest x) rangespec
        (if (> stepv 0)
            `(do ((,var ,minv (+ ,var ,stepv)))
                 ((> ,var ,maxv) nil)
                 ,@code)
            `(do ((,var ,maxv (- ,var ,(- stepv))))
                 ((< ,var ,minv) nil)
                 ,@code))))

(defun wrap-idxloops (name indexes idxlist code &key min-layer)
    (multiple-value-bind
        (loops replace-tbl) (build-loop-list
                                name indexes idxlist
                                :min-layer min-layer)
        (do ((loop-lst (nreverse loops) (cdr loop-lst))
             (cur-code (replace-unquoted code replace-tbl)))
            ((null loop-lst)
                (if (cdr cur-code) `(progn ,@cur-code) (car cur-code)))
            (setf cur-code
                `((loop-range
                      ,(simplify-index (cdr (car loop-lst)))
                      ,@cur-code))))))

(defmacro loop-indexes (name idxlist &body code)
    (let ((indexes      (get name 'mv-indexes)))
        (unless indexes
            (error "Unknown multivalue ~A" name))
        (wrap-idxloops name indexes idxlist code)))

(defparameter M1 0)
(defparameter N5 0)

(pprint (macroexpand-1 '(def-multivalue HFIFi ((i 2 (1- N5) :bands 2) (k 2 (1- M1) :by 2)) :layout (k i))))

(def-multivalue HFIFi ((i 2 (1- N5) :bands 2) (k 2 (1- M1) :by 2)) :layout (k i))

(pprint (macroexpand-1 '(alloc-multivalues HFIFi)))
(pprint (macroexpand-1 '(with-local-multivalues (HFIFi) foo)))
(pprint (macroexpand-1 '(iref HFIFi 100 200)))
(pprint (macroexpand-1 '(setf (iref HFIFi 100 200) 5)))

(let ((tv (macroexpand-1 '(loop-indexes HFIFi ((i :as z :step -1 :skip (2 2)) k) (setf (iref HFIFi z k) 5)))))
    (pprint tv)
    (pprint (macroexpand-1 tv)))

(defun xxx () (loop-indexes HFIFi ((i :as z :step -1 :skip (2 2)) k) (setf (iref HFIFi (+ z 2) k) 5)))
