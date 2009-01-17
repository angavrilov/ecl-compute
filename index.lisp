(use-package :cl-match)

(defun unsymbol (x)
    (if (symbolp x) (symbol-name x) x))

(defun symcat (&rest items)
    (intern
        (apply #'concatenate 'string
            (mapcar #'unsymbol items))))

(defun simplify-index-1 (expr &optional (old-expr expr))
    (match expr
        ;; Expand abbreviations
        (`(1+ ,x) `(+ ,x 1))
        (`(1- ,x) `(- ,x 1))
        ;; Collapse arithmetical no-ops
        ((or `(/ ,x 1) `(* ,x 1)
             `(+ ,x 0) `(- ,x 0) `(+ ,x))
            x)
        ((or `(* ,_ 0) `(/ 0 ,_)) 0)
        (`(/ ,_ 0)
            (error "Division by zero: ~A" old-expr))
        ;; Calculate fully numerical expressions
        ((when (every #'numberp args)
             `(,(or '+ '- '* '/ 'mod 'rem 'truncate 'floor) ,@args))
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
        ((when (= (mod n1 n2) 0)
             `(/ (* ,x ,(type integer n1)) ,(type integer n2)))
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
        ;; Remove trivial zero remainder case
        ((when (= (mod mulv divv) 0)
             `(,(or 'mod 'rem)
                  (* _ ,(type integer mulv))
                  ,(type integer divv)))
            0)
        ;; Nothing to do
        (_ nil)))

(defun simplify-rec (engine expr)
    (if (atom expr)
        expr
        (let* ((rec-res (mapcar
                            #'(lambda (sub) (simplify-rec engine sub))
                            expr))
               (subs-res (funcall engine rec-res expr)))
            (if (null subs-res)
                rec-res
                (simplify-rec engine subs-res)))))

(defun index-dimension (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (let ((fullrange `(/ (- ,maxv ,minv) ,by)))
            (simplify-rec #'simplify-index-1
                (if (> bands 1)
                    (list bands `(1+ (/ ,fullrange ,bands)))
                    (list `(1+ ,fullrange)))))))

(defun index-refexpr (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) (car item)
        (let ((value `(/ (- ,(cdr item) ,minv) ,by)))
            (simplify-rec #'simplify-index-1
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

(defmacro iref (name &rest idxvals)
    (let ((indexes (get name 'mv-indexes))
          (layout  (get name 'mv-layout)))
        (if (null indexes)
            `(aref ,name ,@idxvals)
            (let* ((idxtab (mapcar #'cons indexes idxvals))
                   (idxord (reorder idxtab layout #'caar))
                   (idxlst (mapcan #'index-refexpr idxord)))
                (when (/= (length idxvals) (length indexes))
                    (error "Index count mismatch for ~A: ~A instead of ~A"
                        name idxvals indexes))
                `(aref ,name ,@idxlst)))))

(defparameter M1 0)
(defparameter N5 0)

(pprint (macroexpand-1 '(def-multivalue HFIFi ((i 2 (1- N5) :bands 2) (k 2 (1- M1) :by 2)) :layout (k i))))

(def-multivalue HFIFi ((i 2 (1- N5) :bands 2) (k 2 (1- M1) :by 2)) :layout (k i))

(pprint (macroexpand-1 '(alloc-multivalues HFIFi)))
(pprint (macroexpand-1 '(with-local-multivalues (HFIFi) foo)))
(pprint (macroexpand-1 '(iref HFIFi 100 200)))
(pprint (macroexpand-1 '(setf (iref HFIFi 100 200) 5)))

