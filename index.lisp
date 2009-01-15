(defun unsymbol (x)
    (if (symbolp x) (symbol-name x) x))

(defun symcat (&rest items)
    (intern
        (apply #'concatenate 'string
            (mapcar #'unsymbol items))))

(defun index-dimension (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (let ((fullrange `(/ (- ,maxv ,minv) ,by)))
            (if (> bands 1)
                (list bands `(1+ (/ ,fullrange ,bands)))
                (list `(1+ ,fullrange))))))

(defun index-refexpr (item)
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) (car item)
        (let ((value `(/ (- ,(cdr item) ,minv) ,by)))
            (if (> bands 1)
                (list `(mod ,value ,bands) `(truncate (/ ,value ,bands)))
                (list value)))))

(defun allocator-symbol (mv-name)
    (symcat "ALLOC-" mv-name))

(defun reorder (table order key)
    (mapcar #'(lambda (iname)
                  (let ((item (find iname table :key key)))
                      (when (null item)
                          (error "Cannot find index '~A' in ~A" iname table))
                      item))
        order))

(defmacro def-multivalue (name indexes
                             &key (layout (mapcar #'car indexes)))
    (let* ((index-reordered (reorder indexes layout #'car))
           (index-dims (mapcan #'index-dimension index-reordered))
           (decl-dims (mapcar #'(lambda (x) '*) index-dims)))
        (when (/= (length indexes) (length layout))
            (error "Cannot layout ~A as ~A" indexes layout))
        (setf (get name 'mv-indexes) indexes)
        (setf (get name 'mv-layout) layout)
        `(progn
             (defparameter ,name nil)
             (defun ,(allocator-symbol name) ()
                 (make-array ,index-dims
                     :element-type 'simple-float
                     :initial-element 0.0))
             (declaim (type (array simple-float ,decl-dims) ,name)))))

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

(pprint (macroexpand-1 '(def-multivalue HFIFi ((i 2 (1- N5) :bands 2) (k 2 (1- M1) :by 2)) :layout (k i))))
(pprint (macroexpand-1 '(alloc-multivalues HFIFi)))
(pprint (macroexpand-1 '(with-local-multivalues (HFIFi) foo)))
(pprint (macroexpand-1 '(iref HFIFi 100 200)))
(pprint (macroexpand-1 '(setf (iref HFIFi 100 200) 5)))

