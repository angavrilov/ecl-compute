;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defparameter *current-compute* nil)
(defparameter *current-compute-body* nil)

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

(defmacro def-multivalue (&whole defspec name indexes
                             &key (layout (mapcar #'car indexes)))
    (when (/= (length indexes) (length layout))
        (error "Cannot layout ~A as ~A" indexes layout))
    (let* ((index-reordered (reorder indexes layout #'car))
           (index-dims (mapcan #'index-dimension index-reordered))
           (decl-dims (mapcar #'(lambda (x) '*) index-dims)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
             (when (get ',name 'mv-iref-macro)
                 (error "~A is already defined by def-multivalue-macro" ',name))
             (defvar ,name nil)
             (declaim (type multivalue ,name))
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
                                          (multivalue-data-dims reusable)))
                                 (progn
                                     (setf (multivalue-name reusable) ',name)
                                     reusable)
                                 (progn
                                     (setf reuse-list nil)
                                     (make-multivalue
                                         :name ',name
                                         :data-array
                                             (make-array dims
                                                 :element-type 'single-float
                                                 :initial-element 0.0)
                                         :data-dims dims))))))
                 (defun ,(deallocator-symbol name) (item)
                     (unless (typep item 'multivalue)
                         (error "Trying to deallocate a non-multivalue"))
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

(defmacro def-multivalue-macro (name indexes body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when (get ',name 'mv-indexes)
             (error "~A is already defined by def-multivalue" ',name))
         (set-prop-nochange ',name 'mv-iref-macro '(,indexes . ,body))))

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
