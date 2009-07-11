;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defmacro ranging (expr min max delta &optional ordered-p loop-level &rest tail)
    (declare (ignore min max delta ordered-p loop-level tail))
    expr)

(defmacro arr-dim (arr idx rank)
    `(array-dimension ,arr ,idx))

(defmacro _grp (arg) arg)

(defmacro temporary (name dims level &optional mode)
    (if (null dims)
        0.0
        `(the (array single-float)
             (make-array (list ,@dims)
                 :element-type 'single-float
                 :initial-element 0.0))))

(defmacro tmp-ref (temp &rest dims)
    (if (null dims)
        temp
        `(aref ,temp ,@dims)))

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



(defun ranging-order-flag (rspec)
    (unless (eql (car rspec) 'ranging)
        (error "Not a ranging spec: ~A" rspec))
    (nth 5 rspec))

(defun ranging-loop-level (rspec)
    (unless (eql (car rspec) 'ranging)
        (error "Not a ranging spec: ~A" rspec))
    (nth 6 rspec))

(defun remove-ranges (expr)
    (simplify-rec-once
        #'(lambda (expr old)
              (if (and (consp expr)
                       (eql (car expr) 'ranging))
                  (second expr)
                  nil))
        expr))


(defun get-full-expr (expr)
    (cond
        ((symbolp expr)
            (or (get expr 'full-expr) expr))
        ((consp expr)
            (mapcar-save-old #'get-full-expr expr))
        (t
            expr)))

(defun unwrap-factored (expr)
    (let ((full-expr (if (symbolp expr) (get expr 'let-clause))))
        (or (cadr full-expr) expr)))

(defun recurse-factored (fun expr &rest args)
    (apply fun (unwrap-factored expr) args))


(defun index-expr-p (expr)
    (or (numberp expr)
        (and (consp expr)
             (find (car expr)
                 '(+ - * / 1+ 1- floor ceiling mod rem truncate ranging)))))


(defun apply-skipping-structure (fun expr args)
    (match expr
        (`(progn ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(,(or 'let 'let* 'symbol-macrolet 'loop-range) ,_ ,@rest)
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(safety-check ,checks ,@rest)
            (dolist (item checks)
                (apply-skipping-structure fun (first item) args))
            (dolist (item rest)
                (apply-skipping-structure fun item args)))
        (`(setf ,_ ,_)
            (apply fun expr args))
        (`(declare ,@_) nil)
        (_
;            (format t "Unknown structure statement: ~A" expr)
            (apply fun expr args))))


(defun range-band-master (range)
    (let ((idx (second range)))
        (or (get idx 'band-master)
            idx)))

(defun prepend-loop-item (rloop entry)
    (setf (cddr rloop)
        (cons entry (cddr rloop))))

(defun append-loop-item (rloop entry)
    (nconc rloop (list entry)))
