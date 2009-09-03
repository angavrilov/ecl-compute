;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

;;; Range annotation

(defparameter *print-ranges* nil)

(defstruct (range
             (:print-function print-range))
  min max delta ordered-p loop-level)

(defun print-range (rg stream level)
  (if *print-ranges*
      (format stream "#<~A...~A by ~A~A at ~A>"
              (range-min rg) (range-max rg) (range-delta rg)
              (if (range-ordered-p rg) " ordered" "")
              (range-loop-level rg))
      (format stream "#<RG>")))


(defmacro index (expr rg)
  (declare (ignore rg))
  expr)

(defmacro ranging-spec (expr &rest args)
  (if (or (null args) (keywordp (first args)))
      `(list 'index ,expr (make-range ,@args))
      (destructuring-bind
            (min max delta &optional ordered-p loop-level) args
        `(list 'index ,expr
               (make-range :min ,min :max ,max :delta ,delta
                           :ordered-p ,ordered-p :loop-level ,loop-level)))))

(cl-match:defpattern ranging-spec (expr &rest args)
  (let* ((fix-args (if (or (null args) (keywordp (first args)))
                       args
                       (destructuring-bind
                             (min max delta &optional
                                  (ordered-p '_) (loop-level '_)) args
                         `(:min ,min :max ,max :delta ,delta
                                :ordered-p ,ordered-p :loop-level ,loop-level))))
         (arg-patterns (gmap (:list :filterp #'(lambda (x) (not (eql (second x) '_))))
                             #'list (:plist fix-args))))
    `(list 'index ,expr
           (struct range- ,@arg-patterns))))

;;; Array dimension macro

(defmacro arr-dim (arr idx rank)
  `(array-dimension ,arr ,idx))

;;; Expression flattening barrier

(defmacro _grp (arg) arg)

;;; Temporary buffer

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

;;; Annotated range loop

(defmacro loop-range (rangespec &body code)
  (letmatch (ranging-spec var minv maxv stepv) rangespec
    (if (> stepv 0)
        `(do ((,var ,minv (+ ,var ,stepv)))
             ((> ,var ,maxv) nil)
           (declare (type fixnum ,var))
           ,@code)
        `(do ((,var ,maxv (- ,var ,(- stepv))))
             ((< ,var ,minv) nil)
           (declare (type fixnum ,var))
           ,@code))))

;;; Range annotation helpers

(defun ranging-var (rspec)
  (ifmatch `(index ,var ,_) rspec
      var
    (error "Not a ranging spec: ~A" rspec)))

(defun ranging-info (rspec)
  (ifmatch `(index ,_ ,info) rspec
      info
    (error "Not a ranging spec: ~A" rspec)))

(defun ranging-order-flag (rspec)
  (ifmatch (ranging-spec _ :ordered-p flag) rspec
      flag
    (error "Not a ranging spec: ~A" rspec)))

(defun ranging-loop-level (rspec)
  (ifmatch (ranging-spec _ :loop-level level) rspec
      level
    (error "Not a ranging spec: ~A" rspec)))

(def-rewrite-pass remove-ranges ()
  (`(index ,var ,_) var))

(defun copy-ranging (expr)
  (letmatch `(index ,var ,info) expr
    `(index ,var ,(copy-range info))))

;;; Factored expression helpers

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

;;; Fixed index expression predicate

(defun index-expr-p (expr)
  (or (numberp expr)
      (and (consp expr)
           (find (car expr)
                 '(+ - * / 1+ 1- floor ceiling mod rem truncate index)))))

;;; Tree walker for skipping structure

(defun map-skipping-structure (func expr)
  (match expr
    (`(,(as op (or 'let* 'let 'symbol-macrolet)) ,assns ,@code)
      (list*-save-old expr
                      op
                      (mapcar-save-old #'(lambda (assn)
                                           (list-save-old assn
                                                          (first assn)
                                                          (funcall func (second assn))))
                                       assns)
                      (mapcar-save-old func code)))
    (`(safety-check ,checks ,@body)
      (list*-save-old expr
                      'safety-check
                      (mapcar-save-old #'(lambda (check)
                                           (cons-save-old check
                                                          (funcall func (car check))
                                                          (cdr check)))
                                       checks)
                      (mapcar-save-old func body)))
    (`(temporary ,name ,dims ,@tail)
      (list*-save-old expr
                      'temporary name
                      (mapcar-save-old func dims)
                      tail))
    (_
      (mapcar-save-old func expr))))

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
      ;; (format t "Unknown structure statement: ~A" expr)
      (apply fun expr args))))

;;; Misc

(defun range-band-master (range)
  (let ((idx (second range)))
    (or (get idx 'band-master)
        idx)))

(defun prepend-loop-item (rloop entry)
  (setf (cddr rloop)
        (cons entry (cddr rloop))))

(defun append-loop-item (rloop entry)
  (nconc rloop (list entry)))
