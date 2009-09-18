;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(use-std-readtable)

;; Possible sign information

(defstruct sign-info (negative t) (zero t) (positive t))

(defun sign-info-strictly-positive (info)
  (and (sign-info-positive info)
       (not (sign-info-negative info))
       (not (sign-info-zero info))))

(defun sign-info-strictly-negative (info)
  (and (sign-info-negative info)
       (not (sign-info-positive info))
       (not (sign-info-zero info))))

;; Retrieve sign hints from symbols

(defun symbol-sign-info (sym)
  (if-let (hint (get sym 'sign-hint))
          (ecase hint
            (:positive (make-sign-info :zero nil :negative nil))
            (:negative (make-sign-info :zero nil :positive nil))
            (:nonzero  (make-sign-info :zero nil))
            (:non-positive (make-sign-info :positive nil))
            (:non-negative (make-sign-info :negative nil)))
          (make-sign-info)))

;; Recursively determine sign

(defun get-sign-info (expr)
  (match expr
    ((type number num)
      (cond ((= num 0)
             (make-sign-info :positive nil :negative nil))
            ((> num 0)
             (make-sign-info :zero nil :negative nil))
            (t
             (make-sign-info :positive nil :zero nil))))

    ((type symbol sym)
      (symbol-sign-info sym))

    (`(,(or 'multivalue-data 'aref 'iref)
        ,x ,@_)
      (get-sign-info x))

    (`(ptr-deref ,ptr)
      (get-sign-info (get-ref-root ptr)))

    (`(,(or 'texture-ref 'texture-ref-int) ',name ,@_)
      (get-sign-info name))

    (`(arr-dim ,@_)
      (make-sign-info :zero nil :negative nil))

    ((ranging-spec _ :min (type number minv))
      (cond ((> minv 0)
             (make-sign-info :zero nil :negative nil))
            ((= minv 0)
             (make-sign-info :negative nil))
            (t
             (make-sign-info))))

    (`(,(or '/ '*) ,@mulargs)
      (nlet ((infos (mapcar #'get-sign-info mulargs))
             ((zero         (some #'sign-info-zero infos))
              (nonzero      (every (lambda (x) (or (sign-info-negative x)
                                                   (sign-info-positive x)))
                                   infos))
              (may-neg-cnt  (count-if #'sign-info-negative infos))
              (must-neg-cnt (count-if (lambda (x) (and (sign-info-negative x)
                                                       (not (sign-info-positive x))))
                                      infos))
              ((neg-or-pos-cnt (- may-neg-cnt must-neg-cnt))
               (base-positive  (= (mod must-neg-cnt 2) 0)))))
        (make-sign-info :zero zero
                        :positive (or (> neg-or-pos-cnt 0)
                                      (and nonzero base-positive))
                        :negative (or (> neg-or-pos-cnt 0)
                                      (and nonzero (not base-positive))))))

    (`(- ,x)
      (let ((info (get-sign-info x)))
        (make-sign-info :zero (sign-info-zero info)
                        :positive (sign-info-negative info)
                        :negative (sign-info-positive info))))

    (`(- ,x ,@rest)
      (get-sign-info `(+ ,x (- (+ ,@rest)))))

    (`(+ ,@sumargs)
      (nlet ((infos (mapcar #'get-sign-info sumargs))
             ((all-zero (every #'sign-info-zero infos))
              (positive (some #'sign-info-positive infos))
              (negative (some #'sign-info-negative infos))))
        (cond ((and positive (not negative))
               (make-sign-info :negative nil
                               :zero all-zero))
              ((and negative (not positive))
               (make-sign-info :positive nil
                               :zero all-zero))
              ((and (not positive) (not negative))
               (make-sign-info :positive nil
                               :negative nil))
              (t
               (make-sign-info)))))

    (_
      (make-sign-info))))

;; Filter an expression, removing factors with constant sign

(defcontext ifsign-expr-filter (full-expr)
  (deflex top-negate nil)
  (deflex eliminated-factors nil)

  (defun toggle-negate ()
    (setf top-negate (not top-negate)))

  ;; Assumes flattened representation
  (defun strip-wrappers (expr)
    (match expr
      (`(- ,x)
        (toggle-negate)
        (strip-wrappers x))
      (`(/ ,x)
        (strip-wrappers x))
      (_
        expr)))

  (defun process-factors (items)
    (let (new-items)
      (dolist (item items)
        (let ((info (get-sign-info item)))
          (cond
            ((sign-info-strictly-positive info)
             (push item eliminated-factors))
            ((sign-info-strictly-negative info)
             (toggle-negate)
             (push item eliminated-factors))
            (t
             (push item new-items)))))
      (cond
        ((null new-items)
         1)
        ((null (cdr new-items))
         (strip-wrappers (car new-items)))
        (t
         `(* ,@(nreverse new-items))))))

  (defun process-expr (expr)
    (match (strip-wrappers expr)
      (`(* ,@items)
        (process-factors items))
      (_
        (process-factors (list expr))))))

;; ifsign rewrite passes

(def-rewrite-pass optimize-ifsign (:canonic t
                                   :fallback-to flatten-exprs)
  (`(ifsign ,e ,n ,z ,p)
    (with-context (ifsign-expr-filter e)
      (let ((new-expr (process-expr e)))
        (when top-negate
          (rotatef n p))
        (if (numberp new-expr)
            (progn
              (format t "Sign fully resolved as ~A: ~A~%"
                      (if top-negate (- new-expr) new-expr)
                      (trivialize-refs e))
              (ifsign new-expr n z p))
            (progn
              (when eliminated-factors
                (format t "Eliminated fixed-sign factors:~{ ~A~}~%"
                        (mapcar #'trivialize-refs eliminated-factors)))
              (format t "Keeping ifsign condition: ~A~%"
                      (trivialize-refs new-expr))
              `(ifsign ,new-expr ,n ,z ,p)))))))

(def-rewrite-pass expand-ifsign (:canonic t)
  (`(ifsign ,e ,n ,z ,p)
    `(if (< ,e 0) ,n (if (> ,e 0) ,p ,z))))
