;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun replace-let (let-data replace-tbl)
  (let ((new-defs (mapcar-save-old
                   #'(lambda (item)
                       (cons-save-old item
                                      (car item)
                                      (replace-unquoted (cdr item) replace-tbl)))
                   (car let-data)))
        (new-table (set-difference replace-tbl (car let-data) :key #'car)))
    (cons-save-old let-data
                   new-defs
                   (mapcar-save-old #'(lambda (subexpr)
                                        (replace-unquoted subexpr new-table))
                                    (cdr let-data)))))

(defun replace-let* (let-data replace-tbl)
  (let* ((new-table replace-tbl)
         (new-defs (mapcar-save-old
                    #'(lambda (item)
                        (let ((newv (replace-unquoted (cdr item) new-table)))
                          (setf new-table
                                (remove (car item) new-table :key #'car))
                          (cons-save-old item (car item) newv)))
                    (car let-data))))
    (cons-save-old let-data
                   new-defs
                   (mapcar-save-old #'(lambda (subexpr)
                                        (replace-unquoted subexpr new-table))
                                    (cdr let-data)))))

(defun replace-unquoted (expr replace-tbl)
  (let ((target (cdr (assoc expr replace-tbl))))
    (cond
      (target target)
      ((atom expr) expr)
      ((null replace-tbl) expr)
      ((eql (first expr) 'quote) expr)
      ((eql (first expr) 'let)
       (cons-save-old expr
                      'let (replace-let (cdr expr) replace-tbl)))
      ((eql (first expr) 'symbol-macrolet)
       (cons-save-old expr
                      'symbol-macrolet (replace-let (cdr expr) replace-tbl)))
      ((eql (first expr) 'let*)
       (cons-save-old expr
                      'let* (replace-let* (cdr expr) replace-tbl)))
      (t (cons-save-old expr
                        (replace-unquoted (car expr) replace-tbl)
                        (replace-unquoted (cdr expr) replace-tbl))))))

(defun wrap-progn (code)
  (if (cdr code) `(progn ,@code) (car code)))

(defun wrap-progn-filter (code)
  (wrap-progn
   (mapcan #'(lambda (expr)
               (match expr
                 ('nil nil)
                 (`(declare ,@_) nil)
                 (`(progn ,@code) code)
                 (_ (list expr))))
           code)))

(defun convert-letv-exprs (exprs &key pull-last)
  (let* ((elst (if (and (consp exprs) (eql (car exprs) 'progn))
                   (cdr exprs)
                   (list exprs)))
         (nonull (remove-if-not #'identity elst))
         (body   (if pull-last (last nonull) nil))
         (noblk  (if pull-last (butlast nonull) nonull))
         (items  (mapcar
                  #'(lambda (expr)
                      (match expr
                        (`(setf ,(type symbol var) ,vexpr)
                          (list var vexpr))
                        (_
                          (error "letv: invalid expression ~A" expr))))
                  noblk)))
    (values items body)))

(defun convert-letv-exprs-auto (exprs)
  (if (or (eql (car exprs) 'progn)
          (eql (car exprs) 'setf))
      (convert-letv-exprs exprs)
      exprs))

(defmacro letv (exprs &body blk)
  (multiple-value-bind
        (items body)
      (convert-letv-exprs exprs :pull-last (null blk))
    (if items
        `(let* ,items ,@(or blk body))
        (wrap-progn (or blk body)))))

(defun wrap-with-let (with expr)
  (if with
      `(let* ,(convert-letv-exprs-auto with) ,expr)
      expr))

(defun wrap-symbol-macrolet (with body)
  (if with
      `(symbol-macrolet ,with ,body)
      body))

(defun expand-let-1 (expr table)
  (or (gethash expr table)
      (match expr
        ((type atom _) expr)
        (`(quote ,@_) expr)
        (`(declare ,@_) expr)
        (`(,(or 'let 'symbol-macrolet) ,vars ,@body)
          (with-hash-update table
            (mapcar #'(lambda (x)
                        (cons (first x)
                              (expand-let-1 (second x) table)))
                    vars)
            (expand-let-1 (wrap-progn-filter body) table)))
        (`(let* () ,@body)
          (expand-let-1 (wrap-progn-filter body) table))
        (`(let* (,vspec ,@vars) ,@body)
          (expand-let-1 `(let (,vspec) (let* (,@vars) ,@body)) table))
        (`(letv ,vars ,@body)
          (expand-let-1 (macroexpand-1 expr) table))
        (`(progn ,@code)
          (wrap-progn-filter
           (mapcar #'(lambda (x)
                       (expand-let-1 x table))
                   code)))
        (_
          (mapcar-save-old #'(lambda (e) (expand-let-1 e table))
                           expr)))))

(defun expand-let (expr)
  (expand-let-1 expr (make-hash-table)))
