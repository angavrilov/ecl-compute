;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(import 'fset::empty-2-relation)

(defun unsymbol (x)
  (if (symbolp x) (symbol-name x) x))

(defun symcat (&rest items)
  (intern
   (apply #'concatenate 'string
          (mapcar #'unsymbol items))))

(defmacro removef-least (bag)
  (with-gensyms (lv)
    `(let ((,lv (least ,bag)))
       (removef ,bag ,lv)
       ,lv)))

(defun force-integer (expr)
  (if (integerp expr) expr nil))

(defmacro cond-list* (&body items)
  (let ((ritems (reverse items))
        (xsym (gensym)))
    (reduce #'(lambda (tail clause)
                (cond
                  ((eql (first clause) t)
                   `(list* ,@(rest clause) ,tail))
                  ((eql (first clause) nil)
                   tail)
                  ((eql tail nil)
                   `(if ,(first clause)
                        (list ,@(rest clause))))
                  (t
                   `(let ((,xsym ,tail))
                      (if ,(first clause)
                          (list* ,@(rest clause) ,xsym)
                          ,xsym)))))
            (rest ritems)
            :initial-value (first ritems))))

(defmacro cond-list (&body items)
  `(cond-list* ,@items nil))

(defun cons-save-old (old carv cdrv)
  (if (and (eql carv (car old))
           (eql cdrv (cdr old)))
      old
      (cons carv cdrv)))

(defun mapcar-save-old (fun lst)
  (if (null lst) nil
      (cons-save-old lst
                     (funcall fun (car lst))
                     (mapcar-save-old fun (cdr lst)))))

(defmacro list*-save-old (old &rest elts)
  (if (null (cdr elts))
      (car elts)
      (let ((sym (gensym)))
        `(let ((,sym ,old))
           (cons-save-old ,sym
                          ,(car elts)
                          (list*-save-old (cdr ,sym)
                                          ,@(cdr elts)))))))

(defmacro list-save-old (old &rest elts)
  `(list*-save-old ,old ,@elts nil))

(defun subst-save-old (new old expr)
  (cond
    ((eql expr old)
     new)
    ((atom expr)
     expr)
    (t
     (cons-save-old expr
                    (subst-save-old new old (car expr))
                    (subst-save-old new old (cdr expr))))))

(defun simplify-rec-once (engine expr)
  (let* ((rec-res (if (atom expr)
                      expr
                      (mapcar-save-old #'(lambda (sub)
                                           (simplify-rec-once engine sub))
                                       expr)))
         (subs-res (funcall engine rec-res expr)))
    (if (null subs-res) rec-res subs-res)))

(defun simplify-rec-once-struct (engine expr)
  (labels
      ((process-dumb (expr)
         (match expr
           (`(,(as op (or 'let* 'let)) ,assns ,@code)
             (list*-save-old
              expr op
              (mapcar-save-old #'(lambda (assn)
                                   (list-save-old assn
                                                  (first assn)
                                                  (recurse (second assn))))
                               assns)
              (mapcar-save-old #'recurse code)))
           ((type atom _)
             expr)
           (`(quote ,@_)
             expr)
           (_
             (mapcar-save-old #'recurse expr))))
       (recurse (expr)
         (let* ((rec-res (process-dumb expr))
                (subs-res (funcall engine rec-res expr)))
           (if (null subs-res) rec-res subs-res))))
    (recurse expr)))

(defun apply-hash-change (table vals)
  (mapcar #'(lambda (assn)
              (let ((key (car assn)))
                (prog1
                    (cons key (gethash key table))
                  (setf (gethash key table) (cdr assn)))))
          vals))

(defmacro with-hash-update (table vals &body rest)
  (let ((ss (gensym)))
    `(let ((,ss (apply-hash-change ,table ,vals)))
       (unwind-protect (progn ,@rest)
         (apply-hash-change ,table ,ss)))))

(defun copy-hash-data (new old &rest tables)
  (unless (eql new old)
    (dolist (tbl tables)
      (multiple-value-bind
            (oval found) (gethash new tbl)
        (unless found
          (setf (gethash new tbl)
                (gethash old tbl)))))))

(defun sethash-all (table keys &optional (val t))
  (dolist (item keys table)
    (setf (gethash item table) val)))

(defun hash-set-intersect (table1 table2)
  (do-hashtable (key val table1 table1)
    (unless (gethash key table2)
      (remhash key table1))))

(defun dump-tbl (name hash)
  (format t "~%~%~A:~%" name)
  (maphash #'(lambda (k v)
               (when v
                 (print k)))
           hash))

(define-modify-macro incf-nil (&optional (delta 1))
  (lambda (val delta) (+ (or val 0) delta)))

(defmacro use-cache ((key cache) &body code)
  "Syntax: (use-cache (key cache) &body code)"
  (let ((cached-val (gensym))
        (found (gensym)))
    `(multiple-value-bind (,cached-val ,found) (gethash ,key ,cache)
       (if ,found ,cached-val
           (setf (gethash ,key ,cache)
                 (progn ,@code))))))

(defun set-prop-nochange (sym tag value)
  (let ((old-value (get sym tag)))
    (if (or (null old-value) (equal old-value value))
        (setf (get sym tag) value)
        (error "Cannot redefine ~A for ~A to: ~A~% - already set to: ~A"
               tag sym value old-value))))

(defun has-atom (tree &rest syms)
  (labels ((recurse (tree)
             (if (and tree (consp tree))
                 (some #'recurse tree)
                 (member tree syms))))
    (recurse tree)))

(defun apply-unless-nil (fun args)
  (if (some #'null args)
      nil
      (apply fun args)))

(defmacro pipeline (value &body steps)
  "Args: (value &body steps)
Pipes the value through function calls described by steps.
Each of the steps can be a symbol, or a list. If the list
contains &rest, it specifies the position where the result
of the previous step is passed. Otherwise, it is added as
the last parameter."
  (labels ((step-name (spec)
             (match spec
               (`(,(or 'function 'funcall) ,name ,@_)
                 (step-name name))
               (`(,_ (function ,name) ,@_)
                 (step-name name))
               (`(,head ,@_)
                 (step-name head))
               (_
                 (format nil "P-~A-" spec)))))
    (let* ((init-val (gensym "P-INIT-"))
           (cur-val init-val)
           (step-code
            (mapcar #'(lambda (step)
                        (let* ((new-val (gensym (step-name step)))
                               (step-lst (if (consp step)
                                             (copy-list step)
                                             (list step)))
                               (splice-pos (or (member '&rest step-lst)
                                               (member '_ step-lst))))
                          (if splice-pos
                              (setf (car splice-pos) cur-val)
                              (setf step-lst
                                    (nconc step-lst (list cur-val))))
                          (setf cur-val new-val)
                          (list new-val step-lst)))
                    steps)))
      `(let* ((,init-val ,value)
              ,@step-code)
         ,cur-val))))
