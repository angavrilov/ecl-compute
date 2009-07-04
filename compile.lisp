;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun expand-aref-1 (expr old-expr)
    (match expr
        (`(aref ,name ,@idxvals)
            (let* ((idx-cnt    (length idxvals))
                   (stride     nil)
                   (stride-lst
                       (loop for i from (1- idx-cnt) downto 0
                        collect (prog1 stride
                                   (let ((cstride `(arr-dim ,name ,i)))
                                       (setf stride
                                           (if stride
                                               `(* ,stride ,cstride)
                                               cstride))))))
                   (ofs-lst (mapcar #'(lambda (idx istride)
                                          (if istride `(* ,idx ,istride) idx))
                                idxvals (nreverse stride-lst)))
                   (ofs-expr (simplify-rec-once #'flatten-exprs-1 `(+ ,@ofs-lst)))
                   (ofs-items (match ofs-expr (`(+ ,@rest) rest) (x (list x))))
                   (num-ofs-items (remove-if-not #'numberp ofs-items))
                   (var-ofs-items (remove-if #'numberp ofs-items))
                   (levels   (sort
                                 (remove-duplicates
                                     (mapcar #'min-loop-level var-ofs-items))
                                 #'level>))
                   (ofs-groups (mapcar
                                   #'(lambda (lvl)
                                         (simplify-rec-once #'treeify-1
                                             `(+ ,@(remove lvl var-ofs-items
                                                       :test-not #'eql :key #'min-loop-level))))
                                    levels)))
                `(ptr-deref
                     ,(reduce #'(lambda (base ofs) `(ptr+ ,base ,ofs))
                          (nconc ofs-groups num-ofs-items)
                          :initial-value `(arr-ptr ,name)))))
        (`(tmp-ref ,name)
            nil)
        (`(tmp-ref ,name ,@idxvals)
            (let ((rexpr (expand-aref-1 `(aref ,name ,@idxvals) old-expr)))
                (simplify-index
                    (simplify-rec-once
                        #'(lambda (expr old-expr)
                              (match expr
                                  (`(arr-ptr (temporary ,@_)) (second expr))
                                  (`(arr-dim (temporary ,_ ,dims ,@_) ,i)
                                      (nth i dims))))
                        rexpr))))
        (_ nil)))

(defun expand-aref (expr)
    (simplify-rec-once #'expand-aref-1 expr))

(defun expand-macros (expr)
    (match expr
        ((type atom _) expr)
        (`(declare ,@_) expr)
        (`(multivalue-data ,@_) expr)
        (`(1+ ,v)
            (expand-macros `(+ ,v 1)))
        (`(1- ,v)
            (expand-macros `(- ,v 1)))
        (`(expt ,v 1)
            (expand-macros v))
        (`(expt ,v 2)
            (expand-macros `(* ,v ,v)))
        (`(temporary ,name ,dims ,@rest)
            `(temporary ,name
                 ,(mapcar #'expand-macros dims)
                 ,@rest))
        ((cons (or 'ranging 'aref 'iref '_grp 'tmp-ref 'quote
                   '+ '- '* '/ 'mod 'rem 'floor 'ceiling 'truncate
                   'and 'or 'if 'progn
                   'sin 'cos 'exp 'expt
                   '> '< '>= '<= '/= '= 'min 'max 'setf 'loop-range) tail)
            (cons-save-old expr (car expr)
                (mapcar-save-old #'expand-macros tail)))
        (`(,(as op (or 'let 'let* 'symbol-macrolet)) ,vars ,@body)
            (cons-save-old expr op
                (cons-save-old (cdr expr)
                    (mapcar-save-old
                        #'(lambda (pair)
                              (if (symbolp pair) (cons pair nil)
                                  (cons-save-old pair
                                      (car pair)
                                      (mapcar-save-old #'expand-macros (cdr pair)))))
                        vars)
                    (mapcar-save-old #'expand-macros body))))
        (_
            (multiple-value-bind (res macro) (macroexpand expr)
                (if macro
                    (expand-macros res)
                    (error "Unknown form in compute: ~A" res))))))

(defun get-inline-tag (idx)
    (aref #("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
            "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
            "u" "v" "w" "x" "y" "z") idx))


(defparameter *cg-type-table* nil)
(defparameter *cg-full-expr* nil)

(defun do-make-form-compiler (form-arg args body)
   `(let ((,form-arg cg-form)
          ,@(mapcar
                #'(lambda (sym)
                      `(,sym
                           ,(if (equal (symbol-name sym)
                                    (format nil "~A-TYPE" form-arg))
                                '(if *cg-type-table*
                                     (gethash cg-form *cg-type-table*))
                                'nil)))
                args))
        (declare (ignorable ,form-arg))
        ,@(if args
              `((do ((aptr cg-flags (cddr aptr)))
                    ((null aptr))
                    (case (car aptr)
                        ,@(mapcar
                              #'(lambda (sym)
                                    `(,(intern (symbol-name sym) "KEYWORD")
                                         (setf ,sym (cadr aptr))))
                              args)))))
        (macrolet
                ((text (pattern &rest args)
                     (if (and (stringp pattern)
                             (or args
                                 (position #\~ pattern)))
                         `(format cg-output (formatter ,pattern) ,@args)
                         (progn
                             (assert (null args))
                             `(write-string ,pattern cg-output))))
                 (recurse (expr &rest flags)
                     (let ((use-stack 'cg-full-stack))
                         (when (find (car flags)
                                   '(:use-stack :switch-stack))
                             (setf use-stack (cadr flags))
                             (setf flags (cddr flags)))
                        `(let ((stack ,use-stack))
                             (funcall (car stack)
                                 stack (cdr stack)
                                 cg-output ,expr (list ,@flags)))))
                 (recurse-str (&rest args)
                     `(with-output-to-string (cg-output)
                          (recurse ,@args))))
            (match cg-form
                ,@body
                (_
                    (if cg-cur-stack
                        (funcall (car cg-cur-stack)
                            cg-full-stack (cdr cg-cur-stack)
                            cg-output cg-form cg-flags)
                        (error "Cannot compile, unrecognized form: ~A" cg-form)))))))

(defmacro form-compiler (arg-set &body body)
    (assert (consp arg-set))
    `#'(lambda (cg-full-stack cg-cur-stack cg-output cg-form cg-flags)
           ,(do-make-form-compiler (car arg-set) (cdr arg-set) body)))

(defmacro def-form-compiler (name arg-set &body body)
    (assert (consp arg-set))
    `(defun ,name (cg-full-stack cg-cur-stack cg-output cg-form cg-flags)
         ,(do-make-form-compiler (car arg-set) (cdr arg-set) body)))

(defun call-form-compilers (stack form &rest flags)
    (with-output-to-string (out)
        (funcall (car stack)
            stack (cdr stack) out
            form flags)))


(defun temp-symbol-name (sym)
    (assert (get sym 'let-clause))
    (concatenate 'string "tmp_" (symbol-name sym)))

(def-form-compiler compile-generic (form stmt-p)
    (`(declare ,@_)
        (unless stmt-p
            (text "0")))

    (`(_grp ,x)
        (recurse x))
    (`(tmp-ref ,x)
        (recurse x))

    (`(let* ,assns ,@body)
        (unless stmt-p
            (error "Let in a non-stmt context: ~A" form))
        (text "{~%")
        (dolist (assn assns)
            (recurse
                `(setf-tmp ,(first assn) ,(second assn))))
        (dolist (cmd body)
            (recurse cmd :stmt-p t))
        (text "}~%"))

    ((when stmt-p
         `(progn ,@body))
        (dolist (cmd body)
            (recurse cmd :stmt-p t)))

    (`(progn ,cmd1 ,@rest)
        (text "(")
        (recurse cmd1)
        (dolist (cmd rest)
            (text ",~%")
            (recurse cmd))
        (text ")")))

(def-form-compiler compile-generic-c (form form-type stmt-p)
    ((type float fv)
        (text "~Sf" fv))

    ((type number nv)
        (text "~S" nv))

    (`(ranging ,v ,@_)
        (if (ranging-loop-level form)
            (text (symbol-name v))
            (recurse v)))

    ((when (and (eql form-type 'float)
                (> (or (get sym 'fdiv-users) 0) 1))
         `(/ ,x ,(type symbol sym)))
        (unless (and (numberp x)
                     (= x 1))
            (text "(")
            (recurse x)
            (text ")*"))
        (text "~A_fdiv" (temp-symbol-name sym)))

    ((when (eql form-type 'float)
        `(/ ,x))
        (text "1.0f/(")
        (recurse x)
        (text ")"))

    (`(,(as op (or '+ '- '* '/ 'truncate 'rem 'ptr+
                   'and 'or '> '< '>= '<= '/= '=)) ,a ,b)
        (text "(")
        (recurse a)
        (text ")~A("
            (case op
                ((+ - * / < > >= <=) op)
                (/= "!=")
                (= "==")
                (ptr+ "+")
                (and "&&")
                (or "||")
                (truncate "/")
                (rem "%")))
        (recurse b)
        (text ")"))

    (`(,(as op (or '+ '-)) ,a)
        (text "~A(" op)
        (recurse a)
        (text ")"))

    (`(,(as op (or 'max 'min)) ,a ,b)
        (text "(((")
        (recurse a)
        (text ")~A("
            (if (eql op 'min) "<" ">"))
        (recurse b)
        (text "))?(")
        (recurse a)
        (text "):(")
        (recurse b)
        (text "))"))

    ;; This is actually incorrect, because integer division
    ;; in C is equivalent to truncate, but we use them only
    ;; for positive numbers, where there is no difference.
    ;; However, as a special case, for powers of 2 we can
    ;; precisely emulate floor&rem using bit operations.
    ((when (and (eql form-type 'integer)
                (= (logand b (1- b)) 0))
        `(,(as op (or 'floor 'mod))
                 ,a ,(type integer b)))
        (text "(((int)(")
        (recurse a)
        (if (eql op 'floor)
            (text "))>>~A)"
                (do ((cnt 0 (1+ cnt))
                     (bv b (ash bv -1)))
                    ((<= bv 1) cnt)))
            (text "))&~A)" (1- b))))

    ((when (eql form-type 'integer)
        `(,(as op (or 'floor 'mod)) ,a ,b))
        (text "((int)((")
        (recurse a)
        (text ")~A("
            (case op
                (floor "/")
                (mod "%")))
        (recurse b)
        (text ")))"))

    (`(,(as func (or 'floor 'ceiling 'sin 'cos 'exp 'expt))
          ,arg ,@rest)
        (text "~A("
            (ecase func
                (expt "powf")
                (ceiling "ceilf")
                (floor "floorf")
                (sin "sinf")
                (cos "cosf")
                (exp "expf")))
        (recurse arg)
        (dolist (arg2 rest)
            (text ", ")
            (recurse arg2))
        (text ")"))

    (`(ptr-deref ,ptr)
        (text "*(")
        (recurse ptr)
        (text ")"))

    ((when stmt-p
         `(if ,icond ,a ,b))
        (text "if (")
        (recurse icond)
        (text ") {~%")
        (recurse a :stmt-p t)
        (text "} else {~%")
        (recurse b :stmt-p t)
        (text "}~%"))

    (`(if ,icond ,a ,b)
        (text "(")
        (recurse icond)
        (text ") ? (")
        (recurse a)
        (text ") : (")
        (recurse b)
        (text ")"))

    (`(setf ,target ,expr)
        (recurse target)
        (text " = (")
        (recurse expr)
        (text ")")
        (when stmt-p
            (text ";~%"))))

(def-form-compiler compile-c-inline-temps (form)
    (`(setf-tmp ,var (temporary ',name ,dims ,@_))
        (let* ((const-dim (if (and dims (every #'numberp dims))
                              (reduce #'* dims)))
               (is-const (and const-dim (< const-dim 65536))))
            (text "float")
            (unless (or (null dims) is-const)
                (text "*"))
            (text " ~A" (temp-symbol-name var))
            (cond
                (is-const
                    (text "[~A]" const-dim))
                (dims
                    (text " = (float*)ecl_alloc_atomic(")
                    (recurse
                        (reduce #'(lambda (a b) `(* ,a ,b)) dims))
                    (text ")")))
            (text "; /* ~A */~%" name)))

    (`(setf-tmp ,var ,expr)
        (let ((var-type (gethash expr *cg-type-table*))
              (var-name (temp-symbol-name var))
              (var-fdiv (or (get var 'fdiv-users) 0)))
            (text "~A ~A = ("
                (match var-type
                    ('array "cl_object")
                    ('float "float")
                    ('float-ptr "float*")
                    ('integer "int")
                    ('boolean "int")
                    (_ (error "Bad type ~A of ~A in ~A"
                           var-type expr *cg-full-expr*)))
                var-name)
            (recurse expr)
            (text ");~%")
            (when (> var-fdiv 1)
                (text "float ~A_fdiv = (1.0f/~A);~%"
                    var-name var-name)))))
