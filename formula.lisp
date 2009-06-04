(defpackage formula
    (:documentation "A reader macro to support Waterloo Maple infix expressions in code")
    (:export "*INDEX-ACCESS-SYMBOL*" "ENABLE-EXPR-QUOTES"))

;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package formula)

;;; Symbol to use for array indexing
(defparameter *index-access-symbol* 'aref)

;;; Read characters that match cond-p as a string
(defun read-string (cond-p stream recursive-p)
    (do ((c     (read-char stream nil nil recursive-p)
                (read-char stream nil nil recursive-p))
         (lst   ()
                (cons c lst)))
        ((or (null c) (not (funcall cond-p c)))
            (when c (unread-char c stream))
            (coerce (reverse lst) 'string))))

;;; Allow underscores in identifiers
(defun ident-char-p (c) (or (alpha-char-p c) (eql c #\_)))
(defun ident-num-char-p (c) (or (alphanumericp c) (eql c #\_)))

(defun append-str-char (s c)
    (concatenate 'string s (coerce (list c) 'string)))

;;; Read a token from the stream
(defun read-token (stream &optional (eof-error-p t) eof-value recursive-p)
    (let ((c (peek-char t stream eof-error-p eof-value recursive-p)))
        (cond
            ;; Number
            ((or (digit-char-p c) (eql c #\.))
                (let ((num-text (read-string
                                    #'(lambda (c) (or (digit-char-p c)
                                                      (eql c #\.)))
                                    stream recursive-p))
                      (next-c (peek-char nil stream nil nil recursive-p)))
                    (when (find next-c '(#\E #\e)) ; Exponent
                        (read-char stream nil nil recursive-p)
                        (setq num-text (append-str-char num-text next-c)
                              next-c (peek-char nil stream nil nil recursive-p))
                        (when (find next-c '(#\+ #\-)) ; Allow sign after exponent
                            (read-char stream nil nil recursive-p)
                            (setq num-text (append-str-char num-text next-c)))
                        (setq num-text ; Eat the actual digits
                            (concatenate 'string num-text
                                (read-string #'digit-char-p stream recursive-p))))
                    (read-from-string num-text)))
            ;; Symbol token
            ((eql c #\$)
                (read-char stream nil nil recursive-p)
                (let* ((name (read-string #'(lambda (cc) (not (eql cc #\$))) stream recursive-p))
                       (split-pos (search ":" name))
                       (package (if split-pos
                                    (string-upcase (subseq name 0 split-pos))
                                    *package*))
                       (ident (if split-pos
                                  (subseq name (1+ split-pos))
                                  name)))
                    (read-char stream nil nil recursive-p)
                    (intern (string-upcase ident) package)))
            ((ident-char-p c)
                (let ((name (read-string #'ident-num-char-p stream recursive-p))
                      (package *package*)
                      (next-c (peek-char nil stream nil nil recursive-p)))
                    ;; Handle package names:
                    (when (eql next-c #\:)
                        (read-char stream nil nil recursive-p)
                        (setq package (string-upcase name))
                        (setq name (read-string #'ident-num-char-p stream recursive-p)))
                    (intern (string-upcase name) package)))
            ;; Comparisons
            ((find c '(#\/ #\< #\> #\! #\:))
                (let ((cc     (read-char stream nil nil recursive-p))
                      (next-c (peek-char nil stream nil nil recursive-p)))
                    (if (eql next-c #\=)
                        (progn
                            (read-char stream nil nil recursive-p)
                            (intern (coerce (list cc next-c) 'string) 'formula))
                        cc)))
            ;; Any other character
            (t (read-char stream eof-error-p eof-value recursive-p)))))

;;; Reads tokens until a certain one is reached
(defun read-tokens-until (end stream &optional recursive-p)
    (do ((item (read-token stream t nil recursive-p)
               (read-token stream t nil recursive-p))
         (lst  ()
               (cons item lst)))
        ((eql item end)
            (reverse lst))))

;;; Read a comma-delimited list of expressions
(defun parse-expr-list (tokens &optional lst)
    (multiple-value-bind (expr tail) (parse-expr tokens)
        (if (eql (car tail) #\,)
            (parse-expr-list (cdr tail) (cons expr lst))
            (values (reverse (cons expr lst)) tail))))

;;; Read an expression and eat a token after it
(defun parse-wrapped (tokens parser rbrace msg &optional (wrapper #'identity))
    (multiple-value-bind (rv tail) (funcall parser (cdr tokens))
        (unless (eql (car tail) rbrace)
            (error "Expecting '~A' after ~A, '~A' found" rbrace msg (car tail)))
        (values (funcall wrapper rv) (cdr tail))))

;;; Parse array indexes and function arguments
(defun parse-expr-atom-idx (expr tokens)
    (case (car tokens)
        (#\[
            (parse-wrapped tokens #'parse-expr-list #\] "index list"
                #'(lambda (indexes) `(,*index-access-symbol* ,expr ,@indexes))))
        (#\(
            (parse-wrapped tokens #'parse-expr-list #\) "argument list"
                #'(lambda (args) `(,expr ,@args))))
        (t
            (values expr tokens))))

;;; Parse atomic expressions
(defun parse-expr-atom (tokens)
    (let ((head (car tokens)))
        (cond
            ((or (symbolp head) (numberp head))
                (parse-expr-atom-idx head (cdr tokens)))
            ((eql head #\()
                (parse-wrapped tokens #'parse-expr #\) "nested expression"))
            (t
                (error "Invalid token '~A' in expression" (car tokens))))))

;;; A macro for binary operator parsing
(defmacro binary-ops (lhs lassoc &rest oplst)
    (let* ((cont-expr (if lassoc 'loop-fun 'values))
           (op-checks
               `(case (car tail)
                    ,@(mapcar #'(lambda (opspec)
                                    `(,(car opspec) ; Operator token
                                         (multiple-value-bind (right-expr tail)
                                             (,(cadr opspec) (cdr tail)) ; Handler function
                                             (,cont-expr ,(caddr opspec) tail)))) ; Expression
                          oplst)
                    (t (values left-expr tail)))))
        (if lassoc
            `(multiple-value-bind (left-expr tail) ,lhs
                (labels ((loop-fun (left-expr tail) ,op-checks))
                    (loop-fun left-expr tail)))
            `(multiple-value-bind (left-expr tail) ,lhs ,op-checks))))

;;; Main recursive descent grammar

(defun parse-expr-pow (tokens)
    (binary-ops (parse-expr-atom tokens) nil
        (#\^ parse-expr-unary `(expt ,left-expr ,right-expr))))

(defun parse-expr-unary (tokens) ; Parse unary + and -
    (case (car tokens)
        (#\+
            (parse-expr-pow (cdr tokens)))
        (#\-
            (multiple-value-bind (pexpr tail) (parse-expr-pow (cdr tokens))
                (values `(- ,pexpr) tail)))
        (t
            (parse-expr-pow tokens))))

(defun parse-expr-mul (tokens)
    (binary-ops (parse-expr-unary tokens) t
        (#\* parse-expr-unary `(* ,left-expr ,right-expr))
        (#\/ parse-expr-unary `(/ ,left-expr ,right-expr))))

(defun parse-expr-add (tokens)
    (binary-ops (parse-expr-mul tokens) t
        (#\+ parse-expr-mul `(+ ,left-expr ,right-expr))
        (#\- parse-expr-mul `(- ,left-expr ,right-expr))))

(defun parse-expr-cmp (tokens)
    (binary-ops (parse-expr-add tokens) nil
        (#\= parse-expr-add `(= ,left-expr ,right-expr))
        (#\< parse-expr-add `(< ,left-expr ,right-expr))
        (<=  parse-expr-add `(<= ,left-expr ,right-expr))
        (>=  parse-expr-add `(>= ,left-expr ,right-expr))
        (!=  parse-expr-add `(/= ,left-expr ,right-expr))
        (/=  parse-expr-add `(/= ,left-expr ,right-expr))
        (#\> parse-expr-add `(> ,left-expr ,right-expr))))

(defun parse-expr-and (tokens)
    (binary-ops (parse-expr-cmp tokens) t
        (#\& parse-expr-cmp `(and ,left-expr ,right-expr))))

(defun parse-expr-or (tokens)
    (binary-ops (parse-expr-and tokens) t
        (#\| parse-expr-and `(or ,left-expr ,right-expr))))

(defun parse-expr (tokens)
    (labels ((read-branch (tokens)
                 (multiple-value-bind (texpr tail)
                     (parse-wrapped (cons nil tokens) ; parse-wrapped ignores car
                         #'parse-expr #\: "conditional")
                     (multiple-value-bind (fexpr tail)
                         (parse-expr tail)
                         (values (cons texpr fexpr) tail)))))
        (binary-ops (parse-expr-or tokens) nil
            (#\? read-branch `(if ,left-expr ,(car right-expr) ,(cdr right-expr))))))

(defun parse-expr-assn (tokens)
    (binary-ops (parse-expr tokens) nil
        (|:=| parse-expr `(setf ,left-expr ,right-expr))))

(defun parse-expr-progn (tokens)
    (binary-ops (parse-expr-assn tokens) t
        (#\; parse-expr-assn
            (if (and (consp left-expr)
                     (eql (car left-expr) 'progn))
                (append left-expr (list right-expr))
                `(progn ,left-expr ,right-expr)))))

;;; A reader macro to parse infix expressions
(defun expr-reader (stream sc arg)
    (let ((tokens (read-tokens-until #\} stream t)))
        (multiple-value-bind (expr tail) (parse-expr-progn tokens)
            (if (null tail)
                expr
                (error "Tokens beyond the end of expression: ~A" tail)))))

(defmacro enable-expr-quotes ()
   `(eval-when (:compile-toplevel :execute)
        (set-dispatch-macro-character #\# #\{ #'expr-reader)))
