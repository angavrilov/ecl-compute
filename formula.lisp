;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(defpackage formula
  (:documentation "A reader macro to support Waterloo Maple infix expressions in code")
  (:use "COMMON-LISP" "ALEXANDRIA" "LEXICAL-CONTEXTS")
  (:export "*INDEX-ACCESS-SYMBOL*" "ENABLE-EXPR-QUOTES"))

(in-package formula)

;;; Symbol to use for array indexing
(defparameter *index-access-symbol* 'aref)

;;; Allow underscores in identifiers
(defun ident-char-p (c) (or (alpha-char-p c) (eql c #\_)))
(defun ident-num-char-p (c) (or (alphanumericp c) (eql c #\_)))

(defun float-num-char-p (c) (or (digit-char-p c) (eql c #\.)))

(defun whitespace-char-p (c)
  (case c
    ((#\space #\return #\linefeed #\tab #\newline) t)
    (t nil)))

(defun append-str-char (s c)
    (concatenate 'string s (coerce (list c) 'string)))

;;; ***** SCANNER *****

(defcontext formula-scanner (stream recursive-p)
  (defun read-c (&optional force)
    (read-char stream force nil recursive-p))

  (defun peek-c (&optional mode force)
    (peek-char mode stream force nil recursive-p))

  (defun unread-c (c)
    (unread-char c stream))

  ;; Read characters that match cond-p as a string
  (defun read-string (cond-p)
    (do ((c     (read-c) (read-c))
         (lst   ()       (cons c lst)))
        ((or (null c) (not (funcall cond-p c)))
         (when c (unread-c c))
         (coerce (reverse lst) 'string))))

  ;; Read a token from the stream
  (defun read-token ()
    (let ((c (peek-c t t))) ; Skip whitespace & require non-eof
      (cond
        ;; Number
        ((or (digit-char-p c) (eql c #\.))
         (let ((num-text (read-string #'float-num-char-p))
               (next-c (peek-c)))
           (when (find next-c '(#\E #\e)) ; Exponent
             (read-c)
             (setq num-text (append-str-char num-text next-c)
                   next-c (peek-c))
             (when (find next-c '(#\+ #\-)) ; Allow sign after exponent
               (read-c)
               (setq num-text (append-str-char num-text next-c)))
             (setq num-text ; Eat the actual digits
                   (concatenate 'string num-text
                                (read-string #'digit-char-p))))
           (read-from-string num-text)))
        ;; Symbol token: quoted
        ((eql c #\$)
         (read-c)
         (if (or (eql (peek-c) #\()  ; $(...) - lisp code splicing
                 (eql (peek-c) #\,)) ; $,...  - lisp antiquotation
             (read stream t nil recursive-p)
             ;; Quoted symbol
             (let* ((name (read-string #'(lambda (cc) (not (eql cc #\$)))))
                    (split-pos (search ":" name))
                    (package (if split-pos
                                 (string-upcase (subseq name 0 split-pos))
                                 *package*))
                    (ident (if split-pos
                               (subseq name (1+ split-pos))
                               name)))
               (read-c t)
               (intern (string-upcase ident) package))))
        ;; Symbol token: plain
        ((ident-char-p c)
         (let ((name (read-string #'ident-num-char-p))
               (package *package*)
               (next-c (peek-c)))
           ;; Handle package names:
           (when (eql next-c #\:)
             (read-c)
             (setq package (string-upcase name))
             (setq name (read-string #'ident-num-char-p)))
           (intern (string-upcase name) package)))
        ;; Comparisons
        ((find c '(#\/ #\< #\> #\! #\:))
         (let ((cc     (read-c))
               (next-c (peek-c)))
           (if (eql next-c #\=)
               (progn
                 (read-c)
                 (intern (coerce (list cc next-c) 'string) 'formula))
               cc)))
        ;; Logical ops
        ((eql c #\&)
         (read-c)
         (let ((nc (peek-c)))
           (if (eql nc c)
               (progn (read-c) '|&&|)
               c)))
        ((eql c #\|)
         (read-c)
         (let ((nc (peek-c)))
           (if (eql nc c)
               (progn (read-c) '|\|\||)
               c)))
        ;; Semicolon
        ((eql c #\;)
         (prog1
             (read-c)
           ;; Semicolons are used as a comment marker by lisp, so disallow
           ;; any non-whitespace between the semicolon and the newline in
           ;; order to avoid confusing syntax highlighting, etc.
           (let ((line (read-line stream nil "" recursive-p)))
             (unless (every #'whitespace-char-p line)
               (error "Semicolon must be followed by a newline in formula: ...;~A" line)))))
        ;; Comment: support #| ... |#
        ((eql c #\#)
         (read-c)
         (if (eql (peek-c) #\|)
             (progn
               (read-c)
               (loop
                  (when (and (eql (read-c t) #\|)
                             (eql (peek-c) #\#))
                    (read-c)
                    (return)))
               (read-token))
             #\#))
        ;; Any other character
        (t (read-c)))))

  ;; Reads tokens until a certain one is reached
  (defun read-tokens-until (end)
    (do ((item (read-token) (read-token))
         (lst  () (cons item lst)))
        ((eql item end)
         (reverse lst)))))

;;; ***** PARSER *****

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
      ((or (symbolp head) (numberp head) (consp head))
       (parse-expr-atom-idx head (cdr tokens)))
      ((eql head #\()
       (parse-wrapped tokens #'parse-expr #\) "nested expression"))
      (t
       (error "Invalid token '~A' in expression" (car tokens))))))

;;; A macro for binary operator parsing
(defmacro binary-ops (lhs lassoc &body oplst)
  "Args: (lhs lassoc &body oplst)"
  (let* ((cont-expr (if lassoc 'loop-fun 'values))
         (rule-list (mapcar #'(lambda (opspec)
                                `(,(first opspec) ; Operator token
                                   (multiple-value-bind (right-expr tail)
                                       (,(second opspec) (cdr tail)) ; Handler function
                                     (,cont-expr ,(third opspec) tail)))) ; Expression
                            oplst))
         (op-checks `(case (car tail)
                       ,@rule-list
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
    (#\+ (parse-expr-pow (cdr tokens)))
    (#\- (multiple-value-bind (pexpr tail) (parse-expr-pow (cdr tokens))
           (values `(- ,pexpr) tail)))
    (#\! (multiple-value-bind (pexpr tail) (parse-expr-pow (cdr tokens))
           (values `(not ,pexpr) tail)))
    (t   (parse-expr-pow tokens))))

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
    (|&&| parse-expr-cmp `(and ,left-expr ,right-expr))))

(defun parse-expr-or (tokens)
  (binary-ops (parse-expr-and tokens) t
    (|\|\|| parse-expr-and `(or ,left-expr ,right-expr))))

(defun parse-expr (tokens)
  (labels ((read-branch (tokens)
             (multiple-value-bind (texpr tail)
                 (parse-wrapped (cons nil tokens) ; parse-wrapped ignores car
                                #'parse-expr #\: "conditional")
               (multiple-value-bind (fexpr tail)
                   (parse-expr tail)
                 (values (list texpr fexpr) tail)))))
    (binary-ops (parse-expr-or tokens) nil
      (#\? read-branch `(if ,left-expr ,@right-expr)))))

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

;;; ***** READER MACRO *****

;;; A reader macro to parse infix expressions
(defun expr-reader (stream sc &optional arg)
  (let ((tokens (with-context (formula-scanner stream t)
                  (read-tokens-until #\}))))
    (multiple-value-bind (expr tail) (parse-expr-progn tokens)
      (if (null tail)
          expr
          (error "Tokens beyond the end of expression: ~A" tail)))))

(defmacro enable-expr-quotes ()
  `(eval-when (:compile-toplevel :execute)
     (set-macro-character #\{ #'expr-reader)
     (set-dispatch-macro-character #\# #\{ #'expr-reader)
     (set-macro-character #\} (get-macro-character #\) nil))))
