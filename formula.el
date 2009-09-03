;;; -*- mode:emacs-lisp; indent-tabs-mode: nil; -*-
;;
;; This defines a special indentation mode for lisp files
;; that use the infix formula input syntax extension.
;;

(defun formula-lisp-indent-function (indent-point state)
  (let (formula-start formula-column)
    (if (save-excursion ; Find out if we are inside {}
          (goto-char indent-point)
          (skip-chars-backward "^{}")
          (and (looking-back "{")
               (progn
                 (backward-char)
                 (setq formula-start (point))
                 (setq formula-column (current-column)))))
        (progn
          (goto-char indent-point)
          (let ((last-assn-pos (save-excursion
                                 (search-backward ":=" formula-start t)))
                (last-semi-pos (save-excursion
                                 (search-backward ";" formula-start t)))
                (cur-assn-pos (save-excursion
                                (end-of-line)
                                (search-backward ":=" indent-point t))))
            (let ((offset
                   ;; Indentation pattern:
                   ;; { foo := bar
                   ;;          baz;
                   ;;   xxx := yyy;
                   ;;     zzz }
                   (cond ((and last-assn-pos
                               (or (null last-semi-pos)
                                   (< last-semi-pos last-assn-pos)))
                          ;; RHS of an assignment
                          (save-excursion
                            (goto-char last-assn-pos)
                            (+ 3 (current-column))))

                         ((and last-assn-pos
                               cur-assn-pos)
                          ;; First line of an assignment
                          (+ 2 formula-column))

                         ;; Non-assignment code in a block with assignments:
                         (last-assn-pos (+ 4 formula-column))

                         ;; No assignments at all
                         (t (+ 2 formula-column)))))

              ;; Return the start index to force per-line recalculation
              (list offset formula-start))))

      (common-lisp-indent-function indent-point state))))


;;; Prevent semicolons from being realigned as comments
(defadvice comment-indent (around formula-comment)
  (catch 'quit-formula-comment ad-do-it))

(ad-activate 'comment-indent)

(defun formula-comment-indent ()
  (if (save-excursion ; Find out if we are inside {}
        (skip-chars-backward "^{}")
        (looking-back "{"))
      (throw 'quit-formula-comment nil)
    (lisp-comment-indent)))


;;; Since entering ':=' changes the indentation of an expression,
;;; define a minor mode with an appropriate key hook.
(define-minor-mode formula-mode
  "Toggle Common Lisp + Formula mode.

When Formula mode is enabled, code within {} is indented specially."
  nil " Formula"
  '(("=" . (lambda ()
             (interactive)
             (let ((pchar (preceding-char)))
               (insert "=")
               (if (char-equal pchar ?:)
                   (indent-according-to-mode))))))

  ;; Tie the indentation engine to the mode
  (set (make-local-variable 'lisp-indent-function)
       (if formula-mode
           'formula-lisp-indent-function
         'common-lisp-indent-function))
  (set (make-local-variable 'comment-indent-function)
       (if formula-mode
           'formula-comment-indent
         'lisp-comment-indent)))


;;; Automatically enable the mode if (enable-expr-quotes) is used.
(defun auto-detect-expr-quotes ()
  (if (save-excursion
        (beginning-of-buffer)
        (re-search-forward
         "^[ \t]*([ \t]*\\(?:[a-z-]+:\\)?enable-expr-quotes"
         nil t))
      (formula-mode 1)))

(defun enable-auto-detect-formula ()
  (add-hook 'lisp-mode-hook 'auto-detect-expr-quotes t))


;;; Add {} and [] to the syntax table as sexpr delimiters
(modify-syntax-entry ?{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?} "){" lisp-mode-syntax-table)
(modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?] ")[" lisp-mode-syntax-table)


;;; Indentation rules for macros used in this library

;; Import: ECL FFI
(put 'ffi:c-inline 'common-lisp-indent-function
     '((&whole 4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 &rest 1)
       (&whole nil 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 &rest 1)
       4 &body))

(put 'ffi:def-struct 'common-lisp-indent-function 1)

;; Import: misc-extensions
(put 'nlet 'common-lisp-indent-function
     (or (get 'let 'common-lisp-indent-function)
         '((&whole 4 &rest (&whole 1 1 2)) &body)))

;; Import: cl-match
(put 'match 'common-lisp-indent-function
     '(4 &rest (&whole 2 2 2 2 2 2 2 2 2 &rest 2)))
     ;;; Lots of 2s to work around an apparent bug in &rest

(put 'ifmatch 'common-lisp-indent-function
     '(6 nil 4 &body))

(put 'letmatch 'common-lisp-indent-function
     '(6 nil &body))

;; rewrite engine
(put 'def-rewrite-pass 'common-lisp-indent-function
     '(4 (&whole 4 &rest 1)
         &rest (&whole 2 2 2 2 2 2 2 2 2 &rest 2)))

;; threads
(put 'condition-wait-spin 'common-lisp-indent-function
     '((&whole 4 1 1 1 1 &rest 1) &body))

;; code generation
(put 'def-form-compiler 'common-lisp-indent-function
     '(4 (&whole 4 &rest 1)
         &rest (&whole 2 2 2 2 2 2 2 2 2 &rest 2)))

(put 'form-compiler 'common-lisp-indent-function
     '((&whole 4 &rest 1)
         &rest (&whole 2 2 2 2 2 2 2 2 2 &rest 2)))

;; misc
(put 'use-cache 'common-lisp-indent-function 1)
(put 'pipeline 'common-lisp-indent-function 1)
(put 'do-wrap-loops 'common-lisp-indent-function '(&rest nil))
(put 'error-fallback 'common-lisp-indent-function 2)
(put 'wrap-compute-sync-data 'common-lisp-indent-function 2)


;;; Some tweaks for the indentation of the compute statement.
(put 'compute 'common-lisp-indent-function
     '(4 4 2 &rest 2))

(put ':kernel-name 'common-lisp-indent-function
     '(1 &rest 1)) ; Simple &rest 1 won't work
