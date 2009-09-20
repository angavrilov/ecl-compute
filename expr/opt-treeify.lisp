;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(use-std-readtable)

(defvar *treeify-madd* nil)

(defun split-parts (lst)
  (let* ((len (length lst))
         (half (floor len 2)))
    (do ((head lst (cdr head))
         (part nil (cons (car head) part))
         (i 0 (1+ i)))
        ((= i half)
         (values (nreverse part) head)))))

(defun treeify+ (args)
  (if (null (cdr args))
      (car args)
      (multiple-value-bind (a b) (split-parts args)
        (match (first b)
          (`(- ,_)
            `(- ,(treeify+ a)
                ,(treeify+ (mapcar #'toggle-minus b))))
          (_
            `(+ ,(treeify+ a)
                ,(treeify+ b)))))))

(defun has-mul-p (x)
  (match x
    (`(* ,@_) t)))

(defun group-madd (bag)
  (let* ((mul-exs (filter #f(canonic-in has-mul-p _) bag))
         (muls (image #f(cons 1 _) mul-exs))
         (nmuls (image #f(cons 1 _) (bag-difference bag mul-exs))))
    (while (and (not (empty? muls))
                (> (+ (size muls) (size nmuls)) 1))
      (let ((mulv (removef-least muls))
            (addv (if (empty? nmuls)
                      (removef-least muls)
                      (removef-least nmuls))))
        (adjoinf nmuls
                 (cons (1+ (max (car mulv) (car addv)))
                       (make-canonic `(+ ,(cdr mulv)
                                         ,(cdr addv)))))))
    (canonic-bag-to-list (image #'cdr (union muls nmuls)))))

(defun merge-shuffle (arr1 arr2)
  (cond ((null arr1) arr2)
        ((null arr2) arr1)
        (t (list* (car arr1) (car arr2)
                  (merge-shuffle (cdr arr1) (cdr arr2))))))

(defun treeify-madd (args)
  (nlet ((minus plus (split-list #'has-minus-p args))
         ((positive (list-to-canonic-bag plus))
          (negative (list-to-canonic-bag (mapcar #'toggle-minus minus)))))
    (treeify+ (merge-shuffle (group-madd positive)
                             (mapcar #'toggle-minus
                                     (group-madd negative))))))

(defun treeify* (args)
  (labels ((is-div (x) (match x (`(/ ,_) t)))
           (do-tree (args)
             (if (null (cdr args))
                 (car args)
                 (multiple-value-bind (a b) (split-parts args)
                   `(* ,(do-tree a) ,(do-tree b))))))
    (let ((muls (remove-if #'is-div args))
          (divs (mapcar #'second
                        (remove-if-not #'is-div args))))
      (if divs
          (progn
            (unless muls
              (push 1 muls))
            `(/ ,(do-tree muls) ,(do-tree divs)))
          (do-tree muls)))))

(def-rewrite-pass treeify (:canonic t)
  (`(+ ,x) x)
  (`(- ,_) expr)
  (`(+ ,@args)
    (if *treeify-madd*
        (treeify-madd args)
        (treeify+ args)))
  (`(* ,@args)
    (treeify* args)))

(defun preoptimize-tree (expr)
  (pipeline expr
    flatten-exprs pull-minus pull-factors
    optimize-ifsign expand-ifsign
    split-by-level split-by-cse))

(defun optimize-tree (expr)
  (pipeline (make-canonic expr)
    flatten-exprs pull-minus pull-factors
    treeify canonic-expr-unwrap))
