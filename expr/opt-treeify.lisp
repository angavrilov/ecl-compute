;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

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

(def-rewrite-pass treeify ()
  (`(+ ,x) x)
  (`(- ,_) expr)
  (`(+ ,@args)
    (treeify+ args))
  (`(* ,@args)
    (treeify* args)))

(defun optimize-tree (expr)
  (pipeline (make-canonic expr)
    flatten-exprs pull-minus pull-factors split-by-level
    canonic-expr-unwrap
    treeify))
