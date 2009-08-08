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

(defun treeify-1 (expr old-expr)
    (match expr
        (`(+ ,x) x)
        (`(- ,_) expr)
        (`(+ ,@args)
            (treeify+ args))
        (`(* ,@args)
            (treeify* args))
        (_ nil)))

(defun optimize-tree (expr)
  (let* ((canonic (make-canonic expr))
         (flat (canonic-simplify-rec-once #'flatten-exprs-1 canonic))
         (no- (canonic-simplify-rec-once #'pull-minus-1 flat))
         (fact (canonic-simplify-rec-once #'pull-factors-1 no-)))
    (simplify-rec-once #'treeify-1 (canonic-expr-unwrap fact))))
