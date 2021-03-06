;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(use-std-readtable)

(def-rewrite-pass pull-minus (:canonic t)
  (`(- (- ,x)) x)
  (`(/ (- ,x)) `(- (/ ,x)))
  (`(- ,(type number val)) (- val))
  (`(* ,@args)
    (let* ((minus-cnt 0)
           (args2 (mapcar #'(lambda (arg)
                              (match arg
                                ((when (< val 0) (type number val))
                                  (incf minus-cnt)
                                  (- val))
                                (`(- ,x)
                                  (incf minus-cnt)
                                  x)
                                ((when (< val 0) `(/ ,(type number val)))
                                  (incf minus-cnt)
                                  `(/ ,(- val)))
                                (`(/ (- ,x))
                                  (incf minus-cnt)
                                  `(/ ,x))
                                (_ arg)))
                          args)))
      (if (> minus-cnt 0)
          (if (= (rem minus-cnt 2) 0)
              (collect* args2)
              `(- ,(collect* args2)))
          expr)))
  (`(+ ,@args)
    (let* ((nums     (remove-if-not #'numberp args))
           (non-nums (remove-if #'numberp args)))
      (if (and non-nums
               (every #'(lambda (arg)
                          (match arg (`(- ,_) t)))
                      non-nums))
          `(- (+ ,@(mapcar #'- nums)
                 ,@(mapcar #'second non-nums)))))))

(defun get-factor-bag (arg)
  (list-to-canonic-bag
   (match arg
     (`(* ,@lst)
       lst)
     (`(- (* ,@lst))
       lst)
     (`(- ,x)
       (list x))
     (_
       (list arg)))))

(defun subtract-factor-bag (arg factors)
  (labels ((cut-list (lst)
             (let* ((bv (list-to-canonic-bag lst))
                    (diff (canonic-bag-to-list
                           (bag-difference bv factors))))
               (cond
                 ((null diff) 1)
                 ((null (cdr diff)) (car diff))
                 (t `(* ,@diff))))))
    (match arg
      (`(* ,@lst)
        (cut-list lst))
      (`(- (* ,@lst))
        `(- ,(cut-list lst)))
      (`(- ,x)
        `(- ,(cut-list (list x))))
      (_
        (cut-list (list arg))))))

(def-rewrite-pass pull-factors (:canonic t)
  (`(+ ,@args)
    (let* ((arg-factors (mapcar #'get-factor-bag args))
           (common-prod (if arg-factors
                            (reduce #'intersection arg-factors)
                            (empty-bag))))
      (if (nonempty? common-prod)
          (collect*
           (list* (flatten+ (mapcar #f(subtract-factor-bag _ common-prod) args))
                  (convert 'list common-prod)))
          (collect+ args))))
  (`(* ,@args)
    (flatten* args)))
