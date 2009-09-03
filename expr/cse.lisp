;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(defun count-subexprs-rec (expr cnt-table)
  ;; Increment the counter, and avoid walking trees twice
  (when (> (incf-nil (gethash expr cnt-table)) 1)
    (return-from count-subexprs-rec nil))
  (match expr
    ((type atom var)
      nil)
    (`(index ,@_)
      nil)
    (`(declare ,@_)
      nil)
    (`(multivalue-data ,@_)
      nil)
    (`(loop-range ,(ranging-spec _ :min minv :max maxv) ,@body)
      (count-subexprs-rec minv cnt-table)
      (count-subexprs-rec maxv cnt-table)
      (dolist (item body)
        (count-subexprs-rec item cnt-table)))
    (`(temporary ,_ ,dims ,@_)
      (dolist (titem dims)
        (count-subexprs-rec titem cnt-table)))
    (`(setf ,target ,val)
      (dolist (titem (cdr target))
        (count-subexprs-rec titem cnt-table))
      (count-subexprs-rec val cnt-table))
    (`(safety-check ,checks ,@body)
      (dolist (check checks)
        (count-subexprs-rec (first check) cnt-table))
      (dolist (item body)
        (count-subexprs-rec item cnt-table)))
    (_
      (dolist (item (cdr expr))
        (count-subexprs-rec item cnt-table)))))

(defun count-subexprs (expr)
  (let ((table (make-hash-table :test #'equal)))
    (count-subexprs-rec expr table)
    table))

(defun build-factor-table (cnt-table pull-symbols)
  (let ((fct-table (make-hash-table :test #'equal)))
    (maphash #'(lambda (expr cnt)
                 ;; Factor common subexpressions
                 (when (> cnt 1)
                   (setf (gethash expr fct-table) t))
                 ;; Factor symbols
                 (when (and pull-symbols (symbolp expr))
                   (setf (gethash expr fct-table) t))
                 ;; Factor temporaries and arrays
                 (ifmatch `(,(or 'temporary 'multivalue-data) ,@_) expr
                     (setf (gethash expr fct-table) t))
                 ;; Factor references used on the lhs of an
                 ;; assignment, and also somewhere else
                 (when (and (consp expr)
                            (eql (car expr) 'setf)
                            (>= (or (gethash (second expr) cnt-table) 0) 1))
                   (setf (gethash (second expr) fct-table) t))
                 ;; Factor loop-invariant subexpressions
                 (when (and (consp expr)
                            (not (find (car expr)
                                       '(index safety-check loop-range setf))))
                   (let ((level (min-loop-level expr)))
                     (dolist (sub (cdr expr))
                       (unless (eql level (min-loop-level sub))
                         (setf (gethash sub fct-table) t))))))
             cnt-table)
    (maphash #'(lambda (expr flag)
                 ;; Don't factor constants and loop vars
                 (when (match expr
                         ((type number _) t)
                         ((type string _) t)
                         ('nil t)
                         (`(_grp ,@_) t)
                         ((type symbol var) (not pull-symbols))
                         ((ranging-spec _ :loop-level level) level)
                         (_ nil))
                   (remhash expr fct-table)))
             fct-table)
    fct-table))

(defun factor-vars-dumb (expr fct-table cur-level var-list nil-list)
  (if (or (atom expr) (find (car expr) '(index)))
      expr
      (mapcar-save-old #'(lambda (x)
                           (factor-vars-rec x fct-table
                                            cur-level var-list nil-list))
                       expr)))

(defun factor-vars-rec (expr fct-table cur-level var-list nil-list)
  (match expr
    (`(declare ,@_) expr)
    (`(quote ,@_) expr)
    (`(loop-range ,range ,@body)
      (let* ((range-info (ranging-info range))
             (level (range-loop-level range-info))
             (level-gap (if cur-level (- cur-level level 1) 0))
             (pad-list (loop for i from 1 to level-gap collect nil))
             (vlist (cons nil (nconc pad-list var-list)))
             (nbody (factor-vars-rec body fct-table level vlist nil-list)))
        (unless (or (null cur-level) (< level cur-level))
          (error "Invalid loop nesting: ~A at level ~A" expr cur-level))
        ;; Factor the loop range args
        (setf (range-min range-info)
              (factor-vars-rec (range-min range-info)
                               fct-table cur-level var-list nil-list))
        (setf (range-max range-info)
              (factor-vars-rec (range-max range-info)
                               fct-table cur-level var-list nil-list))
        ;; Pop the substitutions
        (dolist (subs (car vlist))
          (setf (gethash
                 (get (first subs) 'full-expr)
                 fct-table)
                t))
        ;; Verify that the gap is empty
        (loop for i from 1 to level-gap
           do (unless (null (nth i vlist))
                (error "Invalid loop nesting: ~A of level ~A in gap ~A to ~A"
                       (nth i vlist) (+ level i) level cur-level)))
        ;; Wrap with let if needed
        (if (car vlist)
            `(loop-range ,range (let* ,(nreverse (car vlist)) ,@nbody))
            `(loop-range ,range ,@nbody))))
    (`(setf ,target ,val)
      `(setf ,(factor-vars-dumb target fct-table cur-level var-list nil-list)
             ,(optimize-tree
               (factor-vars-rec val fct-table cur-level var-list nil-list))))
    (`(safety-check ,checks ,@body)
      (cons-save-old expr 'safety-check
                     (cons-save-old (cdr expr)
                                    (mapcar-save-old
                                     #'(lambda (check)
                                         (cons-save-old check
                                                        (factor-vars-rec (car check)
                                                                         fct-table cur-level var-list nil-list)
                                                        (cdr check)))
                                     checks)
                                    (factor-vars-dumb body
                                                      fct-table cur-level var-list nil-list))))
    (_
      (let ((factor (gethash expr fct-table)))
        (cond
          ((eql factor nil)
           (factor-vars-dumb expr fct-table cur-level var-list nil-list))
          ((eql factor t)
           (let* ((sym   (get-new-symbol))
                  (nexpr (factor-vars-dumb expr fct-table cur-level var-list nil-list))
                  (level (min-loop-level expr))
                  (expr-pair (list sym (optimize-tree nexpr))))
             (setf (get sym 'let-clause) expr-pair)
             (setf (get sym 'full-expr) expr)
             (setf (get sym 'loop-level) level)
             (cond
               ((eql level nil)
                (push expr-pair (car nil-list)))
               ((and cur-level (>= level cur-level))
                (push expr-pair (nth (- level cur-level) var-list)))
               (t
                (error "Invalid level ~A at current ~A" level cur-level)))
             (setf (gethash expr fct-table) sym)))
          (t  factor))))))

(defun factor-vars (expr fct-table)
  (let* ((nil-list (list nil))
         (nexpr    (factor-vars-rec expr fct-table nil nil nil-list)))
    (if (car nil-list)
        `(let* ,(nreverse (car nil-list)) ,nexpr)
        nexpr)))

(defun code-motion (expr &key pull-symbols)
  (factor-vars expr (build-factor-table (count-subexprs expr) pull-symbols)))
