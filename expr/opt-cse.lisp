;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package fast-compute)

(use-std-readtable)

(defun has-minus-p (expr)
  (match expr
    (`(- ,_) t)))

(defun has-div-p (expr)
  (match expr
    (`(/ ,(type number _)) nil)
    (`(/ ,_) t)
    (`(- (/ ,_)) t)))

;; Common subset extractor code

(defcontext common-subset-extract (op #'inv-func #'inv-p)
  ;; Mapping of original canonic-expr's to current content bags
  (deflex work-bags (make-hash-table))

  ;; Set of work-bags keys still in processing
  (deflex active-set (empty-set))

  ;; Queue of subsets to extract, in form (cons penalty bag)
  (deflex pull-queue (empty-set))

  ;; Specifies where the subset comes from (used for efficiency)
  (deflex pull-index (with-default (empty-map) (empty-set))) ; subset bag -> old-expr*

  (defun get-state () ; for debug
    (values (hash-table-alist work-bags)
            active-set pull-queue pull-index))

  ;; Misc. conversions
  (defun invert (expr-bag)
    (image #f(canonic inv-func _) expr-bag))

  (defun bag-to-expr (expr-bag)
    (let ((ilist (convert 'list expr-bag)))
      (if (cdr ilist)
          (make-canonic (list* op ilist))
          (car ilist))))

  (defun expr-to-bag (expr)
    (let ((ce (canonic-expr-unwrap expr)))
      (assert (and (consp ce) (eql (car ce) op)))
      (convert 'bag (mapcar #'make-canonic (cdr ce)))))

  ;; Queue
  (defun bag-queue-descriptor (expr-bag)
    (cons (cons (- (size expr-bag)) ; Prioritize largest
                ;; Prioritize least inverted:
                (count-if #f(canonic-in inv-p _) expr-bag))
          expr-bag))

  (defun dequeue-bag (expr-bag)
    (removef pull-queue (bag-queue-descriptor expr-bag))
    (removef pull-index expr-bag))

  (defun queue-bag (expr-bag &rest exprs)
    (adjoinf pull-queue (bag-queue-descriptor expr-bag))
    (unionf (@ pull-index expr-bag) (convert 'set exprs)))

  (defun get-next-cut ()
    (let* ((head  (least pull-queue))
           (bag   (cdr head))
           (exprs (@ pull-index bag)))
      (removef pull-queue head)
      (removef pull-index bag)
      (values bag exprs)))

  ;; Subset finder
  (defun requeue-expr (expr)
    (let* ((expr-bag (gethash expr work-bags))
           (inv-bag  (invert expr-bag)))
      (when (>= (size expr-bag) 2)
        ;; Iterate active expressions
        (do-set (expr2 active-set)
          (let* ((bag2 (gethash expr2 work-bags))
                 (intf (intersection expr-bag bag2))
                 (inti (intersection inv-bag bag2)))
            ;; Queue forward match
            (when (> (size intf) 1)
              (queue-bag intf expr expr2))
            ;; Queue inverted match
            (when (> (size inti) 1)
              (queue-bag inti expr expr2)
              (queue-bag (invert inti) expr expr2))))
        ;; Activate the expression
        (adjoinf active-set expr))))

  (defun enqueue-expr (expr)
    (setf (gethash expr work-bags) (expr-to-bag expr))
    (requeue-expr expr))

  ;; Split cycle
  (defun process-one-split ()
    (multiple-value-bind (cut-bag exprs) (get-next-cut)
      (let* ((inv-bag       (invert cut-bag))
             (cut-expr      (bag-to-expr cut-bag))
             (inv-expr      (canonic inv-func cut-expr))
             (changed-exprs (empty-set))
             (scan-set      (less (intersection exprs active-set)
                                  cut-expr)))
        ;; Kill the opposite
        (dequeue-bag inv-bag)
        ;; Process pending expressions
        (do-set (expr scan-set)
          (let* ((cur-bag (gethash expr work-bags))
                 (changed nil))
            ;; Forward split
            (do ((cnt 0 (1+ cnt)))
                ((not (subbag? cut-bag cur-bag))
                 (setf cur-bag (with cur-bag cut-expr cnt)))
              (setf changed t)
              (setf cur-bag (bag-difference cur-bag cut-bag)))
            ;; Inverted split
            (do ((cnt 0 (1+ cnt)))
                ((not (subbag? inv-bag cur-bag))
                 (setf cur-bag (with cur-bag inv-expr cnt)))
              (setf changed t)
              (setf cur-bag (bag-difference cur-bag inv-bag)))
            ;; Save changes
            (when changed
              (adjoinf changed-exprs expr)
              (setf (gethash expr work-bags) cur-bag))))
        ;; Register the new node
        (unless (or (empty? changed-exprs)
                    (gethash cut-expr work-bags))
          (setf (gethash cut-expr work-bags) cut-bag)
          (adjoinf changed-exprs cut-expr))
        ;; Requeue changed expressions
        (setf active-set (set-difference active-set changed-exprs))
        (do-set (expr changed-exprs)
          (requeue-expr expr)))))

  (defun process-splits ()
    (until (empty? pull-queue)
      (process-one-split)))

  ;; Output mapping
  (defun make-replace-hash (&optional (hash (make-hash-table)))
    ;; Walk the work table in a stable order
    (do-set (expr (convert 'set (hash-table-keys work-bags)))
      (let ((new-expr (bag-to-expr (gethash expr work-bags))))
        (unless (eql expr new-expr)
          (setf (gethash expr hash) new-expr))))
    hash)

  (defun process-all (exprs repl-table)
    (mapc #'enqueue-expr exprs)
    (process-splits)
    (make-replace-hash repl-table)))


;; Wrapper

(defun split-by-cse (expr)
  (let ((expr-table (count-subexprs (canonic-expr-unwrap expr)))
        (add-list   nil)
        (mul-list   nil)
        (fix-table  (make-hash-table)))
    ;; Collect additions and multiplications
    (maphash (lambda (sub cnt)
               (match sub
                 (`(+ ,@_)
                   (push (make-canonic sub) add-list))
                 (`(* ,@_)
                   (push (make-canonic sub) mul-list))))
             expr-table)
    ;; Determine the splits
    (with-context (common-subset-extract '+ #'toggle-minus #'has-minus-p)
      (process-all add-list fix-table))
    (with-context (common-subset-extract '* #'toggle-div #'has-div-p)
      (process-all mul-list fix-table))
    ;; Substitute them
    (canonic-substitute fix-table expr :replace-once nil)))
