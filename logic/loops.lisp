;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package fast-compute)

(defun get-num-step (step)
    (match step
        (`(* ,x) x)
        (_ (or step 1))))

(defun get-ord-step (step)
    (match step
        (`(* ,_) nil)
        (_ step)))

(defun index-iterexpr (item iname &key (as iname) (var as) step (skip '(0 0))
                          (skip-low (car skip)) (skip-high (cadr skip)))
    (unless (symbolp var)
        (error "Expecting a symbol for dimension '~A', found: ~A" iname var))
    (destructuring-bind (iname minv maxv &key (by 1) (bands 1)) item
        (if (> bands 1)
            ;; Multiple bands
            (let* ((dimension (cadr (index-dimension item)))
                   (num-step  (get-num-step step))
                   (one-step  (if (> num-step 0) 1 -1))
                   (band-step (if (>= (abs num-step) bands) bands num-step))
                   (line-step (if (>= (abs num-step) bands) (/ num-step bands) one-step))
                   (band-min  (if (< skip-low (abs band-step)) skip-low 0))
                   (band-max  (if (< skip-high (abs band-step)) skip-high 0))
                   (line-min  (if (>= skip-low bands) (/ skip-low bands) 0))
                   (line-max  (if (>= skip-high bands) (/ skip-high bands) 0))
                   (var-range (ranging-spec var
                                   line-min
                                   (simplify-index `(- (- ,dimension ,line-max) 1))
                                   line-step (get-ord-step step) nil)))
                (unless (and (integerp band-step) (integerp line-step)
                             (= (mod bands band-step) 0))
                    (error "~A: step ~A does not match band count ~A"
                        iname num-step bands))
                (unless (and (integerp line-min)
                            (= (+ (* bands line-min) band-min) skip-low))
                    (error "~A: cannot skip-low ~A with ~A bands and step ~A"
                        iname skip-low bands num-step))
                (unless (and (integerp line-max)
                            (= (+ (* bands line-max) band-max) skip-high))
                    (error "~A: cannot skip-high ~A with ~A bands and step ~A"
                        iname skip-high bands num-step))
                (when (>= (+ band-min band-max) (abs band-step))
                    (error "~A: cannot skip (~A + ~A) with ~A bands and step ~A"
                        iname skip-low skip-high bands num-step))
                (if (= bands (abs band-step))
                    (let* ((band-value (if (> one-step 0) band-min
                                           (- (- bands band-max) 1)))
                           (expr `(+ (* (+ (* ,var-range ,bands) ,band-value) ,by) ,minv)))
                        (values (cons var expr) (list var-range)))
                    (let* ((band-var    (get-new-symbol :stem var))
                           (band-range (ranging-spec band-var
                                            band-min (- (- bands band-max) 1)
                                            band-step band-step
                                            nil))
                           (expr `(+ (* (+ (* ,var-range ,bands) ,band-range) ,by) ,minv)))
                        (setf (get band-var 'band-master) var)
                        (values (cons var expr) (list band-range var-range)))))
            ;; Single band
            (let* ((dimension (car (index-dimension item)))
                   (num-step  (get-num-step step))
                   (var-range (ranging-spec var
                                   skip-low
                                   (simplify-index `(- (- ,dimension ,skip-high) 1))
                                   num-step (get-ord-step step) nil))
                   (expr `(+ (* ,var-range ,by) ,minv)))
                (values (cons var expr) (list var-range))))))

(defun apply-index-iterexpr (name indexes item)
    (let* ((wrap  (if (atom item) (list item) item))
           (wrap2 (if (or (null (cdr wrap)) (keywordp (cadr wrap)))
                      wrap
                      (list* (car wrap) ':as (cdr wrap))))
           (idxobj (find (car wrap2) indexes :key #'car)))
        (unless idxobj
            (error "Unknown index '~A' for multivalue ~A: ~A" (car wrap2) name item))
        (apply #'index-iterexpr idxobj wrap2)))

(defun correct-loop-levels (range-list min-layer)
    (dolist (range (reverse range-list))
        (setf (range-loop-level (ranging-info range)) min-layer)
        (incf min-layer))
    (values range-list min-layer))

(defun build-loop-list (name indexes idxlist &key min-layer)
    (let ((replace-tbl nil)
          (loop-lst    nil))
        (dolist (item (reverse idxlist))
            (multiple-value-bind
                    (pair ranges)
                    (apply-index-iterexpr name indexes item)
                (push pair replace-tbl)
                (setf loop-lst (nconc ranges loop-lst))))
        (when min-layer
            (correct-loop-levels loop-lst min-layer))
        (values loop-lst replace-tbl)))

(defun do-wrap-loops (code ranges replace-tbl)
    (let ((loops nil)
          (cur-code (replace-unquoted code replace-tbl)))
        (dolist (item (reverse ranges))
            (let ((cloop `(loop-range ,item ,@cur-code)))
                (push cloop loops)
                (setf cur-code (list cloop))))
        (values
            (wrap-progn cur-code)
            loops ranges replace-tbl)))

(defun wrap-idxloops (name indexes idxlist code &key min-layer)
    (multiple-value-call
        #'do-wrap-loops
        code
        (build-loop-list
            name indexes idxlist
            :min-layer min-layer)))

(defmacro loop-indexes (name idxlist &body code)
    (let ((indexes      (get name 'mv-indexes)))
        (unless indexes
            (error "Unknown multivalue ~A" name))
        (wrap-idxloops name indexes idxlist code)))
