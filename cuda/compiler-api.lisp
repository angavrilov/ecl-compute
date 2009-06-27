;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(in-package cuda)

(defun translate-args (func-var args)
    (let ((arg-strings nil)
          (arg-forms nil)
          (offset 0))
        (dolist (arg args)
            (destructuring-bind
                (atype aname aform) arg
                (labels
                    ((add-arg (size type-str set-form &key (align size))
                         (setf offset
                             (logand (+ offset align -1) (lognot (- align 1))))
                         (push
                             `(,set-form ,func-var ,offset ,aform)
                             arg-forms)
                         (push
                             (format nil "~A ~A" type-str aname)
                             arg-strings)
                         (incf offset size)))
                    (case atype
                        ((:char :byte)
                            (add-arg 1 "char" 'param-set-int))
                        ((:unsigned-char :unsigned-byte)
                            (add-arg 1 "unsigned char" 'param-set-uint))
                        (:int
                            (add-arg 4 "int" 'param-set-int))
                        (:unsigned-int
                            (add-arg 4 "unsigned int" 'param-set-uint))
                        (:float
                            (add-arg 4 "float" 'param-set-float))
                        (:double
                            (add-arg 8 "double" 'param-set-double))
                        (:float-ptr
                            (add-arg +ptr-size+ "float*" 'param-set-ptr))
                        (:double-ptr
                            (add-arg +ptr-size+ "double*" 'param-set-ptr))
                        (otherwise
                            (error "Unsupported kernel parameter type: ~A" atype))))))
        (values
            (nreverse arg-strings)
            (nreverse arg-forms)
            offset)))

(defparameter *compiled-cache* (make-hash-table :test #'equal))

(defvar *nvcc* "nvcc")
(defvar *nvcc-flags* "")

(defun do-compile-kernel (code)
    (let* ((tmpname (ext:mkstemp #P"TMP:CUDAKERNEL"))
           (srcname (make-pathname :type "cu" :defaults tmpname))
           (outname (make-pathname :type "ptx" :defaults tmpname))
           (cmd (format nil
                    "~A ~A -m~A --ptx --output-file=~A ~A"
                    *nvcc* *nvcc-flags*
                    (* +ptr-size+ 8)
                    outname srcname)))
        (with-open-file (src srcname :direction :output
                            :if-exists :supersede)
            (write-string code src))
        (unwind-protect
            (progn
                (format t "Running command:~%  ~A~%" cmd)
                (let ((rv (ext:system cmd)))
                    (unless (= rv 0)
                        (error "Compilation failed: ~A~%" rv)))
                (with-open-file (out outname)
                    (let ((buffer (make-string (file-length out)
                                      :element-type 'base-char)))
                        (read-sequence buffer out)
                        buffer)))
            (when (probe-file srcname)
                (delete-file srcname))
            (when (probe-file outname)
                (delete-file outname)))))

(defun compile-kernel (code)
    (let ((cached-code (gethash code *compiled-cache*)))
        (if cached-code cached-code
            (setf (gethash code *compiled-cache*)
                (do-compile-kernel code)))))

(defmacro kernel (args code &key (grid-size '(1 1)) (block-size '(1 1 1)))
    (assert (= (length block-size) 3)
        (block-size) "Bad block size spec: ~A" block-size)
    (assert (= (length grid-size) 2)
        (grid-size) "Bad grid size spec: ~A" grid-size)
    (let* ((func-var (gensym)))
        (multiple-value-bind
            (arg-strings arg-forms arg-size)
            (translate-args func-var args)
            (let* ((full-code
                       (format nil
                           "extern \"C\" __global__ __device__
                            void kernel_func(~{~A~^, ~}) {~%~A~%}~%"
                           arg-strings code))
                   (compiled-code (compile-kernel full-code)))
                `(let ((,func-var (load-kernel '("kernel_func" . ,compiled-code))))
                     ,@arg-forms
                     (launch-kernel ,func-var ,arg-size ,@block-size ,@grid-size))))))
