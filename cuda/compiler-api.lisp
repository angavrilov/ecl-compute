;;; -*- mode:lisp; indent-tabs-mode: nil; -*-

(in-package cuda)

(defun translate-args (func-var args)
  (let ((arg-strings nil)
        (arg-forms nil)
        (offset 0))
    (dolist (arg args)
      (destructuring-bind (atype aname aform) arg
        (labels
            ((add-arg (size type-str set-form &key (align size))
               (setf offset
                     (logand (+ offset align -1) (lognot (- align 1))))
               (push `(,set-form ,func-var ,offset ,aform)
                     arg-forms)
               (push (format nil "~A ~A" type-str aname)
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
    (values (nreverse arg-strings)
            (nreverse arg-forms)
            offset)))

(defun get-texture-name (spec)
  (coerce (third spec) 'base-string))

(defun get-texture-decl (spec)
  (destructuring-bind
        (vtype dim name arg) spec
    (assert (eql vtype :float))
    (assert (or (eql dim 1) (eql dim 2)))
    (format nil "texture<float,~A> ~A" dim name)))

(defun get-texture-assn (fun var idx spec)
  (destructuring-bind
        (vtype dim name arg) spec
    (ecase dim
      (1 `(param-set-texture-1d ,fun (svref ,var ,idx) ,arg))
      (2 `(param-set-texture-2d ,fun (svref ,var ,idx) ,arg)))))

(defparameter *compiled-cache* (make-hash-table :test #'equal))

(defvar *nvcc* "nvcc")
(defvar *nvcc-flags* "")
(defvar *nvcc-cubin* nil)

(defvar *print-kernel-code* nil)

(defun do-compile-kernel (code)
  (let* ((tmpname (ext:mkstemp #P"TMP:CUDAKERNEL"))
         (srcname (make-pathname :type "cu" :defaults tmpname))
         (outname (make-pathname :type (if *nvcc-cubin* "cubin" "ptx")
                                 :defaults tmpname))
         (cmd (format nil "~A ~A ~A -m~A --output-file=~A ~A"
                      *nvcc*
                      (if *nvcc-cubin* "--cubin" "--ptx")
                      *nvcc-flags*
                      (* +ptr-size+ 8)
                      outname srcname)))
    (with-open-file (src srcname :direction :output
                         :if-exists :supersede)
      (write-string code src))
    (when *print-kernel-code*
      (format t "Compiling:~%~A" code))
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
               (when *print-kernel-code*
                 (format t "Result:~%~A" buffer))
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

(defmacro kernel (args code &key
                  (grid-size '(1 1)) (block-size '(1 1 1))
                  (name "kernel_func") (max-registers nil)
                  (textures nil))
  (assert (= (length block-size) 3)
          (block-size) "Bad block size spec: ~A" block-size)
  (assert (= (length grid-size) 2)
          (grid-size) "Bad grid size spec: ~A" grid-size)
  (let* ((func-var (gensym))
         (grp-var (gensym)))
    (multiple-value-bind (arg-strings arg-forms arg-size)
        (translate-args func-var args)
      (let* ((full-code
              (format nil
                      "~{~A;~%~}extern \"C\" __global__ __device__
void ~A(~{~A~^, ~}) {~%~A~%}~%"
                      (mapcar #'get-texture-decl textures)
                      name arg-strings code))
             (compiled-code (compile-kernel full-code))
             (texs (if textures
                       (list :textures
                             (mapcar #'get-texture-name textures))))
             (args (if max-registers
                       (list* :max-registers max-registers texs)
                       texs))
             (load-spec `(load-kernel '(,(coerce name 'base-string)
                                        ,compiled-code ,@args)))
             (letspec (if textures
                          `((,grp-var (the vector ,load-spec))
                            (,func-var (svref ,grp-var 0)))
                          `((,func-var ,load-spec)))))
        `(let* ,letspec
           (declare (optimize (safety 1) (debug 0)))
           ,@arg-forms
           ,@(loop for tex in textures
                for idx from 1
                collect (get-texture-assn func-var grp-var idx tex))
           (launch-kernel ,func-var ,arg-size ,@block-size ,@grid-size))))))
