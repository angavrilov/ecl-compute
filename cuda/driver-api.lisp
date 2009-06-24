;;;; kate: indent-width 4; replace-tabs yes; space-indent on;

(defpackage cuda
    (:documentation "Interface to the NVidia CUDA driver")
    (:use "COMMON-LISP")
    (:export
        "*DEVICE-COUNT*" "GET-CAPS"
        "*CURRENT-CONTEXT*"
        "CREATE-CONTEXT" "DESTROY-CONTEXT"
        "CREATE-LINEAR-BUFFER" "DESTROY-LINEAR-BUFFER"
        "LINEAR-SIZE" "LINEAR-PITCH" "LINEAR-PITCHED-P"
    ))

(in-package cuda)

(ffi:clines "

#include <stdio.h>
#include <cuda.h>

static void check_error(CUresult err) {
    switch (err) {
    case CUDA_SUCCESS: return;
    case CUDA_ERROR_INVALID_VALUE:
        FEerror(\"Invalid value.\",0);
        break;
    case CUDA_ERROR_NOT_INITIALIZED:
        FEerror(\"Driver not initialized.\",0);
        break;
    case CUDA_ERROR_DEINITIALIZED:
        FEerror(\"Driver deinitialized.\",0);
        break;
    case CUDA_ERROR_NO_DEVICE:
        FEerror(\"No CUDA-capable device available.\",0);
        break;
    case CUDA_ERROR_INVALID_DEVICE:
        FEerror(\"Invalid device.\",0);
        break;
    case CUDA_ERROR_INVALID_IMAGE:
        FEerror(\"Invalid kernel image.\",0);
        break;
    case CUDA_ERROR_INVALID_CONTEXT:
        FEerror(\"Invalid context.\",0);
        break;
    case CUDA_ERROR_CONTEXT_ALREADY_CURRENT:
        FEerror(\"Context already current.\",0);
        break;
    case CUDA_ERROR_MAP_FAILED:
        FEerror(\"Map failed\",0);
        break;
    case CUDA_ERROR_UNMAP_FAILED:
        FEerror(\"Unmap failed.\",0);
        break;
    case CUDA_ERROR_ARRAY_IS_MAPPED:
        FEerror(\"Array is mapped.\",0);
        break;
    case CUDA_ERROR_ALREADY_MAPPED:
        FEerror(\"Already mapped.\",0);
        break;
    case CUDA_ERROR_NO_BINARY_FOR_GPU:
        FEerror(\"No binary for GPU.\",0);
        break;
    case CUDA_ERROR_ALREADY_ACQUIRED:
        FEerror(\"Already acquired.\",0);
        break;
    case CUDA_ERROR_NOT_MAPPED:
        FEerror(\"Not mapped.\",0);
        break;
    case CUDA_ERROR_INVALID_SOURCE:
        FEerror(\"Invalid source.\",0);
        break;
    case CUDA_ERROR_FILE_NOT_FOUND:
        FEerror(\"File not found.\",0);
        break;
    case CUDA_ERROR_INVALID_HANDLE:
        FEerror(\"Invalid handle.\",0);
        break;
    case CUDA_ERROR_NOT_FOUND:
        FEerror(\"Not found.\",0);
        break;
    case CUDA_ERROR_NOT_READY:
        FEerror(\"CUDA not ready.\",0);
        break;
    case CUDA_ERROR_LAUNCH_FAILED:
        FEerror(\"Launch failed.\",0);
        break;
    case CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES:
        FEerror(\"Launch exceeded resources.\",0);
        break;
    case CUDA_ERROR_LAUNCH_TIMEOUT:
        FEerror(\"Launch exceeded timeout.\",0);
        break;
    case CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING:
        FEerror(\"Launch with incompatible texturing.\",0);
        break;
    default:
        FEerror(\"Unknown CUDA error.\",0);
    }
}")

(defvar *device-count* nil)

(unless *device-count*
    (setf *device-count*
        (ffi:c-inline () () :int "
            int major, minor, count;

            check_error(cuInit(0));
            check_error(cuDeviceGetCount(&count));
            if (count > 0) {
                check_error(cuDeviceComputeCapability(&major, &minor, 0));
                if (major == 9999 && minor == 9999)
                    count = 0;
            }
            @(return) = count;")))

(defstruct capabilities
    revision name memory mp-count const-memory shared-memory reg-count warp-size
    max-threads tex-alignment has-overlap has-mapping has-timeout)

(defun get-caps (device)
    (multiple-value-bind
        (revision name memory mp-count const-memory shared-memory reg-count warp-size
         max-threads tex-alignment has-overlap has-timeout has-mapping)
        (ffi:c-inline (device) (:int)
            (values :object :object :int :int :int :int :int :int :int :int :int :int :int)
         "int major, minor, tmp;
          CUdevprop props;
          int dev = #0;
          char name[256];

          check_error(cuDeviceComputeCapability(&major, &minor, dev));
          @(return 0) = ecl_cons(ecl_make_integer(major), ecl_make_integer(minor));

          check_error(cuDeviceGetName(name, 256, dev));
          @(return 1) = make_base_string_copy(name);

          check_error(cuDeviceTotalMem(&tmp, dev));
          @(return 2) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, dev));
          @(return 3) = tmp;

          check_error(cuDeviceGetProperties(&props, dev));
          @(return 4) = props.totalConstantMemory;
          @(return 5) = props.sharedMemPerBlock;
          @(return 6) = props.regsPerBlock;
          @(return 7) = props.SIMDWidth;
          @(return 8) = props.maxThreadsPerBlock;
          @(return 9) = props.textureAlign;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, dev));
          @(return 10) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT, dev));
          @(return 11) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY, dev));
          @(return 12) = tmp;")
        (make-capabilities
            :revision revision :name name :memory memory :mp-count mp-count
            :const-memory const-memory :shared-memory shared-memory :reg-count reg-count
            :warp-size warp-size :tex-alignment tex-alignment :has-overlap (/= 0 has-overlap)
            :has-mapping (/= 0 has-mapping) :has-timeout (/= 0 has-timeout) :max-threads max-threads)))

(ffi:def-foreign-type context-pointer :void)

(defmacro check-ffi-type (var typespec)
    `(progn
         (check-type ,var si:foreign-data)
         (assert (eql (si:foreign-data-tag ,var) ',typespec)
             (,var)
             "Type mismatch: ~A is not a wrapped foreign ~A" ,var ',typespec)))

(defun destroy-context (context)
    (check-ffi-type context context-pointer)
    (ffi:c-inline (context) (:object) :void "{
            CUcontext ctx = ecl_foreign_data_pointer_safe(#0);
            if (ctx)
                check_error(cuCtxDestroy(ctx));
            (#0)->foreign.data = NULL;
        }"))

(defvar *current-context* nil)

(defun create-context (device &key sync-mode with-mapping)
    (let* ((map-flag (if with-mapping 1 0))
           (sync-flag (case sync-mode
                         ((nil) 0) (:auto 0) (:spin 1) (:yield 2) (:block 3)
                         (t (error "Invalid sync mode: ~A" sync-mode))))
           (context
               (ffi:c-inline
                   (device map-flag sync-flag 'context-pointer)
                   (:int :int :int :object)
                   :object "{
                       CUcontext ctx;
                       int flags = 0, dev = #0;
                       if (#1)
                           flags |= CU_CTX_MAP_HOST;
                       switch (#2) {
                       case 0: flags |= CU_CTX_SCHED_AUTO; break;
                       case 1: flags |= CU_CTX_SCHED_SPIN; break;
                       case 2: flags |= CU_CTX_SCHED_YIELD; break;
                       case 3: flags |= CU_CTX_BLOCKING_SYNC; break;
                       }
                       check_error(cuCtxCreate(&ctx, flags, dev));
                       @(return) = ecl_make_foreign_data(#3, 0, ctx);
                   }")))
        (ext:set-finalizer context #'destroy-context)
        (setf *current-context* context)))

(ffi:def-struct linear-buffer
    (width :unsigned-int)
    (height :unsigned-int)
    (pitch :unsigned-int)
    (device-ptr :unsigned-int))

(ffi:clines "
    typedef struct {
        unsigned width;
        unsigned height;
        unsigned pitch;
        CUdeviceptr device_ptr;
    } LinearBuffer;
")

(defun destroy-linear-buffer (buffer)
    (check-ffi-type buffer linear-buffer)
    (ffi:c-inline (buffer) (:object) :void "{
            LinearBuffer *pbuf = ecl_foreign_data_pointer_safe(#0);
            if (pbuf->device_ptr)
                check_error(cuMemFree(pbuf->device_ptr));
            pbuf->device_ptr = NULL;
        }"))

(defun create-linear-buffer (width &optional (height 1) &key pitched)
    (let* ((buffer
               (ffi:c-inline
                   (width height (or pitched 0) 'linear-buffer)
                   (:int :int :int :object)
                   :object "{
                       cl_object buf = ecl_allocate_foreign_data(#3,sizeof(LinearBuffer));
                       LinearBuffer *pbuf = buf->foreign.data;
                       pbuf->width = #0;
                       pbuf->height = #1;
                       if (#2 > 0) {
                           check_error(cuMemAllocPitch(&pbuf->device_ptr, &pbuf->pitch,
                                                       pbuf->width, pbuf->height, #2));
                       } else {
                           pbuf->pitch = pbuf->width;
                           check_error(cuMemAlloc(&pbuf->device_ptr, pbuf->width*pbuf->height));
                       }
                       @(return) = buf;
                   }")))
        (ext:set-finalizer buffer #'destroy-linear-buffer)
        buffer))

(defun linear-size (buffer)
    (* (ffi:get-slot-value buffer 'linear-buffer 'pitch)
       (ffi:get-slot-value buffer 'linear-buffer 'height)))

(defun linear-pitch (buffer)
    (ffi:get-slot-value buffer 'linear-buffer 'pitch))

(defun linear-pitched-p (buffer)
    (/= (ffi:get-slot-value buffer 'linear-buffer 'pitch)
        (ffi:get-slot-value buffer 'linear-buffer 'width)))
