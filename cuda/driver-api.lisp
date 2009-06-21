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

(defvar *cuda-device-count* nil)

(unless *cuda-device-count*
    (setf *cuda-device-count*
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

(defstruct cuda-caps
    revision name memory mp-count const-memory shared-memory reg-count warp-size
    max-threads tex-alignment has-overlap has-mapping has-timeout)

(defun cuda-get-caps (device)
    (multiple-value-bind
        (revision name memory mp-count const-memory shared-memory reg-count warp-size
         max-threads tex-alignment has-overlap has-timeout has-mapping)
        (ffi:c-inline (device) (:int)
            (values :object :object :int :int :int :int :int :int :int :int :int :int :int)
         "int major, minor, tmp;
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

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY, dev));
          @(return 4) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK, dev));
          @(return 5) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK, dev));
          @(return 6) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_WARP_SIZE, dev));
          @(return 7) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, dev));
          @(return 8) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT, dev));
          @(return 9) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_GPU_OVERLAP, dev));
          @(return 10) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT, dev));
          @(return 11) = tmp;

          check_error(cuDeviceGetAttribute(&tmp, CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY, dev));
          @(return 12) = tmp;")
        (make-cuda-caps
            :revision revision :name name :memory memory :mp-count mp-count
            :const-memory const-memory :shared-memory shared-memory :reg-count reg-count
            :warp-size warp-size :tex-alignment tex-alignment :has-overlap (/= 0 has-overlap)
            :has-mapping (/= 0 has-mapping) :has-timeout (/= 0 has-timeout) :max-threads max-threads)))

