;; /home/martin/src/gpu-playground/render-nodes-minimal/main.c
;; based on code by Eduardo Lima Mitev <elima@igalia.com>


(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro er (&body body)
  `(with-compilation-unit (<< "std::cerr" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro checked-ioctl (fd cmd arg)
  `(if (< (funcall ioctl ,fd ,cmd ,arg) 0)
     (macroexpand (er ,(format nil "ioctl ~a failed." cmd)))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw " ")
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
         `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))
    (raw " ")))


(defparameter *drm-facts*
  `((10 "")))


(defmacro with-open-fstream ((f fn &key (dir "/dev/shm")) &body body)
  `(let ((,f :type "std::ofstream" :ctor (comma-list (string ,(format nil "~a/~a" dir fn)))))
     ,@body))



(progn
  (let* ((shader-code (let ((code `(with-compilation-unit
				     (raw "#version 310 es\\n")
				     
				     (raw "layout (local_size_x = 1, local_size_y = 1, local_size_z = 1) in;")
				     
				     #+nil (raw ,(format nil "void main(){;}")
					  )
				     
				     (function (main () void)
						     (raw "/* code here */")
						     (raw "")))))
			(replace-all (emit-cpp
				      :clear-env t
				      :code code)
				     "
"
				     "\\
")))
	 (code `(with-compilation-unit
		    (with-compilation-unit
			(raw "//! \\file main.cpp Draw to screen using linux direct rendering manager"))

		  (include <EGL/egl.h>)
		  (include <EGL/eglext.h>)
		  (include <GLES3/gl31.h>)
		  (include <gbm.h>)
		  (include <fcntl.h>)
		  (include <stdbool.h>)
		  
		  (include <iostream>)
		  (include <cassert>)
		 
					;(include <errno.h>)
		  (include <cstring>)
		  ;(include <sys/mman.h>)
		  (include <unistd.h>)

		  
		  (raw "//! This repository contains a minimal program to run compute shaders in linux.")
		  (raw "//! \\section Dependencies ")
		  (raw "//! - Linux kernel with DRM driver")
		  (raw "//! - EGL")
		  (raw " ")
		  (raw "//! - sbcl to generate c++ code")
					;(raw "//! - cmake to configure for build")
		  (raw "//! - g++ to compile c++ code")
		  (raw " ")
		  (raw "//! - For the documentation (optional):")
		  (raw "//!   + doxygen")
		  (raw " ")
		  
		  (raw " ")
		  (raw "//! \\section References ")
		  ,@(loop for i from 1 and e in '("gpu-playground/render-nodes-minimal/main.c"
						  "https://www.khronos.org/opengl/wiki/GLSL_Optimizations")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))
		  
		  (raw ,(format nil "#define COMPUTE_SHADER_SRC \"~a\"" shader-code))
		  
		  
		  ,@(dox :brief "main function"
			 :usage "draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)
			    (let ((fd ;:type int32_t
				      :init (paren-list
					     (let ((fd_ :init (funcall open (string "/dev/dri/renderD128") O_RDWR)))
					       (funcall assert (< 0 fd_))
					       (raw "fd_"))))
				  (gbm :init (paren-list
					      (let ((g_ :init (funcall gbm_create_device fd)))
						(funcall assert (!= nullptr g_))
						(raw g_))))
				  (egl_dpy :init (paren-list
						  (let ((d_ :init (funcall eglGetPlatformDisplay
									   EGL_PLATFORM_GBM_MESA
									   gbm
									   nullptr)))
						    (funcall assert (!= nullptr d_))
						    (raw "d_"))))
				  (egl_runs_p :type bool
					      :init (paren-list
						     (let ((res :init (funcall eglInitialize egl_dpy
									       nullptr nullptr)))
						       (funcall assert res)
						       (raw "1"))))
				  (surface_less_supported_p
				   :type bool
				   :init (paren-list
					  (let ((egl_extension :init (funcall eglQueryString egl_dpy
									      EGL_EXTENSIONS)))
					    (funcall assert (!= nullptr
								(funcall strstr
									 egl_extension
									 (string "EGL_KHR_create_context"))))
					    (funcall assert (!= nullptr
								(funcall strstr
									 egl_extension
									 (string "EGL_KHR_surfaceless_context"))))
					    (raw "1"))))
				  (cfg :init
				    (paren-list
				     (let (((aref config_attribs) :type "static const EGLint"
					    :init (list EGL_RENDERABLE_TYPE
							EGL_OPENGL_ES3_BIT_KHR
							EGL_NONE))
					   (cfg_ :type EGLConfig)
					   (count :type EGLint)
					   (res :init (funcall eglChooseConfig  egl_dpy
							       config_attribs
							       &cfg_
							       1
							       &count)))
				       (funcall assert res)
				       (raw cfg_))))

				  (egl_api_bound :init
				    (paren-list
				     (let ((res :init (funcall eglBindAPI EGL_OPENGL_ES_API)))
				       (funcall assert res)
				       (raw res))))
				  (core_ctx :init
				    (paren-list
				     (let (((aref attribs) :type "static const EGLint"
					    :init (list EGL_CONTEXT_CLIENT_VERSION 3
								       EGL_NONE))
					   (ctx :init (funcall eglCreateContext egl_dpy
							       cfg EGL_NO_CONTEXT
							       attribs)))
				       (funcall assert (!= EGL_NO_CONTEXT ctx))
				       (raw "ctx"))))
				  (ctx_current :init
				    (paren-list
				     (let ((res :init (funcall eglMakeCurrent egl_dpy EGL_NO_SURFACE
							 EGL_NO_SURFACE core_ctx)))
				       (funcall assert res)
				       (raw res))))
				  (compute_shader :init
				    (paren-list
				     (let ((res :init (funcall glCreateShader GL_COMPUTE_SHADER)))
				       (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				       (raw res))))
				  (shader_program :init
				    (paren-list
				     (let ((shader_source :type "const char*" :init COMPUTE_SHADER_SRC)
					   )
				       (funcall glShaderSource compute_shader 1 &shader_source nullptr)
				       (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				       (funcall glCompileShader compute_shader)
				       (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				       (funcall glCreateProgram))))
				  (attached_linked :init
				    (paren-list
				     (funcall glAttachShader shader_program compute_shader)
				     (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				     (funcall glLinkProgram shader_program)
				     (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				     1))

				  
				  (used :init
				    (paren-list
				     (funcall glDeleteShader compute_shader)
				     (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				     (funcall glUseProgram shader_program)
				     (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				     1))
				  (dispatched :init
				    (paren-list
				     (funcall glDispatchCompute 1 1 1)
				     (funcall assert (== GL_NO_ERROR (funcall glGetError)))
				     1))
				  (cleaned_up :init
				    (paren-list
				     (funcall glDeleteProgram shader_program)
				     (funcall eglDestroyContext egl_dpy core_ctx)
				     (funcall eglTerminate egl_dpy)
				     (funcall gbm_device_destroy gbm)
				     (funcall close fd)))))
			    (return 0)))))
    (write-source "stage/cl-gen-egl-compute/source/main" "cpp" code)))


