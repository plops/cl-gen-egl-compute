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
  `(statements (<< "std::cerr" ,@(loop for e in body collect
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
  (let* ((code `(with-compilation-unit
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
					;(include <cstring>)
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
		  ,@(loop for i from 1 and e in '("gpu-playground/render-nodes-minimal/main.c")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))
		  
		  
		  ,@(dox :brief "main function"
			 :usage "draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)
			    #+Nil (has_dumb :init (paren-list
						   (let ((has_dumb :type uint64_t :init 0))
						     (funcall bla)
						     (raw "has_dumb"))))
			    (let ((fd :type int32_t
				      :init (paren-list
					     (let ((fd_ :init (funcall open (string "/dev/dri/renderD128") O_RDWR)))
					       (funcall assert (< 0 fd_))
					       (raw "fd_"))))
				  (gbm :init (paren-list
					      (let ((g_ :init (funcall gbm_create_device fd)))
						(funcall assert (!= nullptr g_))
						(raw g_)))))
			      )
			    (return 0)))))
    (write-source "stage/cl-gen-egl-compute/source/main" "cpp" code)))


