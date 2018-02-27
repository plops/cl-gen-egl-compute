//! \file main.cpp Draw to screen using linux direct rendering manager
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GLES3/gl31.h>
#include <cassert>
#include <cstring>
#include <fcntl.h>
#include <gbm.h>
#include <iostream>
#include <stdbool.h>
#include <unistd.h>
//! This repository contains a minimal program to run compute shaders in linux.
//! \section Dependencies
//! - Linux kernel with DRM driver
//! - EGL

//! - sbcl to generate c++ code
//! - g++ to compile c++ code

//! - For the documentation (optional):
//!   + doxygen

//! \section References
//! 1. gpu-playground/render-nodes-minimal/main.c
#define COMPUTE_SHADER_SRC                                                     \
  "#version 310 es\
layout (local_size_x=1,local_size_y=1,local_size_z=1) in\
main(nil void){\
  // code here;\
  ;}"

//! @brief main function
//!
//! @usage draw to screen
//!
//! @param argc input number of command line arguments
//! @param argv input
//!
//! @return Integer

int main(int argc, char **argv) {
  {
    auto fd = ({
      auto fd_ = open("/dev/dri/renderD128", O_RDWR);
      assert((0 < fd_));
      fd_;
    });
    auto gbm = ({
      auto g_ = gbm_create_device(fd);
      assert((nullptr != g_));
      g_;
    });
    auto egl_dpy = ({
      auto d_ = eglGetPlatformDisplay(EGL_PLATFORM_GBM_MESA, gbm, nullptr);
      assert((nullptr != d_));
      d_;
    });
    bool egl_runs_p = ({
      auto res = eglInitialize(egl_dpy, nullptr, nullptr);
      assert(res);
      1;
    });
    bool surface_less_supported_p = ({
      auto egl_extension = eglQueryString(egl_dpy, EGL_EXTENSIONS);
      assert((nullptr != strstr(egl_extension, "EGL_KHR_create_context")));
      assert((nullptr != strstr(egl_extension, "EGL_KHR_surfaceless_context")));
      1;
    });
    auto cfg = ({
      static const EGLint config_attribs[] = {EGL_RENDERABLE_TYPE,
                                              EGL_OPENGL_ES3_BIT_KHR, EGL_NONE};
      EGLConfig cfg_;
      EGLint count;
      auto res = eglChooseConfig(egl_dpy, config_attribs, &cfg_, 1, &count);
      assert(res);
      cfg_;
    });
    auto egl_api_bound = ({
      auto res = eglBindAPI(EGL_OPENGL_ES_API);
      assert(res);
      res;
    });
    auto core_ctx = ({
      static const EGLint attribs[] = {EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE};
      auto ctx = eglCreateContext(egl_dpy, cfg, EGL_NO_CONTEXT, attribs);
      assert((EGL_NO_CONTEXT != ctx));
      ctx;
    });
    auto ctx_current = ({
      auto res =
          eglMakeCurrent(egl_dpy, EGL_NO_SURFACE, EGL_NO_SURFACE, core_ctx);
      assert(res);
      res;
    });
    auto compute_shader = ({
      auto res = glCreateShader(GL_COMPUTE_SHADER);
      assert((GL_NO_ERROR == glGetError()));
      res;
    });
    auto shader_program = ({
      const char *shader_source = COMPUTE_SHADER_SRC;
      glShaderSource(compute_shader, 1, &shader_source, nullptr);
      assert((GL_NO_ERROR == glGetError()));
      glCompileShader(compute_shader);
      assert((GL_NO_ERROR == glGetError()));
      glCreateProgram();
    });
    auto attached_linked =
        (glAttachShader(shader_program, compute_shader),
         assert((GL_NO_ERROR == glGetError())), glLinkProgram(shader_program),
         assert((GL_NO_ERROR == glGetError())), 1);
    auto used =
        (glDeleteShader(compute_shader), assert((GL_NO_ERROR == glGetError())),
         glUseProgram(shader_program),
         {
           auto e = glGetError();
           if ((GL_NO_ERROR != e)) {
             (std::cerr << "error: " << e << std::endl);
           }
         },
         1);
    auto dispatched =
        (glDispatchCompute(1, 1, 1), assert((GL_NO_ERROR == glGetError())), 1);
    auto cleaned_up =
        (glDeleteProgram(shader_program), eglDestroyContext(egl_dpy, core_ctx),
         eglTerminate(egl_dpy), gbm_device_destroy(gbm), close(fd));
  }
  return 0;
}