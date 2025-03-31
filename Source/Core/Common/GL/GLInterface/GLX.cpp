// Copyright 2012 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/GL/GLInterface/GLX.h"

#include <array>
#include <sstream>

#include "Common/Logging/Log.h"

#define GLX_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define GLX_CONTEXT_MINOR_VERSION_ARB 0x2092

typedef GLXContext (*PFNGLXCREATECONTEXTATTRIBSPROC)(Display*, GLXFBConfig, GLXContext, Bool,
                                                     const int*);

#ifndef GLX_EXT_swap_control
typedef void (*PFNGLXSWAPINTERVALEXTPROC)(Display*, GLXDrawable, int);
#endif

typedef int (*PFNGLXSWAPINTERVALMESAPROC)(unsigned int);

static PFNGLXCREATECONTEXTATTRIBSPROC gl_x_create_context_attribs = nullptr;
static PFNGLXSWAPINTERVALEXTPROC gl_x_swap_interval_ext_ptr = nullptr;
static PFNGLXSWAPINTERVALMESAPROC gl_x_swap_interval_mesa_ptr = nullptr;

static PFNGLXCREATEGLXPBUFFERSGIXPROC gl_x_create_glx_pbuffer_sgix = nullptr;
static PFNGLXDESTROYGLXPBUFFERSGIXPROC gl_x_destroy_glx_pbuffer_sgix = nullptr;

static bool s_glx_error;
static int CtxErrorHandler(Display* dpy, XErrorEvent* ev)
{
  s_glx_error = true;
  return 0;
}

GLContextGLX::~GLContextGLX()
{
  DestroyWindowSurface();
  if (m_context)
  {
    if (glXGetCurrentContext() == m_context)
      glXMakeCurrent(m_display, None, nullptr);

    glXDestroyContext(m_display, m_context);
  }
}

bool GLContextGLX::IsHeadless() const
{
  return !m_render_window;
}

void GLContextGLX::SwapInterval(int Interval)
{
  if (!m_drawable)
    return;

  // Try EXT_swap_control, then MESA_swap_control.
  if (gl_x_swap_interval_ext_ptr)
  {
    gl_x_swap_interval_ext_ptr(m_display, m_drawable, Interval);
  }
  else if (gl_x_swap_interval_mesa_ptr)
  {
    gl_x_swap_interval_mesa_ptr(static_cast<unsigned int>(Interval));
  }
  else
  {
    ERROR_LOG_FMT(VIDEO,
                  "No support for SwapInterval (framerate clamped to monitor refresh rate).");
  }
}

void* GLContextGLX::GetFuncAddress(const std::string& name)
{
  return reinterpret_cast<void*>(glXGetProcAddress(reinterpret_cast<const GLubyte*>(name.c_str())));
}

void GLContextGLX::Swap()
{
  glXSwapBuffers(m_display, m_drawable);
}

// Create rendering window.
// Call browser: Core.cpp:EmuThread() > main.cpp:Video_Initialize()
bool GLContextGLX::Initialize(const WindowSystemInfo& wsi, bool stereo, bool core)
{
  m_display = static_cast<Display*>(wsi.display_connection);
  int screen = DefaultScreen(m_display);

  // checking glx version
  int glx_major_version, glx_minor_version;
  glXQueryVersion(m_display, &glx_major_version, &glx_minor_version);
  if (glx_major_version < 1 || (glx_major_version == 1 && glx_minor_version < 4))
  {
    ERROR_LOG_FMT(VIDEO, "glX-Version {}.{} detected, but need at least 1.4", glx_major_version,
                  glx_minor_version);
    return false;
  }

  // loading core context creation function
  gl_x_create_context_attribs =
      (PFNGLXCREATECONTEXTATTRIBSPROC)GetFuncAddress("glXCreateContextAttribsARB");
  if (!gl_x_create_context_attribs)
  {
    ERROR_LOG_FMT(VIDEO,
                  "glXCreateContextAttribsARB not found, do you support GLX_ARB_create_context?");
    return false;
  }

  // choosing framebuffer
  int visual_attribs[] = {GLX_X_RENDERABLE,
                          True,
                          GLX_DRAWABLE_TYPE,
                          GLX_WINDOW_BIT,
                          GLX_X_VISUAL_TYPE,
                          GLX_TRUE_COLOR,
                          GLX_RED_SIZE,
                          8,
                          GLX_GREEN_SIZE,
                          8,
                          GLX_BLUE_SIZE,
                          8,
                          GLX_DEPTH_SIZE,
                          0,
                          GLX_STENCIL_SIZE,
                          0,
                          GLX_DOUBLEBUFFER,
                          True,
                          GLX_STEREO,
                          stereo ? True : False,
                          None};
  int fbcount = 0;
  GLXFBConfig* fbc = glXChooseFBConfig(m_display, screen, visual_attribs, &fbcount);
  if (!fbc || !fbcount)
  {
    ERROR_LOG_FMT(VIDEO, "Failed to retrieve a framebuffer config");
    return false;
  }
  m_fbconfig = *fbc;
  XFree(fbc);

  s_glx_error = false;
  XErrorHandler old_handler = XSetErrorHandler(&CtxErrorHandler);

  // Create a GLX context.
  if (core)
  {
    for (const auto& version : s_desktop_opengl_versions)
    {
      std::array<int, 9> context_attribs = {
          {GLX_CONTEXT_MAJOR_VERSION_ARB, version.first, GLX_CONTEXT_MINOR_VERSION_ARB,
           version.second, GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
           GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB, None}};

      s_glx_error = false;
      m_context =
          gl_x_create_context_attribs(m_display, m_fbconfig, nullptr, True, &context_attribs[0]);
      XSync(m_display, False);
      if (!m_context || s_glx_error)
        continue;

      // Got a context.
      INFO_LOG_FMT(VIDEO, "Created a GLX context with version {}.{}", version.first,
                   version.second);
      m_attribs.insert(m_attribs.end(), context_attribs.begin(), context_attribs.end());
      break;
    }
  }

  // Failed to create any core contexts, try for anything.
  if (!m_context || s_glx_error)
  {
    std::array<int, 5> context_attribs_legacy = {
        {GLX_CONTEXT_MAJOR_VERSION_ARB, 1, GLX_CONTEXT_MINOR_VERSION_ARB, 0, None}};
    s_glx_error = false;
    m_context =
        gl_x_create_context_attribs(m_display, m_fbconfig, nullptr, True, &context_attribs_legacy[0]);
    XSync(m_display, False);
    m_attribs.clear();
    m_attribs.insert(m_attribs.end(), context_attribs_legacy.begin(), context_attribs_legacy.end());
  }
  if (!m_context || s_glx_error)
  {
    ERROR_LOG_FMT(VIDEO, "Unable to create GL context.");
    XSetErrorHandler(old_handler);
    return false;
  }

  gl_x_swap_interval_ext_ptr = nullptr;
  gl_x_swap_interval_mesa_ptr = nullptr;
  gl_x_create_glx_pbuffer_sgix = nullptr;
  gl_x_destroy_glx_pbuffer_sgix = nullptr;
  m_supports_pbuffer = false;

  std::string tmp;
  std::istringstream buffer(glXQueryExtensionsString(m_display, screen));
  while (buffer >> tmp)
  {
    if (tmp == "GLX_SGIX_pbuffer")
    {
      gl_x_create_glx_pbuffer_sgix = reinterpret_cast<PFNGLXCREATEGLXPBUFFERSGIXPROC>(
          GetFuncAddress("glXCreateGLXPbufferSGIX"));
      gl_x_destroy_glx_pbuffer_sgix = reinterpret_cast<PFNGLXDESTROYGLXPBUFFERSGIXPROC>(
          GetFuncAddress("glXDestroyGLXPbufferSGIX"));
      m_supports_pbuffer = gl_x_create_glx_pbuffer_sgix && gl_x_destroy_glx_pbuffer_sgix;
    }
    else if (tmp == "GLX_EXT_swap_control")
    {
      gl_x_swap_interval_ext_ptr =
          reinterpret_cast<PFNGLXSWAPINTERVALEXTPROC>(GetFuncAddress("glXSwapIntervalEXT"));
    }
    else if (tmp == "GLX_MESA_swap_control")
    {
      gl_x_swap_interval_mesa_ptr =
          reinterpret_cast<PFNGLXSWAPINTERVALMESAPROC>(GetFuncAddress("glXSwapIntervalMESA"));
    }
  }

  if (!CreateWindowSurface(reinterpret_cast<Window>(wsi.render_surface)))
  {
    ERROR_LOG_FMT(VIDEO, "Error: CreateWindowSurface failed\n");
    XSetErrorHandler(old_handler);
    return false;
  }

  XSetErrorHandler(old_handler);
  m_opengl_mode = Mode::OpenGL;
  return MakeCurrent();
}

std::unique_ptr<GLContext> GLContextGLX::CreateSharedContext()
{
  s_glx_error = false;
  XErrorHandler old_handler = XSetErrorHandler(&CtxErrorHandler);

  GLXContext new_glx_context =
      gl_x_create_context_attribs(m_display, m_fbconfig, m_context, True, &m_attribs[0]);
  XSync(m_display, False);

  if (!new_glx_context || s_glx_error)
  {
    ERROR_LOG_FMT(VIDEO, "Unable to create GL context.");
    XSetErrorHandler(old_handler);
    return nullptr;
  }

  std::unique_ptr<GLContextGLX> new_context = std::make_unique<GLContextGLX>();
  new_context->m_context = new_glx_context;
  new_context->m_opengl_mode = m_opengl_mode;
  new_context->m_supports_pbuffer = m_supports_pbuffer;
  new_context->m_display = m_display;
  new_context->m_fbconfig = m_fbconfig;
  new_context->m_is_shared = true;

  if (m_supports_pbuffer && !new_context->CreateWindowSurface(None))
  {
    ERROR_LOG_FMT(VIDEO, "Error: CreateWindowSurface failed");
    XSetErrorHandler(old_handler);
    return nullptr;
  }

  XSetErrorHandler(old_handler);
  return new_context;
}

bool GLContextGLX::CreateWindowSurface(Window window_handle)
{
  if (window_handle)
  {
    // Get an appropriate visual
    XVisualInfo* vi = glXGetVisualFromFBConfig(m_display, m_fbconfig);
    m_render_window = GLX11Window::Create(m_display, window_handle, vi);
    if (!m_render_window)
      return false;

    m_backbuffer_width = m_render_window->GetWidth();
    m_backbuffer_height = m_render_window->GetHeight();
    m_drawable = static_cast<GLXDrawable>(m_render_window->GetWindow());
    XFree(vi);
  }
  else if (m_supports_pbuffer)
  {
    m_pbuffer = gl_x_create_glx_pbuffer_sgix(m_display, m_fbconfig, 1, 1, nullptr);
    if (!m_pbuffer)
      return false;

    m_drawable = static_cast<GLXDrawable>(m_pbuffer);
  }

  return true;
}

void GLContextGLX::DestroyWindowSurface()
{
  m_render_window.reset();
  if (m_supports_pbuffer && m_pbuffer)
  {
    gl_x_destroy_glx_pbuffer_sgix(m_display, m_pbuffer);
    m_pbuffer = 0;
  }
}

bool GLContextGLX::MakeCurrent()
{
  return glXMakeCurrent(m_display, m_drawable, m_context);
}

bool GLContextGLX::ClearCurrent()
{
  return glXMakeCurrent(m_display, None, nullptr);
}

void GLContextGLX::Update()
{
  m_render_window->UpdateDimensions();
  m_backbuffer_width = m_render_window->GetWidth();
  m_backbuffer_height = m_render_window->GetHeight();
}
