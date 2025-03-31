// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/GL/GLUtil.h"

#include <memory>

#include "Common/Assert.h"
#include "Common/GL/GLContext.h"
#include "Common/Logging/Log.h"

namespace GLUtil
{
GLuint CompileProgram(const std::string& vertexShader, const std::string& fragmentShader)
{
  // generate objects
  GLuint vertex_shader_id = glCreateShader(GL_VERTEX_SHADER);
  GLuint fragment_shader_id = glCreateShader(GL_FRAGMENT_SHADER);
  GLuint program_id = glCreateProgram();

  // compile vertex shader
  const char* shader = vertexShader.c_str();
  glShaderSource(vertex_shader_id, 1, &shader, nullptr);
  glCompileShader(vertex_shader_id);
#if defined(_DEBUG) || defined(DEBUGFAST)
  GLint result = GL_FALSE;
  char string_buffer[1024];
  GLsizei string_buffer_usage = 0;
  glGetShaderiv(vertex_shader_id, GL_COMPILE_STATUS, &result);
  glGetShaderInfoLog(vertex_shader_id, 1024, &string_buffer_usage, string_buffer);

  if (result && string_buffer_usage)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL vertex shader warnings:\n{}{}", string_buffer, vertexShader);
  }
  else if (!result)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL vertex shader error:\n{}{}", string_buffer, vertexShader);
  }
  else
  {
    INFO_LOG_FMT(VIDEO, "GLSL vertex shader compiled:\n{}", vertexShader);
  }

  bool shader_errors = !result;
#endif

  // compile fragment shader
  shader = fragmentShader.c_str();
  glShaderSource(fragment_shader_id, 1, &shader, nullptr);
  glCompileShader(fragment_shader_id);
#if defined(_DEBUG) || defined(DEBUGFAST)
  glGetShaderiv(fragment_shader_id, GL_COMPILE_STATUS, &result);
  glGetShaderInfoLog(fragment_shader_id, 1024, &string_buffer_usage, string_buffer);

  if (result && string_buffer_usage)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL fragment shader warnings:\n{}{}", string_buffer, fragmentShader);
  }
  else if (!result)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL fragment shader error:\n{}{}", string_buffer, fragmentShader);
  }
  else
  {
    INFO_LOG_FMT(VIDEO, "GLSL fragment shader compiled:\n{}", fragmentShader);
  }

  shader_errors |= !result;
#endif

  // link them
  glAttachShader(program_id, vertex_shader_id);
  glAttachShader(program_id, fragment_shader_id);
  glLinkProgram(program_id);
#if defined(_DEBUG) || defined(DEBUGFAST)
  glGetProgramiv(program_id, GL_LINK_STATUS, &result);
  glGetProgramInfoLog(program_id, 1024, &string_buffer_usage, string_buffer);

  if (result && string_buffer_usage)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL linker warnings:\n{}{}{}", string_buffer, vertexShader,
                  fragmentShader);
  }
  else if (!result && !shader_errors)
  {
    ERROR_LOG_FMT(VIDEO, "GLSL linker error:\n{}{}{}", string_buffer, vertexShader, fragmentShader);
  }
#endif

  // cleanup
  glDeleteShader(vertex_shader_id);
  glDeleteShader(fragment_shader_id);

  return program_id;
}

void EnablePrimitiveRestart(const GLContext* context)
{
  constexpr GLuint PRIMITIVE_RESTART_INDEX = 65535;

  if (context->IsGLES())
  {
    glEnable(GL_PRIMITIVE_RESTART_FIXED_INDEX);
  }
  else
  {
    if (GLExtensions::Version() >= 310)
    {
      glEnable(GL_PRIMITIVE_RESTART);
      glPrimitiveRestartIndex(PRIMITIVE_RESTART_INDEX);
    }
    else
    {
      glEnableClientState(GL_PRIMITIVE_RESTART_NV);
      glPrimitiveRestartIndexNV(PRIMITIVE_RESTART_INDEX);
    }
  }
}
}  // namespace GLUtil
