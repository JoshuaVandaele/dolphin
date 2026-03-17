cmake_minimum_required(VERSION 3.25...4.2)

project(DSP_InterC LANGUAGES CXX)

if(NOT WIN32)
  message(FATALE_RROR "DSP_InterC can only be built on Windows.")
endif()

if(NOT MSVC)
  message(FATAL_ERROR "DSP_InterC can only be built with MSVC.")
endif()

set(SOURCES
  DSP_InterC/DSP_InterC.cpp
  DSP_InterC/DSPExt.cpp
  DSP_InterC/DSPOpcodes.cpp
  DSP_InterC/OutBuffer.cpp
  DSP_InterC/stdafx.cpp
)

add_executable(DSP_InterC ${SOURCES})

target_include_directories(DSP_InterC PRIVATE DSP_InterC)

target_precompile_headers(DSP_InterC PRIVATE DSP_InterC/stdafx.h)

target_compile_options(DSP_InterC PRIVATE
  /W3 # Set warning level 3
  $<$<CONFIG:Debug>:
    /Od   # No optimisations
    /RTC1 # Runtime checks
    /MDd  # Debug multithreaded DLL runtime
    /ZI   # More debugging info
  >
  $<$<CONFIG:Release>:
    /O2 # Optimisation level 2
    /Oi # Intrinsic functions
    /GL # Whole program optimisation
    /MD # Multithreaded DLL runtime
    /Gy # Function-level linking
    /Zi # Debug info
  >
)

target_link_options(DSP_InterC PRIVATE
  /DEBUG # Generate debug data
  $<$<CONFIG:Release>:
    /OPT:REF # Optimize references
    /OPT:ICF # COMDAT folding
    /LTCG    # Link-time code generation
  >
)
