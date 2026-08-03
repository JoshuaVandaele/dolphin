set(SRCS
  source/ChannelMaps.cpp
  source/KissFFT.cpp
  source/KissFFTR.cpp
  source/FreeSurroundDecoder.cpp
)

add_library(FreeSurround STATIC ${SRCS})
dolphin_disable_warnings(FreeSurround)
target_include_directories(FreeSurround PUBLIC include)
target_compile_options(FreeSurround PRIVATE -w)
