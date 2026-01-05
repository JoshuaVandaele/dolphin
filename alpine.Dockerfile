###############
# Alpine base #
###############
FROM alpine:latest AS alpine-base

ENV DOLPHIN_DIR=/dolphin

# linux-headers is required to build libusb from Externals
RUN apk add --no-cache \
    bash \
    git \
    cmake \
    ninja \
    g++ \
    clang \
    lld \
    linux-headers \
    ccache

RUN git config --global --add safe.directory $DOLPHIN_DIR

ENV USE_SYSTEM_LIBS=OFF
ENV ENABLE_HEADLESS=ON
ENV DENABLE_HWDB=OFF
ENV ENABLE_EVDEV=OFF
ENV EXTRA_CMAKE_ARGS=""

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

########################
# Alpine nodeps gcc    #
########################
FROM alpine-base AS alpine-nodeps-gcc

###########################
# Alpine nodeps clang     #
###########################
FROM alpine-nodeps-gcc AS alpine-nodeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

#########################
# Alpine alldeps gcc     #
#########################
FROM alpine-base AS alpine-alldeps-gcc
ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_LIBMGBA=OFF -DUSE_SYSTEM_CUBEB=OFF -DENABLE_LLVM=OFF"

RUN apk add --no-cache \
    # Kernel headers
    linux-headers \
    # PkgConfig
    pkgconf \
    # OpenGL
    glfw-dev \
    # X11
    libx11-dev \
    libxrandr-dev \
    libxi-dev \
    # EGL
    mesa-egl \
    # FFMPEG
    ffmpeg-dev \
    # udev
    eudev-dev \
    # evdev
    libevdev-dev \
    # SDL
    sdl3-dev \
    # fmt
    fmt-dev \
    # glslang
    glslang-dev \
    spirv-tools-dev \
    # pugixml
    pugixml-dev \
    # enet
    enet-dev \
    # xxhash
    xxhash-dev \
    # bzip2
    bzip2-dev \
    # LZMA
    xz-dev \
    # zstd
    zstd-dev \
    # zlib
    zlib-dev \
    # minizip-ng
    minizip-ng-dev \
    # lzo
    lzo-dev \
    # lz4
    lz4-dev \
    # spng
    libspng-dev \
    # cubeb
    ## Not available in repositories
    # libusb
    libusb-dev \
    # SFML
    ## libsfml-dev \
    ## SFML 3.0 is not yet shipped on Alpine
    # MiniUPNPC
    miniupnpc-dev \
    # MbedTLS
    mbedtls2-dev \
    # cURL
    curl-dev \
    # hidapi
    hidapi-dev \
    # mgba
    ## libmgba-dev \
    ## Newer MGBA versions are currently broken with Dolphin
    # systemd
    ## N/A
    # gtest
    gtest-dev \
    # ALSA
    alsa-lib-dev \
    # PulseAudio
    pulseaudio-dev \
    # LLVM
    ## llvm-dev \
    ## LLVM is currently badly packaged
    # BlueZ
    bluez-dev \
    # Qt6
    qt6-qtbase-dev \
    qt6-qtbase-private-dev \
    qt6-qtsvg-dev \
    # Gettext
    gettext

#########################
# Alpine alldeps clang   #
#########################
FROM alpine-alldeps-gcc AS alpine-alldeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"
