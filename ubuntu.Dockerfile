###############
# Ubuntu base #
###############
FROM ubuntu:latest AS ubuntu-base

ENV DEBIAN_FRONTEND=noninteractive
ENV DOLPHIN_DIR=/dolphin

RUN apt-get update && apt-get upgrade -y && apt-get install -y \
    bash \
    git \
    cmake \
    ninja-build \
    g++ \
    clang \
    lld \
    software-properties-common \
    ccache
RUN rm -rf /var/lib/apt/lists/*

RUN git config --global --add safe.directory $DOLPHIN_DIR

ENV USE_SYSTEM_LIBS=OFF
ENV ENABLE_HEADLESS=ON
ENV DENABLE_HWDB=OFF
ENV ENABLE_EVDEV=OFF
ENV EXTRA_CMAKE_ARGS=""

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

#####################
# Ubuntu nodeps gcc #
#####################
FROM ubuntu-base AS ubuntu-nodeps-gcc

#######################
# Ubuntu nodeps clang #
#######################
FROM ubuntu-nodeps-gcc AS ubuntu-nodeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

##################
# Ubuntu alldeps #
##################
FROM ubuntu-base AS ubuntu-alldeps-gcc
ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_MINIZIP-NG=OFF -DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF -DUSE_SYSTEM_ENET=OFF -DUSE_SYSTEM_ZLIB=OFF"

# SDL3 is not available for 25.04<
RUN add-apt-repository ppa:hrzhu/sdl3-backport
# FMT 10 is not available for 25.04
RUN add-apt-repository ppa:trldp/libfmt
# ENET 1.3.18 is not available for 25.04<
# ZLIB>=1.3.1 is not available for 25.04<


RUN apt-get update && apt-get upgrade -y && apt-get install -y \
    # kernel headers
    linux-libc-dev \
    # PkgConfig
    pkg-config \
    # OpenGL
    libgl1-mesa-dev \
    # X11
    libx11-dev \
    libxrandr-dev \
    libxi-dev \
    # EGL
    libegl1-mesa-dev \
    # FFMPEG
    libavcodec-dev \
    libavformat-dev \
    libavutil-dev \
    libswresample-dev \
    libswscale-dev \
    # udev (Use libeudev-dev if on non-systemd)
    libudev-dev \
    # evdev
    libevdev-dev \
    # SDL
    libsdl3-dev \
    # FMT
    libfmt-dev \
    # glslang
    glslang-dev \
    glslang-tools \
    # pugixml
    libpugixml-dev \
    # enet
    libenet-dev \
    # xxhash
    libxxhash-dev \
    # bzip2
    libbz2-dev \
    # LZMA
    liblzma-dev \
    # zstd
    libzstd-dev \
    # zlib
    zlib1g-dev \
    # minizip-ng
    ## Not packaged yet (Soon)
    # lzo
    liblzo2-dev \
    # lz4
    liblz4-dev \
    # spng
    libspng-dev \
    # cubeb
    libcubeb-dev \
    # libusb
    libusb-1.0-0-dev \
    # SFML
    ## libsfml-dev \
    ## SFML 3.0 is not yet shipped on Ubuntu
    # MiniUPNPC
    libminiupnpc-dev \
    # MbedTLS
    ## We are using an outdated 2.x version, and Ubuntu only ships 3.x now
    # cURL (this could also be libcurl4-gnutls-dev)
    libcurl4-openssl-dev \
    # hidapi
    libhidapi-dev \
    # mgba
    ## libmgba-dev \
    ## Newer MGBA versions are currently broken with Dolphin
    # systemd
    libsystemd-dev \
    # gtest
    libgtest-dev \
    # ALSA
    libasound2-dev \
    # PulseAudio
    libpulse-dev \
    # LLVM
    llvm-dev \
    # BlueZ
    libbluetooth-dev \
    # Qt6
    qt6-base-dev \
    qt6-base-private-dev \
    qt6-svg-dev \
    # Gettext
    gettext
RUN rm -rf /var/lib/apt/lists/*

########################
# Ubuntu alldeps clang #
########################
FROM ubuntu-alldeps-gcc AS ubuntu-alldeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"
