################
# Fedora base  #
################
FROM fedora:latest AS fedora-base

ENV DOLPHIN_DIR=/dolphin

RUN dnf -y update && dnf -y upgrade && dnf -y install \
    bash \
    git \
    cmake \
    ninja-build \
    gcc-c++ \
    clang \
    lld \
    ccache
RUN dnf clean all

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
# Fedora nodeps gcc #
#####################
FROM fedora-base AS fedora-nodeps-gcc

#######################
# Fedora nodeps clang #
#######################
FROM fedora-nodeps-gcc AS fedora-nodeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

##################
# Fedora alldeps #
##################
FROM fedora-base AS fedora-alldeps-gcc
ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF"


RUN dnf -y update && dnf -y upgrade && dnf -y install \
    # kernel headers
    kernel-headers \
    # PkgConfig
    pkgconf-pkg-config \
    # OpenGL
    mesa-libGL-devel \
    # X11
    libX11-devel \
    libXrandr-devel \
    libXi-devel \
    # EGL
    mesa-libEGL-devel \
    # FFMPEG
    libavcodec-free-devel \
    libavformat-free-devel \
    libavutil-free-devel \
    libswresample-free-devel \
    libswscale-free-devel \
    # udev
    systemd-devel \
    # evdev
    libevdev-devel \
    # SDL
    SDL3-devel \
    # FMT
    fmt-devel \
    # glslang
    glslang-devel \
    # pugixml
    pugixml-devel \
    # enet
    enet-devel \
    # xxhash
    xxhash-devel \
    # bzip2
    bzip2-devel \
    # LZMA
    xz-devel \
    # zstd
    libzstd-devel \
    # zlib
    zlib-devel \
    # minizip-ng
    minizip-ng-devel \
    # lzo
    lzo-devel \
    # lz4
    lz4-devel \
    # spng
    libspng-devel \
    # cubeb
    cubeb-devel \
    # libusb
    libusb1-devel \
    # SFML
    ## SFML-devel \
    ## SFML 3.0 is not yet shipped on Fedora
    # MiniUPNPC
    miniupnpc-devel \
    # MbedTLS
    ## We are using an outdated 2.x version, and Fedora only ships 3.x now
    # cURL
    libcurl-devel \
    # hidapi
    hidapi-devel \
    # mgba
    ## Not in the Fedora repos
    # systemd (already installed for udev)
    # gtest
    gtest-devel \
    # ALSA
    alsa-lib-devel \
    # PulseAudio
    pulseaudio-libs-devel \
    # LLVM
    llvm-devel \
    # BlueZ
    bluez-libs-devel \
    # Qt6
    qt6-qtbase-devel \
    qt6-qtbase-private-devel \
    qt6-qtsvg-devel \
    # Gettext
    gettext
RUN dnf clean all

########################
# Fedora alldeps clang #
########################
FROM fedora-alldeps-gcc AS fedora-alldeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"
