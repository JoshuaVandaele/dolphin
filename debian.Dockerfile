###############
# Debian base #
###############
FROM debian:latest AS debian-base

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
# Debian base cross #
#####################
FROM debian-base AS debian-base-cross
ENV TARGET=aarch64-linux-gnu
ENV CONFIG_PRESET="unix-release-arm64"
ENV BUILD_PRESET="unix-build-release-arm64"
ENV SYSROOT=/opt/sysroots/$TARGET

RUN apt-get update && \
    apt-get install -y \
        crossbuild-essential-arm64 \
        debootstrap \
        qemu-user-static \
        binfmt-support \
        schroot && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p $SYSROOT
# Need to run
# docker run --privileged --rm tonistiigi/binfmt --install all
# docker run --rm --privileged multiarch/qemu-user-static --reset -p yes
RUN debootstrap --foreign --arch=arm64 stable $SYSROOT http://deb.debian.org/debian/
RUN cp /usr/bin/qemu-aarch64-static $SYSROOT/usr/bin/
RUN chroot $SYSROOT /debootstrap/debootstrap --second-stage
RUN rm $SYSROOT/lib/ld-linux-aarch64.so.1
RUN ln -s /lib/aarch64-linux-gnu/ld-linux-aarch64.so.1 $SYSROOT/lib/ld-linux-aarch64.so.1

#####################
# Debian nodeps gcc #
#####################
FROM debian-base AS debian-nodeps-gcc

#######################
# Debian nodeps clang #
#######################
FROM debian-nodeps-gcc AS debian-nodeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

##################
# Debian alldeps #
##################
FROM debian-base AS debian-alldeps-gcc
ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_MINIZIP-NG=OFF -DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF"

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
    ## SFML 3.0 is not yet shipped on Debian
    # MiniUPNPC
    libminiupnpc-dev \
    # MbedTLS
    ## We are using an outdated 2.x version, and Debian only ships 3.x now
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
# Debian alldeps clang #
########################
FROM debian-alldeps-gcc AS debian-alldeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

############################
# Debian alldeps cross gcc #
############################
FROM debian-base-cross AS debian-alldeps-gcc-cross
ENV CC=aarch64-linux-gnu-gcc
ENV CXX=aarch64-linux-gnu-g++

ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_MINIZIP-NG=OFF -DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF"

RUN chroot $SYSROOT apt update && chroot $SYSROOT apt install -y --no-install-recommends \
    # kernel headers
    linux-libc-dev-arm64-cross \
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
    # tinygltf
    ## TinyGLTF relies on multiple dependencies that are not shipped by Debian
    libtinygltf-dev \
    nlohmann-json3-dev \
    libstb-dev \
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
    ## SFML 3.0 is not yet shipped on Debian
    # MiniUPNPC
    libminiupnpc-dev \
    # MbedTLS
    ## We are using an outdated 2.x version, and Debian only ships 3.x now
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

RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config

RUN rm -rf /var/lib/apt/lists/*


###########################
# Debian nodeps cross gcc #
###########################
FROM debian-base-cross AS debian-nodeps-gcc-cross
ENV CC=aarch64-linux-gnu-gcc
ENV CXX=aarch64-linux-gnu-g++

##############################
# Debian alldeps cross clang #
##############################
FROM debian-alldeps-gcc-cross AS debian-alldeps-clang-cross
ENV CC="clang --target=$TARGET --gcc-install-dir=/usr/lib/gcc-cross/aarch64-linux-gnu/14"
ENV CXX="clang++ --target=$TARGET --gcc-install-dir=/usr/lib/gcc-cross/aarch64-linux-gnu/14"
ENV LDFLAGS="-fuse-ld=lld"

#############################
# Debian nodeps cross clang #
#############################
FROM debian-base-cross AS debian-nodeps-clang-cross
# TODO: I don't like hard coded paths with versioning in them, but Docker can't run commands in ENV :(
ENV CC="clang --target=$TARGET --gcc-install-dir=/usr/lib/gcc-cross/aarch64-linux-gnu/14"
ENV CXX="clang++ --target=$TARGET --gcc-install-dir=/usr/lib/gcc-cross/aarch64-linux-gnu/14"
ENV LDFLAGS="-fuse-ld=lld"

############################
# Debian alldeps cross gcc # OLD
############################
FROM debian-base AS debian-alldeps-gcc-cross-old
ENV CC=aarch64-linux-gnu-gcc
ENV CXX=aarch64-linux-gnu-g++
ENV PKG_CONFIG_LIBDIR=/usr/lib/aarch64-linux-gnu/pkgconfig
ENV PKG_CONFIG_PATH=$PKG_CONFIG_LIBDIR

ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON
ENV EXTRA_CMAKE_ARGS="$EXTRA_CMAKE_ARGS -DUSE_SYSTEM_MINIZIP-NG=OFF -DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF -DENABLE_LLVM=OFF"
ENV CONFIG_PRESET="unix-release-arm64"
ENV BUILD_PRESET="unix-build-release-arm64"

RUN dpkg --add-architecture arm64 \
    && apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y \
        # cross build tools
        crossbuild-essential-arm64

RUN apt-get install -y --no-install-recommends \
    # kernel headers
    linux-libc-dev-arm64-cross \
    # PkgConfig
    pkg-config \
    # OpenGL
    libgl1-mesa-dev:arm64 \
    # X11
    libx11-dev:arm64 \
    libxrandr-dev:arm64 \
    libxi-dev:arm64 \
    # EGL
    libegl1-mesa-dev:arm64 \
    # FFMPEG
    libavcodec-dev:arm64 \
    libavformat-dev:arm64 \
    libavutil-dev:arm64 \
    libswresample-dev:arm64 \
    libswscale-dev:arm64 \
    # udev (Use libeudev-dev if on non-systemd)
    libudev-dev:arm64 \
    # evdev
    libevdev-dev:arm64 \
    # SDL
    libsdl3-dev:arm64 \
    # FMT
    libfmt-dev:arm64 \
    # glslang
    glslang-dev:arm64 \
    glslang-tools:arm64 \
    # tinygltf
    ## TinyGLTF relies on multiple dependencies that are not shipped by Debian
    libtinygltf-dev:arm64 \
    nlohmann-json3-dev:arm64 \
    libstb-dev:arm64 \
    # pugixml
    libpugixml-dev:arm64 \
    # enet
    libenet-dev:arm64 \
    # xxhash
    libxxhash-dev:arm64 \
    # bzip2
    libbz2-dev:arm64 \
    # LZMA
    liblzma-dev:arm64 \
    # zstd
    libzstd-dev:arm64 \
    # zlib
    zlib1g-dev:arm64 \
    # minizip-ng
    ## Not packaged yet (Soon)
    # lzo
    liblzo2-dev:arm64 \
    # lz4
    liblz4-dev:arm64 \
    # spng
    libspng-dev:arm64 \
    # cubeb
    libcubeb-dev:arm64 \
    # libusb
    libusb-1.0-0-dev:arm64 \
    # SFML
    ## libsfml-dev:arm64 \
    ## SFML 3.0 is not yet shipped on Debian
    # MiniUPNPC
    libminiupnpc-dev:arm64 \
    # MbedTLS
    ## We are using an outdated 2.x version, and Debian only ships 3.x now
    # cURL (this could also be libcurl4-gnutls-dev)
    libcurl4-openssl-dev:arm64 \
    # hidapi
    libhidapi-dev:arm64 \
    # mgba
    ## libmgba-dev:arm64 \
    ## Newer MGBA versions are currently broken with Dolphin
    # systemd
    libsystemd-dev:arm64 \
    # gtest
    libgtest-dev:arm64 \
    # ALSA
    libasound2-dev:arm64 \
    # PulseAudio
    libpulse-dev:arm64 \
    # LLVM
    ## llvm-dev:arm64 \
    ## Has a postinstall script that tries to run arm64 binaries on amd64 host
    # BlueZ
    libbluetooth-dev:arm64 \
    # Qt6
    qt6-base-dev:arm64 \
    qt6-base-private-dev:arm64 \
    qt6-svg-dev:arm64 \
    # Gettext
    gettext
RUN rm -rf /var/lib/apt/lists/*

###########################
# Debian nodeps cross gcc # OLD
###########################
FROM debian-base AS debian-nodeps-gcc-cross-old
ENV CC=aarch64-linux-gnu-gcc
ENV CXX=aarch64-linux-gnu-g++
ENV PKG_CONFIG_LIBDIR=/usr/lib/aarch64-linux-gnu/pkgconfig
ENV PKG_CONFIG_PATH=$PKG_CONFIG_LIBDIR

ENV USE_SYSTEM_LIBS=OFF
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=OFF
ENV ENABLE_EVDEV=OFF
ENV CONFIG_PRESET="unix-release-arm64"
ENV BUILD_PRESET="unix-build-release-arm64"

RUN dpkg --add-architecture arm64 \
    && apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y \
        # cross build tools
        crossbuild-essential-arm64

# RUN apt-get install -y --no-install-recommends \
#     # kernel headers
#     linux-libc-dev-arm64-cross

RUN rm -rf /var/lib/apt/lists/*

