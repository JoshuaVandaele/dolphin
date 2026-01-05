################
# Arch base    #
################
FROM archlinux:latest AS arch-base

ENV DOLPHIN_DIR=/dolphin

RUN pacman -Syu --noconfirm \
    bash \
    git \
    cmake \
    ninja \
    base-devel \
    clang \
    lld \
    ccache
RUN pacman -Scc --noconfirm

RUN git config --global --add safe.directory $DOLPHIN_DIR

ENV USE_SYSTEM_LIBS=OFF
ENV ENABLE_HEADLESS=ON
ENV OPROFILING=OFF
ENV DENABLE_HWDB=OFF
ENV ENABLE_EVDEV=OFF
ENV EXTRA_CMAKE_ARGS=""

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

###########################
# Arch nodeps gcc         #
###########################
FROM arch-base AS arch-nodeps-gcc

###########################
# Arch nodeps clang       #
###########################
FROM arch-nodeps-gcc AS arch-nodeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

###########################
# Arch alldeps gcc        #
###########################
FROM arch-base AS arch-alldeps-gcc
ENV USE_SYSTEM_LIBS=ON
ENV ENABLE_HEADLESS=OFF
ENV DENABLE_HWDB=ON
ENV ENABLE_EVDEV=ON

ENV EXTRA_CMAKE_ARGS="-DUSE_SYSTEM_CUBEB=OFF -DUSE_SYSTEM_LIBMGBA=OFF"

RUN pacman -Syu --noconfirm \
    # Kernel headers
    linux-headers \
    # PkgConf
    pkgconf \
    # OpenGL
    libglvnd \
    # X11
    libx11 \
    libxrandr \
    libxi \
    # EGL (Already installed with OpenGL)
    # FFMPEG
    ffmpeg \
    # udev
    systemd-libs \
    # evdev
    libevdev \
    # SDL
    sdl3 \
    # FMT
    fmt \
    # glslang
    glslang \
    # pugixml
    pugixml \
    # enet
    enet \
    # xxhash
    xxhash \
    # bzip2
    bzip2 \
    # LZMA
    xz \
    # zstd
    zstd \
    # zlib
    zlib-ng \
    # minizip-ng
    minizip-ng \
    # lzo
    lzo \
    # lz4
    lz4 \
    # spng
    libspng \
    # cubeb
    ## cubeb \
    ## Currently only in the AUR
    # libusb
    libusb \
    # SFML
    sfml \
    # MiniUPNPC
    miniupnpc \
    # MbedTLS
    mbedtls2 \
    # cURL
    curl \
    # hidapi
    hidapi \
    # systemd (Already installed with udev)
    # gtest
    gtest \
    # ALSA
    alsa-lib \
    # PulseAudio
    libpulse \
    # LLVM
    llvm \
    # BlueZ
    bluez-libs \
    # Qt6
    qt6-base \
    qt6-svg \
    # Gettext
    gettext
RUN pacman -Scc --noconfirm

###########################
# Arch alldeps clang      #
###########################
FROM arch-alldeps-gcc AS arch-alldeps-clang
ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"
