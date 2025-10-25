########
# Base #
########
FROM alpine:latest AS dolphin-base

ENV BUILD_DIR=/build
ENV DOLPHIN_DIR=/dolphin

RUN echo "https://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories
RUN apk update
RUN apk add --no-cache \
        bash \
        coreutils \
        g++ \
        cmake \
        git \
        ninja \
        pkgconfig \
        eudev-dev \
        libevdev-dev \
        qt6-qtbase-dev \
        qt6-qtsvg-dev 
        
RUN git config --global --add safe.directory /dolphin
RUN mkdir -p $BUILD_DIR/Binaries

WORKDIR $BUILD_DIR

# MGBA breaks stuff 24/7 and therefore isn't compatible with upstream
# Cubeb is not in the repos
# SFML in the repos is outdated
ENTRYPOINT ["/bin/sh", "-c", "\
    cd $BUILD_DIR && \
    ln -fs $DOLPHIN_DIR/Data/Sys $BUILD_DIR/Binaries/ && \
    cmake $DOLPHIN_DIR \
        -DLINUX_LOCAL_DEV=true \
        -DCMAKE_BUILD_TYPE=Release \
        -DUSE_SYSTEM_LIBS=${USE_SYSTEM_LIBS:-OFF} \
        -DUSE_SYSTEM_LIBMGBA=OFF \
        -DUSE_SYSTEM_CUBEB=OFF \
        -DUSE_SYSTEM_SFML=OFF \
        -G Ninja \
        -DCMAKE_POLICY_VERSION_MINIMUM=3.5 && \
    ninja && \
    ninja tests && \
    Binaries/Tests/tests && \
    echo 'Build finished successfully'"]


############
# No Deps #
############
FROM dolphin-base AS dolphin-nodeps
ENV USE_SYSTEM_LIBS=OFF

############
# Clang ++ #
############
FROM dolphin-nodeps AS dolphin-nodeps-clang

RUN apk add --no-cache clang lld

ENV CC=clang
ENV CXX=clang++
ENV LDFLAGS="-fuse-ld=lld"

############
# All Deps #
############
FROM dolphin-base AS dolphin-alldeps
ENV USE_SYSTEM_LIBS=ON

RUN apk add --no-cache \
    zlib-dev \
    bzip2-dev \
    lzo-dev \
    xz-dev \
    libffi-dev \
    zstd-dev \
    lz4-dev \
    fftw-dev \
    pulseaudio-dev \
    alsa-lib-dev \
    ffmpeg-dev \
    mesa-dev \
    libxext-dev libxi-dev libxrandr-dev libxinerama-dev \
    libevdev-dev \
    sdl3-dev \
    libusb-dev \
    vulkan-headers \
    vulkan-loader \
    glfw-dev \
    fmt-dev \
    glslang-dev \
    spirv-tools-dev \
    pugixml-dev \
    enet-dev \
    xxhash-dev \
    minizip-ng-dev \
    libspng-dev \
    miniupnpc-dev \
    mbedtls2-dev \
    curl-dev \
    hidapi-dev \
    gtest-dev \
    speexdsp-dev \
    bluez-dev \
    llvm


##############
# Debian     #
##############
FROM debian:13 AS dolphin-debian

ENV DEBIAN_FRONTEND=noninteractive
ENV BUILD_DIR=/build
ENV DOLPHIN_DIR=/dolphin

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y \
        build-essential \
        git \
        cmake \
        ffmpeg \
        libavcodec-dev \
        libavformat-dev \
        libavutil-dev \
        libswscale-dev \
        libevdev-dev \
        libusb-1.0-0-dev \
        libxrandr-dev \
        libxi-dev \
        libpangocairo-1.0-0 \
        qt6-base-private-dev \
        libqt6svg6-dev \
        libbluetooth-dev \
        libasound2-dev \
        libpulse-dev \
        libgl1-mesa-dev \
        libcurl4-openssl-dev \
        ninja-build \
        pkg-config \
        libudev-dev \
        qt6-base-dev \
        qt6-svg-dev \
        libzstd-dev \
        liblz4-dev \
        libfftw3-dev \
        libbz2-dev \
        liblzo2-dev \
        liblzma-dev \
        libxext-dev \
        libxinerama-dev \
        libsdl3-dev \
        libvulkan-dev \
        libglfw3-dev \
        libfmt-dev \
        libpugixml-dev \
        libenet-dev \
        libxxhash-dev \
        libminizip-dev \
        libspng-dev \
        libminiupnpc-dev \
        libmbedtls-dev \
        libhidapi-dev \
        libspeexdsp-dev \
        llvm \
        clang \
        lld \
        glslang-tools \
        glslang-dev \
        libcubeb-dev \
        libmbedtls21 \
        libgtest-dev \
    && rm -rf /var/lib/apt/lists/*

RUN git config --global --add safe.directory /dolphin
RUN mkdir -p $BUILD_DIR/Binaries
WORKDIR $BUILD_DIR

ENTRYPOINT ["/bin/bash", "-c", "\
    cd $BUILD_DIR && \
    ln -fs $DOLPHIN_DIR/Data/Sys $BUILD_DIR/Binaries/ && \
    cmake $DOLPHIN_DIR \
        -DLINUX_LOCAL_DEV=true \
        -DCMAKE_BUILD_TYPE=Release \
        -DUSE_SYSTEM_LIBS=ON \
        -DUSE_SYSTEM_LIBMGBA=OFF \
        -DUSE_SYSTEM_SFML=OFF \
        -DUSE_SYSTEM_MINIZIP-NG=OFF \
        -DUSE_SYSTEM_MBEDTLS=OFF \
        -G Ninja && \
    ninja && \
    ninja tests && \
    Binaries/Tests/tests && \
    echo 'Build finished successfully'"]

##############
# Android    #
##############
FROM mingc/android-build-box:latest AS dolphin-android

RUN git config --global --add safe.directory /dolphin

WORKDIR /dolphin/Source/Android

ENTRYPOINT ["./gradlew", "assembleDebug"]
