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
    echo 'Build finished successfully'"]


############
# No Deps #
############
FROM dolphin-base AS dolphin-nodeps
ENV USE_SYSTEM_LIBS=OFF

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
# Android    #
##############
FROM mingc/android-build-box:latest AS dolphin-android

RUN git config --global --add safe.directory /dolphin

WORKDIR /dolphin/Source/Android

ENTRYPOINT ["./gradlew", "assembleDebug"]
