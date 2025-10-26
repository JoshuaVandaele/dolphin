#!/bin/bash
set -e

echo "USE_SYSTEM_LIBS=${USE_SYSTEM_LIBS}"
echo "ENABLE_HEADLESS=${ENABLE_HEADLESS}"
echo "DENABLE_HWDB=${DENABLE_HWDB}"
echo "ENABLE_EVDEV=${ENABLE_EVDEV}"
echo "EXTRA_CMAKE_ARGS=${EXTRA_CMAKE_ARGS}"

cd "$BUILD_DIR"

ln -fs "$DOLPHIN_DIR/Data/Sys" "$BUILD_DIR/Binaries/"

cmake "$DOLPHIN_DIR" \
    -DLINUX_LOCAL_DEV=true \
    -DCMAKE_BUILD_TYPE=Release \
    -DUSE_SYSTEM_LIBS=${USE_SYSTEM_LIBS} \
    -DENABLE_HEADLESS=${ENABLE_HEADLESS} \
    -DENABLE_HWDB=${DENABLE_HWDB} \
    -DENABLE_EVDEV=${ENABLE_EVDEV} \
    -G Ninja \
    -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
    ${EXTRA_CMAKE_ARGS}

ninja
ninja tests

Binaries/Tests/tests

echo "Build finished successfully"
