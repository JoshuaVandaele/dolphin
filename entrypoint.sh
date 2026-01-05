#!/bin/bash
set -e

if [[ -n "$CC" ]]; then
  echo "Using custom CC: $CC"
  export CC
fi

if [[ -n "$CXX" ]]; then
  echo "Using custom CXX: $CXX"
  export CXX
fi

echo "USE_SYSTEM_LIBS=${USE_SYSTEM_LIBS}"
echo "ENABLE_HEADLESS=${ENABLE_HEADLESS}"
echo "DENABLE_HWDB=${DENABLE_HWDB}"
echo "ENABLE_EVDEV=${ENABLE_EVDEV}"
echo "EXTRA_CMAKE_ARGS=${EXTRA_CMAKE_ARGS}"

if [[ -z "$CONFIG_PRESET" ]]; then
  CONFIG_PRESET="unix-release-x64"
fi
echo "CONFIG_PRESET=${CONFIG_PRESET}"

if [[ -z "$BUILD_PRESET" ]]; then
  BUILD_PRESET="unix-build-release-x64"
fi
echo "BUILD_PRESET=${BUILD_PRESET}"

if [[ -n "$SYSROOT" ]]; then
  SYSROOT_ARG="-DCMAKE_SYSROOT=${SYSROOT}"
  echo "SYSROOT=${SYSROOT}"
fi

echo "Copying source to /dolphin-local/"
rm -rf /dolphin-local/ || true
mkdir -p /dolphin-local/
cp -R "$DOLPHIN_DIR"/. /dolphin-local/
rm -rf /dolphin-local/build/* || true
# rsync -a --exclude build/ --info=progress2 --info=name0 "$src" "$dst""$DOLPHIN_DIR"/ /dolphin-local/
cd /dolphin-local

echo "Clearing /out..."
rm -fr /out/*

echo "Running command:"
echo "cmake --preset \"$CONFIG_PRESET\""
echo "      -DUSE_SYSTEM_LIBS=${USE_SYSTEM_LIBS}"
echo "      -DENABLE_HEADLESS=${ENABLE_HEADLESS}"
echo "      -DENABLE_HWDB=${DENABLE_HWDB}"
echo "      -DENABLE_EVDEV=${ENABLE_EVDEV}"
echo "      ${EXTRA_CMAKE_ARGS}"
echo "      ${SYSROOT_ARG:-}"
cmake --preset "$CONFIG_PRESET" \
      -DUSE_SYSTEM_LIBS=${USE_SYSTEM_LIBS} \
      -DENABLE_HEADLESS=${ENABLE_HEADLESS} \
      -DENABLE_HWDB=${DENABLE_HWDB} \
      -DENABLE_EVDEV=${ENABLE_EVDEV} \
      ${EXTRA_CMAKE_ARGS} \
      ${SYSROOT_ARG:-}

echo "Building using preset: $BUILD_PRESET"
cmake --build --preset "$BUILD_PRESET"
cmake --build --preset "$BUILD_PRESET" --target unittests
echo "Build finished successfully"

echo "Copying build artifacts to /out..."
cp -a /dolphin-local/build /out
