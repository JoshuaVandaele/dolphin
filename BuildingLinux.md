# Building for Linux

- [Building for Linux](#building-for-linux)
  - [Prerequisites](#prerequisites)
    - [Tools](#tools)
    - [Required Dependencies](#required-dependencies)
    - [Optional Dependencies](#optional-dependencies)
    - [System-specific instructions](#system-specific-instructions)
      - [Debian](#debian)
      - [Ubuntu](#ubuntu)
      - [Fedora](#fedora)
      - [Arch](#arch)
      - [Alpine](#alpine)
      - [Chrome/Chromium OS (via Chromebrew)](#chromechromium-os-via-chromebrew)
  - [Checkout Dolphin](#checkout-dolphin)
  - [Building](#building)
    - [Global Build (if unsure, use this option)](#global-build-if-unsure-use-this-option)
    - [Local Build](#local-build)
    - [Portable Build](#portable-build)
    - [Distributable .deb Package](#distributable-deb-package)
  - [Externally Managed Packages](#externally-managed-packages)
  - [Troubleshooting](#troubleshooting)

## Prerequisites

### Tools

- `git`
- `curl`
- `cmake`, version 3.13 or newer for Dolphin 5.0-16995 and newer.
- C++ compiler such as `gcc`, with C++20 support for Dolphin 5.0-17123 and newer.

### Required Dependencies

Libraries will silently fall back to their vendored counterpart if they cannot be found on the system.

|     Dependency     | Vendored | Version  | Note | Debian                                                      | Fedora             | Arch            | Alpine                          |
| :----------------: | :------: | :------: | ---- | ----------------------------------------------------------- | ------------------ | --------------- | ------------------------------- |
| **Kernel Headers** |    ⬜    |          |      | `linux-libc-dev`                                            | `kernel-headers`   | `linux-headers` | `linux-headers`                 |
|      **fmt**       |    ✅    |  >=10.1  |      | `libfmt-dev`                                                | `fmt-devel`        | `fmt`           | `fmt-dev`                       |
|    **glslang**     |    ✅    |  >=15.0  |      | `glslang-tools` `glslang-dev`                               | `glslang-devel`    | `glslang`       | `glslang-dev` `spirv-tools-dev` |
|    **pugixml**     |    ✅    |          |      | `libpugixml-dev`                                            | `pugixml-devel`    | `pugixml`       | `pugixml-dev`                   |
|      **enet**      |    ✅    | >=1.3.18 |      | `libenet-dev`                                               | `enet-devel`       | `enet`          | `enet-dev`                      |
|     **xxhash**     |    ✅    | >=0.8.2  |      | `libxxhash-dev`                                             | `xxhash-devel`     | `xxhash`        | `xxhash-dev`                    |
|     **bzip2**      |    ✅    |          |      | `libbz2-dev`                                                | `bzip2-devel`      | `bzip2`         | `bzip2-dev`                     |
|      **lzma**      |    ✅    |          |      | `liblzma-dev`                                               | `xz-devel`         | `xz`            | `xz-dev`                        |
|      **zstd**      |    ✅    | >=1.4.0  |      | `libzstd-dev`                                               | `libzstd-devel`    | `zstd`          | `zstd-dev`                      |
|      **zlib**      |    ✅    | >=1.3.1  |      | `zlib1g-dev`                                                | `zlib-devel`       | `zlib-ng`       | `zlib-dev`                      |
|   **minizip-ng**   |    ✅    | >=4.0.4  |      | [Missing](https://github.com/zlib-ng/minizip-ng/issues/358) | `minizip-ng-devel` | `minizip-ng`    | `minizip-ng-dev`                |
|      **lzo**       |    ✅    |          |      | `liblzo2-dev`                                               | `lzo-devel`        | `lzo`           | `lzo-dev`                       |
|      **lz4**       |    ✅    |  >=1.8   |      | `liblz4-dev`                                                | `lz4-devel`        | `lz4`           | `lz4-dev`                       |
|      **spng**      |    ✅    |          |      | `libspng-dev`                                               | `libspng-devel`    | `libspng`       | `libspng-dev`                   |
|     **libusb**     |    ✅    |          |      | `libusb-1.0-0-dev`                                          | `libusb1-devel`    | `libusb`        | `libusb-dev`                    |
|      **curl**      |    ✅    |          |      | `libcurl4-openssl-dev` or `libcurl4-gnutls-dev`             | `libcurl-devel`    | `curl`          | `curl-dev`                      |
|     **hidapi**     |    ✅    |          |      | `libhidapi-dev`                                             | `hidapi-devel`     | `hidapi`        | `hidapi-dev`                    |

### Optional Dependencies

|   Dependency   | Vendored |   Version    | Note                                                                  | Debian                                                                                  | Fedora                                                                                                                     | Arch                                                                 | Alpine                                                                                                                    |
| :------------: | :------: | :----------: | --------------------------------------------------------------------- | --------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
|    **Qt6**     |    ⬜    |    >=6.0     | Only optional if `-DENABLE_HEADLESS=ON` is set.                       | `qt6-base-dev` `qt6-base-private-dev` `qt6-svg-dev`                                     | `qt6-qtbase-devel` `qt6-qtbase-private-devel` `qt6-qtsvg-devel`                                                            | `qt6-base` `qt6-svg`                                                 | `qt6-qtbase-dev` `qt6-qtsvg-dev`                                                                                          |
|   **OpenGL**   |    ⬜    |              |                                                                       | `libgl1-mesa-dev`                                                                       | `mesa-libGL-devel`                                                                                                         | `libglvnd`                                                           | `glfw-dev`                                                                                                                |
|    **x11**     |    ⬜    |              |                                                                       | `libx11-dev` `libxrandr-dev` `libxi-dev`                                                | `libX11-devel` `libXrandr-devel` `libXi-devel`                                                                             | `libx11` `libxrandr` `libxi`                                         | `libx11-dev` `libxrandr-dev` `libxi-dev`                                                                                  |
| **PkgConfig**  |    ⬜    |              |                                                                       | `pkg-config`                                                                            | `pkgconf-pkg-config`                                                                                                       | `pkgconf`                                                            | `pkgconf`                                                                                                                 |
|    **egl**     |    ⬜    |              | Disable missing warnings with `-DENABLE_EGL=OFF`.                     | `libegl1-mesa-dev`                                                                      | `mesa-libEGL-devel`                                                                                                        | `libglvnd`                                                           | `mesa-egl`                                                                                                                |
|   **FFmpeg**   |    ⬜    |              | Disable missing warnings with `-DENCODE_FRAMEDUMPS=OFF`.              | `libavcodec-dev` `libavformat-dev` `libavutil-dev` `libswresample-dev` `libswscale-dev` | `libavcodec-free-devel` `libavformat-free-devel` `libavutil-free-devel` `libswresample-free-devel` `libswscale-free-devel` | `ffmpeg`                                                             | `ffmpeg-dev`                                                                                                              |
|  **libudev**   |    ⬜    |              | Only optional if `-DENABLE_HWDB=OFF` and `DENABLE_EVDEV=OFF` are set. | `libudev-dev` or `libeudev-dev`                                                         | `systemd-devel`                                                                                                            | `systemd-libs`                                                       | `eudev-dev`                                                                                                               |
|  **libevdev**  |    ⬜    |              | Only optional if `-DENABLE_EVDEV=OFF` is set.                         | `libevdev-dev`                                                                          | `libevdev-devel`                                                                                                           | `libevdev`                                                           | `libevdev-dev`                                                                                                            |
|    **SDL**     |    ✅    |   >=3.2.0    | Disable with `-DENABLE_SDL=OFF`.                                      | `libsdl3-dev`                                                                           | `SDL3-devel`                                                                                                               | `sdl3`                                                               | `sdl3-dev`                                                                                                                |
|   **cubeb**    |    ✅    |              | Disable with `-DENABLE_CUBEB=OFF`.                                    | `libcubeb-dev`                                                                          | `cubeb-devel`                                                                                                              | `cubeb` (Only in AUR)                                                | Not in the repos                                                                                                          |
|    **SFML**    |    ✅    |    >=3.0     |                                                                       | ~~`libsfml-dev`~~ Version 3.x currently not available outside of testing                | ~~`SFML-devel`~~ Version 3.x [currently not available](https://bugzilla.redhat.com/show_bug.cgi?id=2312363)                | `sfml`                                                               | ~~`sfml-dev`~~ Version 3.x [Currently not available](https://gitlab.alpinelinux.org/alpine/aports/-/merge_requests/85247) |
| **miniupnpc**  |    ✅    |    >=1.6     | Disable with `-DUSE_UPNP=OFF`.                                        | `libminiupnpc-dev`                                                                      | `miniupnpc-devel`                                                                                                          | `miniupnpc`                                                          | `miniupnpc-dev`                                                                                                           |
|  **mbedtls**   |    ✅    | >=2.0, <=3.0 |                                                                       | ~~`libmbedtls-dev`~~ MbedTLS 2.x is no longer shipped from Debian Trixie and forward.   | ~~`mbedtls-devel`~~ MbedTLS 2.x is no longer shipped from Fedora 42 forward.                                               | `mbedtls2`                                                           | `mbedtls2-dev`                                                                                                            |
|   **iconv**    |    ✅    |    >=1.14    |                                                                       | Built-in                                                                                | Built-in                                                                                                                   | `libiconv`                                                           | Built-in                                                                                                                  |
|    **mgba**    |    ✅    |              | Disable with `-DUSE_MGBA=OFF` or `DENABLE_HEADLESS=ON`.               | ~~`libmgba-dev`~~ Newer MGBA versions are currently broken with Dolphin.                | Not in the repos                                                                                                           | ~~`libmgba`~~ Newer MGBA versions are currently broken with Dolphin. | ~~`libmgba-dev `~~ Newer MGBA versions are currently broken with Dolphin.                                                 |
|  **systemd**   |    ⬜    |              |                                                                       | `libsystemd-dev`                                                                        | `systemd-devel`                                                                                                            | `systemd-libs`                                                       | N/A                                                                                                                       |
|   **gtest**    |    ✅    |              | Disable with `-DENABLE_TESTS=OFF`.                                    | `libgtest-dev`                                                                          | `gtest-devel`                                                                                                              | `gtest`                                                              | `gtest-dev`                                                                                                               |
|    **ALSA**    |    ⬜    |              | Disable with `-DENABLE_ALSA=OFF`.                                     | `libasound2-dev`                                                                        | `alsa-lib-devel`                                                                                                           | `alsa-lib`                                                           | `alsa-lib-dev`                                                                                                            |
| **PulseAudio** |    ⬜    |              | Disable with `-DENABLE_PULSEAUDIO=OFF`.                               | `libpulse-dev`                                                                          | `pulseaudio-libs-devel`                                                                                                    | `libpulse`                                                           | `pulseaudio-dev`                                                                                                          |
|    **LLVM**    |    ⬜    |              | Disable with `-DENABLE_LLVM=OFF`.                                     | `llvm-dev`                                                                              | `llvm-devel`                                                                                                               | `llvm`                                                               | ~~`llvm-dev`~~ Broken packaging                                                                                           |
|   **BlueZ**    |    ⬜    |              | Disable with `-ENABLE_BLUEZ=OFF`.                                     | `libbluetooth-dev`                                                                      | `bluez-libs-devel`                                                                                                         | `bluez-libs`                                                         | `bluez-dev`                                                                                                               |
|  **gettext**   |    ⬜    |              |                                                                       | `gettext`                                                                               | `gettext`                                                                                                                  | `gettext`                                                            | `gettext`                                                                                                                 |

### System-specific instructions

#### Debian

CMake Flags: `-DUSE_SYSTEM_MINIZIP-NG=OFF -DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF`

<details>

<summary>Install Command</summary>

```sh
apt install \
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
    ## Not packaged yet (but soon)
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
```

</details>

#### Ubuntu

Ubuntu 18.04 and 20.04 users, please install Qt6 from source or an outside repo. One way to do so is to add [this PPA](https://launchpad.net/~okirby/+archive/ubuntu/qt6-backports) by running `sudo add-apt-repository ppa:okirby/qt6-backports && sudo apt update` before running the next command.

CMake Flags: `-DUSE_SYSTEM_LIBMGBA=OFF`

<details>

<summary>Install Command</summary>

```sh

```

</details>

#### Fedora

CMake Flags: `-DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_MBEDTLS=OFF -DUSE_SYSTEM_LIBMGBA=OFF`

<details>

<summary>Install Command</summary>

```sh
dnf install \
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
```

</details>

#### Arch

CMake Flags: `-DUSE_SYSTEM_CUBEB=OFF -DUSE_SYSTEM_LIBMGBA=OFF`

<details>

<summary>Install Command</summary>

```sh
pacman -Syu \
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
```

</details>

See also [dolphin-emu-git's dependencies](https://aur.archlinux.org/packages/dolphin-emu-git?all_deps=1#pkgdeps)

#### Alpine

CMake Flags: `-DUSE_SYSTEM_SFML=OFF -DUSE_SYSTEM_LIBMGBA=OFF -DUSE_SYSTEM_CUBEB=OFF -DENABLE_LLVM=OFF`

<details>

<summary>Install Command</summary>

```sh
apk add \
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
    qt6-qtsvg-dev \
    # Gettext
    gettext
```

</details>

#### Chrome/Chromium OS (via [Chromebrew](https://github.com/chromebrew/chromebrew))

> [!IMPORTANT]
> This is still a work in progress.

<details>

<summary>Install Command</summary>

```bash
crew install binutils wayland cmake gcc buildessential git libx11 pkg-config libxi libxext qtbase qtwayland mesa libxfixes libxxf86vm libusb libdrm libxrandr gettext linuxheaders enet libxshmfence libunwind lm_sensors elfutils patchelf vulkan_icd_loader

# Intel hardware
crew install xorg_intel_driver

# Nvidia hardware
crew install xorg_nouveau_driver
```

</details>

## Checkout Dolphin

1. `git clone https://github.com/dolphin-emu/dolphin`
2. `cd dolphin`
3. Add necessary submodules and check for new commits

```bash
git -c submodule."Externals/Qt".update=none \
-c submodule."Externals/FFmpeg-bin".update=none \
-c submodule."Externals/libadrenotools".update=none \
submodule update --init --recursive \
&& git pull --recurse-submodules
```

## Building

If your system’s default compiler is too old to support C++20, you’ll need to specify a newer version manually when configuring the build.

For example, on Ubuntu 20.04, you can install `gcc-13` from [this PPA](https://launchpad.net/~ubuntu-toolchain-r/+archive/ubuntu/test) or `clang-18` from [LLVM's APT repository](https://apt.llvm.org/).

After installation, use the newer compiler explicitly during configuration, like so:

```sh
cmake .. -DCMAKE_C_COMPILER=gcc-13 -DCMAKE_CXX_COMPILER=g++-13
# or
cmake .. -DCMAKE_C_COMPILER=clang-18 -DCMAKE_CXX_COMPILER=clang++-18
```

### Global Build (if unsure, use this option)

```bash
mkdir build && cd build

cmake ..

make -j$(nproc)

sudo make install # Optional, to install Dolphin
```

### Local Build

```bash
mkdir build && cd build

cmake .. -DLINUX_LOCAL_DEV=true

make -j$(nproc)

ln -s ../../Data/Sys Binaries/
```

### Portable Build

```bash
mkdir build && cd build

cmake .. -DLINUX_LOCAL_DEV=true

make -j$(nproc)

cp -r ../Data/Sys/ Binaries/

touch Binaries/portable.txt
```

### Distributable .deb Package

Following the instructions from [the PR where this feature was implemented](https://github.com/dolphin-emu/dolphin/pull/10170):

```bash
sudo apt install dpkg-dev file

mkdir build && cd build

cmake .. -DCPACK_PACKAGE_CONTACT="Your Name Here" # You're responsible for what you distribute.

make -j$(nproc)

cpack -G DEB
```

## Externally Managed Packages

Dolphin maintains official Flatpak repositories for both [releases](https://flatpak.dolphin-emu.org/releases.flatpakrepo) and [development builds](https://flatpak.dolphin-emu.org/dev.flatpakrepo). Additionally, on some distributions of Linux, package maintainers provide unofficial, unmodified builds of Dolphin. These are listed below:

- [Debian LTS (Bullseye)](https://packages.debian.org/buster/dolphin-emu), [Ubuntu 24.04 LTS (Noble Numbat)](https://packages.ubuntu.com/focal/dolphin-emu), and older **use a highly outdated build from 2016. Do not install Dolphin Emulator from APT on these versions!**
- [Debian Stable (Trixie)](https://packages.debian.org/search?keywords=dolphin-emu&searchon=names&suite=all&section=all), [Ubuntu 24.10 (Oracular Oriole)](https://packages.ubuntu.com/search?keywords=dolphin-emu&searchon=names), and newer have recent release versions of Dolphin, though they can still become outdated due to software versions being mostly frozen when distribution versions release.
- [Arch Linux](https://archlinux.org/packages/extra/x86_64/dolphin-emu/), [Developement Builds](https://aur.archlinux.org/packages/dolphin-emu-git)
- [Fedora](https://packages.fedoraproject.org/pkgs/dolphin-emu/dolphin-emu/) follows the latest release version but isn't always quite caught up with the most recent.
- [OpenSUSE](https://software.opensuse.org/package/dolphin-emu)
- [Gentoo](https://packages.gentoo.org/packages/games-emulation/dolphin)
- [Flathub](https://flathub.org/apps/details/org.DolphinEmu.dolphin-emu) is updated with the latest release versions. Steam Deck users may recognize this as the version distributed in the desktop mode's Discover store.

## Troubleshooting

- The compiler will normally tell you what's going on if you run into an error. Read the error messages closely and look them up verbatim to see if it's something missing on your system and/or a typo in recent Dolphin commits.
- Need more speed? As an alternative to `cmake ..` during the build process, type `cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS=-march=native -DCMAKE_C_FLAGS=-march=native` instead. The resulting build will be optimized specifically for YOUR processor! The difference is more noticeable the weaker your computer is. Note that this may result in longer compile times, and the binaries you build will almost certainly be unusable on any computer besides your own. AUR users of `*-git` packages can get a similar effect by following <https://wiki.archlinux.org/title/makepkg#Building_optimized_binaries>
