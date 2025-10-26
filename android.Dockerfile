# TODO: Not rely on some random image and build one myself for Debian/Alpine/etc
##############
# Android    #
##############
FROM mingc/android-build-box:latest AS dolphin-android

RUN git config --global --add safe.directory /dolphin

WORKDIR /dolphin/Source/Android

ENTRYPOINT ["./gradlew", "assembleDebug"]
