#!/usr/bin/env bash

# build and GHCJS js-unknown-ghcjs cross-compiler
#
# Run in top-level directory of ghc source tree.
# tested on Linux.
#
# this script is probably temporary and various bits should be worked
# into the GHC build system

##########################################################################
#                           Configuration
#
# Choose quick or perf build

# BUILDTYPE=perf
BUILDTYPE=quick

##########################################################################

# exit when any command fails
set -e

# Find our toolchain
#
# We use emscripten. Another clang based toolchain with wasm32/asmjs
# targets should also be possible to make work.

if [ -d "$EMSDK" ] ; then
    echo "Using emsdk: $EMSDK"
else
    echo -e "Error: Could not find Emscripten SDK.\nCheck the EMSDK environment variable."
    exit 1
fi

# create some wrapper scripts
mkdir -p "inplace/toolchain"
(
    cd inplace/toolchain
    echo "creating toolchain wrappers"

    echo -e "#!/usr/bin/env bash\\nexec \"$EMSDK/upstream/bin/llvm-ar\" \"\$@\"" > js-unknown-ghcjs-ar
    chmod 755 js-unknown-ghcjs-ar

    echo -e "#!/usr/bin/env bash\\nexec \"$EMSDK/upstream/emscripten/emcc\" \"\$@\"" > js-unknown-ghcjs-cc
    chmod 755 js-unknown-ghcjs-cc

    echo -e "#!/usr/bin/env bash\\nexec \"$EMSDK/upstream/bin/llvm-ranlib\" \"\$@\"" > js-unknown-ghcjs-ranlib
    chmod 755 js-unknown-ghcjs-ranlib

    # not sure if this one is correct, but it's probably unused
    echo -e "#!/usr/bin/env bash\\nexec \"$EMSDK/upstream/bin/llvm-ld\" \"\$@\"" > js-unknown-ghcjs-ld
    chmod 755 js-unknown-ghcjs-ld
)

export PATH=$(PWD)/inplace/toolchain:$PATH

./boot
./configure --target=js-unknown-ghcjs

# copy some data files that ghcjs needs
#
# this should be worked into the build system properly
#
# also the dependency on HsBaseConfig.h (included by some shims)
# should be reworked. we copy a cached file here, the file is normally 
# generated later in the build process.
cp -r ghcjs-data/lib inplace/lib

# now set up the correct build profile
echo "BuildFlavour=${BUILDTYPE}-cross-ghcjs" > mk/build.mk
cat mk/build.mk.sample >> mk/build.mk

# Make it so!

make -j8 2>&1 | tee build-ghcjs.log
