#!/usr/bin/env bash

set -euo pipefail

# also set this when building wasm32-wasi-ghc for production
# deployment of haskell playground, so all the .so files are
# optimized.
export WASM_SO_OPT="--debuginfo --low-memory-unused --strip-dwarf -Oz"

# we'll build a rootfs tarball that contains everything in tmp and
# extracts to /tmp, mapped from here
mkdir ./tmp

$TEST_HC \
  -v0 \
  -package ghc \
  -shared -dynamic \
  -no-keep-hi-files -no-keep-o-files \
  -O2 \
  playground001.hs -o ./tmp/libplayground001.so
rm -f ./*_stub.h ./playground001.hs

# /tmp/clib contains libc/libc++ .so files
cp -r "$(dirname "$TEST_CC")/../share/wasi-sysroot/lib/wasm32-wasi" ./tmp/clib
# trim unneeded stuff in c libdir
find ./tmp/clib -type f ! -name "*.so" -delete
rm -f \
  ./tmp/clib/libsetjmp.so \
  ./tmp/clib/libwasi-emulated-*.so

# /tmp/hslib/lib is the ghc libdir
mkdir ./tmp/hslib
cp -r "$($TEST_HC --print-libdir)" ./tmp/hslib/lib
# unregister Cabal/Cabal-syntax, too big
$GHC_PKG --no-user-package-db --global-package-db=./tmp/hslib/lib/package.conf.d unregister Cabal Cabal-syntax
$GHC_PKG --no-user-package-db --global-package-db=./tmp/hslib/lib/package.conf.d recache
# we only need non-profiling .dyn_hi/.so, trim as much as we can
find ./tmp/hslib/lib "(" \
  -name "*.hi" \
  -o -name "*.a" \
  -o -name "*.p_hi" \
  -o -name "libHS*_p.a" \
  -o -name "*.p_dyn_hi" \
  -o -name "libHS*_p*.so" \
  -o -name "libHSrts*_debug*.so" \
  ")" -delete
rm -rf \
  ./tmp/hslib/lib/doc \
  ./tmp/hslib/lib/html \
  ./tmp/hslib/lib/latex \
  ./tmp/hslib/lib/*.mjs \
  ./tmp/hslib/lib/*.js \
  ./tmp/hslib/lib/*.txt
# HS_SEARCHDIR is something like
# /tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024 which is the
# dynamic-library-dirs that contains all libHS*.so in one place, and
# also static libraries in per-unit directories
HS_SEARCHDIR=$(find ./tmp/hslib/lib -type f -name "*.so" -print0 | xargs -0 -n1 dirname | sort -u | sed "s|^\./|/|")
# hunt down the remaining bits of Cabal/Cabal-syntax. too bad there's
# no ghc-pkg uninstall.
rm -rf ."$HS_SEARCHDIR"/*Cabal*

# fix the hard coded search dir in index.html
SED_IS_GNU=$(sed --version &> /dev/null && echo 1 || echo 0)
if [[ $SED_IS_GNU == "1" ]]; then
  sed -i "s|/tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024|$HS_SEARCHDIR|" ./index.html
else
  sed -i "" "s|/tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024|$HS_SEARCHDIR|" ./index.html
fi

# also set ZSTD_NBTHREADS/ZSTD_CLEVEL when building for production
tar -cf ./rootfs.tar.zst --zstd tmp
rm -rf ./tmp

# pass puppeteer.launch() opts as json
exec ./playground001.js "$1"
