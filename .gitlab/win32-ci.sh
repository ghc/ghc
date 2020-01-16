#!/bin/bash

# This is the primary driver of the Win32 CI infrastructure.

set -e

case $MSYSTEM in
  MINGW32)
    triple="i386-unknown-mingw32"
    cabal_arch="i386"
    ;;
  MINGW64)
    triple="x86_64-unknown-mingw32"
    cabal_arch="x86_64"
    ;;
  *)
    echo "win32-init: Unknown MSYSTEM $MSYSTEM"
    exit 1
    ;;
esac

# Bring mingw64 toolchain into PATH
source /etc/profile

# This will contain GHC's local native toolchain
toolchain="$PWD/toolchain"
PATH="$toolchain/bin:$PATH"

# Use a local temporary directory to ensure that concurrent builds don't
# interfere with one another
mkdir -p tmp
export TMP=$PWD/tmp
export TEMP=$PWD/tmp

function run() {
  echo "Running $@..."
  $@
}

# Extract GHC toolchain
setup() {
  if [ -d "`pwd`/cabal-cache" ]; then
      echo "Extracting cabal cache..."
      cp -Rf cabal-cache $APPDATA/cabal
  fi

  if [ ! -e $toolchain/bin/ghc ]; then
      url="https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-$triple.tar.xz"
      echo "Fetching GHC binary distribution from $url..."
      curl $url | tar -xJ
      mv ghc-$GHC_VERSION toolchain
  fi

  if [ ! -e $toolchain/bin/cabal ]; then
      url="https://downloads.haskell.org/~cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-$cabal_arch-unknown-mingw32.zip"
      echo "Fetching cabal binary distribution from $url..."
      curl $url > /tmp/cabal.zip
      unzip /tmp/cabal.zip
      mv cabal.exe $toolchain/bin
  fi

  if [ ! -e $toolchain/bin/happy ]; then
      echo "Building happy..."
      cabal update
      cabal install happy
      cp $APPDATA/cabal/bin/happy $toolchain/bin
  fi

  if [ ! -e $toolchain/bin/alex ]; then
      echo "Building alex..."
      cabal update
      cabal install alex
      cp $APPDATA/cabal/bin/alex $toolchain/bin
  fi
}

cleanup_submodules() {
  # On Windows submodules can inexplicably get into funky states where git
  # believes that the submodule is initialized yet its associated repository
  # is not valid. Avoid failing in this case with the following insanity.
  git submodule sync --recursive || git submodule deinit --force --all
  git submodule update --init --recursive
  git submodule foreach git clean -xdf
}

configure() {
  run python boot
  run ./configure \
    --enable-tarballs-autodownload \
    GHC=$toolchain/bin/ghc \
    HAPPY=$toolchain/bin/happy \
    ALEX=$toolchain/bin/alex
}

build_make() {
  echo "include mk/flavours/${BUILD_FLAVOUR}.mk" > mk/build.mk
  echo 'GhcLibHcOpts+=-haddock' >> mk/build.mk
  make -j`mk/detect-cpu-count.sh`
  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
}

fetch_perf_notes() {
  echo "Fetching perf notes..."
  git fetch https://gitlab.haskell.org/ghc/ghc-performance-notes.git refs/notes/perf:refs/notes/perf \
    || echo "warning: Failed to fetch perf notes"
}

test_make() {
  make binary-dist-prep TAR_COMP_OPTS=-1
  make test_bindist TEST_PREP=YES
  make V=0 test \
    THREADS=`mk/detect-cpu-count.sh` \
    JUNIT_FILE=../../junit.xml
}

clean_make() {
  rm -R tmp
  make clean || true
}

build_hadrian() {
  hadrian/build.cabal.sh \
    --flavour=$FLAVOUR \
    -j`mk/detect-cpu-count.sh` \
    --flavour=Quick \
    --docs=no-sphinx \
    binary-dist

  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
}

test_hadrian() {
  export TOP=$(pwd)
  cd _build/bindist/ghc-*/
  ./configure --prefix=$TOP/_build/install
  make install
  cd ../../../

  # skipping perf tests for now since we build a quick-flavoured GHC,
  # which might result in some broken perf tests?
  hadrian/build.cabal.sh \
    --flavour=$FLAVOUR \
    -j`mk/detect-cpu-count.sh` \
    --flavour=quick \
    test \
    --summary-junit=./junit.xml \
    --skip-perf \
    --test-compiler=$TOP/_build/install/bin/ghc
}

clean_hadrian() {
  rm -R tmp
  run rm -Rf _build
}

case $1 in
  setup) setup && cleanup_submodules ;;
  configure) configure ;;
  build_make) build_make ;;
  test_make) fetch_perf_notes; test_make ;;
  clean_make) clean_make ;;
  build_hadrian) build_hadrian ;;
  test_hadrian) fetch_perf_notes; test_hadrian ;;
  clean_hadrian) clean_hadrian ;;
  *)
    echo "unknown mode $1"
    exit 1 ;;
esac
