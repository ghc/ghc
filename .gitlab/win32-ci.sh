#!/bin/bash

# This is the primary driver of the Win32 CI infrastructure.

set -e -o pipefail

# Colors
BLACK="0;30"
GRAY="1;30"
RED="0;31"
LT_RED="1;31"
BROWN="0;33"
LT_BROWN="1;33"
GREEN="0;32"
LT_GREEN="1;32"
BLUE="0;34"
LT_BLUE="1;34"
PURPLE="0;35"
LT_PURPLE="1;35"
CYAN="0;36"
LT_CYAN="1;36"
WHITE="1;37"
LT_GRAY="0;37"

echo_color() {
  local color="$1"
  local msg="$2"
  echo -e "\e[${color}m${msg}\e[0m"
}

error() { echo_color "${RED}" "$1"; }
warn() { echo_color "${LT_BROWN}" "$1"; }
info() { echo_color "${LT_BLUE}" "$1"; }

fail() { error "$1"; exit 1; }

function run() {
  info "Running $@..."
  $@ || ( error "$@ failed"; return 1; )
}

if [ -z "$GHC_VERSION" ]; then
  fail "GHC_VERSION not set"
fi

case $MSYSTEM in
  MINGW32)
    triple="i686-unknown-mingw32"
    boot_triple="i386-unknown-mingw32" # triple of bootstrap GHC
    cabal_arch="i386"
    ;;
  MINGW64)
    triple="x86_64-unknown-mingw32"
    boot_triple="x86_64-unknown-mingw32" # triple of bootstrap GHC
    cabal_arch="x86_64"
    ;;
  *)
    fail "win32-init: Unknown MSYSTEM $MSYSTEM"
    ;;
esac

TOP="$(pwd)"

# Bring mingw toolchain into PATH.
# This is extracted from /etc/profile since this script inexplicably fails to
# run under gitlab-runner.
source /etc/msystem
MINGW_MOUNT_POINT="${MINGW_PREFIX}"
PATH="$MINGW_MOUNT_POINT/bin:$PATH"

# This will contain GHC's local native toolchain
toolchain="$TOP/toolchain"
mkdir -p $toolchain/bin
PATH="$toolchain/bin:$PATH"

# Use a local temporary directory to ensure that concurrent builds don't
# interfere with one another
mkdir -p $TOP/tmp
export TMP=$TOP/tmp
export TEMP=$TOP/tmp

# Extract GHC toolchain
function setup() {
  if [ -d "$TOP/cabal-cache" ]; then
      info "Extracting cabal cache..."
      cp -Rf cabal-cache $APPDATA/cabal
  fi

  if [ ! -e $toolchain/bin/ghc ]; then
      url="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-${boot_triple}.tar.xz"
      info "Fetching GHC binary distribution from $url..."
      curl $url | tar -xJ \
        || fail "failed to fetch GHC binary distribution"
      cp -r ghc-${GHC_VERSION}/* toolchain
      rm -Rf ghc-${GHC_VERSION}
  fi

  if [ ! -e $toolchain/bin/cabal ]; then
      url="https://downloads.haskell.org/~cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-$cabal_arch-unknown-mingw32.zip"
      info "Fetching cabal binary distribution from $url..."
      curl $url > $TMP/cabal.zip
      unzip $TMP/cabal.zip
      mv cabal.exe $toolchain/bin
  fi

  if [ ! -e $toolchain/bin/happy ]; then
      info "Building happy..."
      cabal update
      cabal install happy
      cp $(cygpath $APPDATA)/cabal/bin/happy $toolchain/bin
  fi

  if [ ! -e $toolchain/bin/alex ]; then
      info "Building alex..."
      cabal update
      cabal install alex
      cp $(cygpath $APPDATA)/cabal/bin/alex $toolchain/bin
  fi

  info "====================================================="
  info "Toolchain versions"
  info "====================================================="
  $toolchain/bin/ghc --version
  $toolchain/bin/cabal --version
  $toolchain/bin/happy --version
  $toolchain/bin/alex --version
}

function cleanup_submodules() {
  # On Windows submodules can inexplicably get into funky states where git
  # believes that the submodule is initialized yet its associated repository
  # is not valid. Avoid failing in this case with the following insanity.
  git submodule sync --recursive || git submodule deinit --force --all
  git submodule update --init --recursive
  git submodule foreach git clean -xdf
}

function configure() {
  run python boot
  run ./configure \
    --enable-tarballs-autodownload \
    --target=$triple \
    $CONFIGURE_ARGS \
    GHC=$toolchain/bin/ghc \
    HAPPY=$toolchain/bin/happy \
    ALEX=$toolchain/bin/alex \
    || ( cat config.log; fail "configure failed" )
}

function build_make() {
  echo "include mk/flavours/${BUILD_FLAVOUR}.mk" > mk/build.mk
  echo 'GhcLibHcOpts+=-haddock' >> mk/build.mk
  run make -j$(mk/detect-cpu-count.sh)
  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
  ls -lh ghc.tar.xz
}

function fetch_perf_notes() {
  info "Fetching perf notes..."
  git fetch \
    https://gitlab.haskell.org/ghc/ghc-performance-notes.git \
    refs/notes/perf:refs/notes/perf \
    || warn "warning: Failed to fetch perf notes"
}

function test_make() {
  run make binary-dist-prep TAR_COMP_OPTS=-1
  run make test_bindist TEST_PREP=YES
  run make V=0 test \
    THREADS=$(mk/detect-cpu-count.sh) \
    JUNIT_FILE=../../junit.xml
}

function clean_make() {
  rm -R tmp
  make clean || true
}

function build_hadrian() {
  run hadrian/build.cabal.sh \
    --flavour=$FLAVOUR \
    -j$(mk/detect-cpu-count.sh) \
    --flavour=Quick \
    --docs=no-sphinx \
    binary-dist

  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
}

function test_hadrian() {
  cd _build/bindist/ghc-*/
  run ./configure --prefix=$TOP/_build/install
  run make install
  cd ../../../

  # skipping perf tests for now since we build a quick-flavoured GHC,
  # which might result in some broken perf tests?
  run hadrian/build.cabal.sh \
    --flavour=$FLAVOUR \
    -j$(mk/detect-cpu-count.sh) \
    --flavour=quick \
    test \
    --summary-junit=./junit.xml \
    --skip-perf \
    --test-compiler=$TOP/_build/install/bin/ghc
}

function clean_hadrian() {
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
  *) fail "unknown mode $1" ;;
esac
