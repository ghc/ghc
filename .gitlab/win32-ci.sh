#!/bin/bash

# This is the primary driver of the Win32 CI infrastructure.

set -e

# Wrap escape sequences in \[ \] to avoid wrapping issues
local NO_COLOUR="\[\e[0m\]"
local BLACK="\[\e[0;30m\]"
local GRAY="\[\e[1,30m\]"
local RED="\[\e[0;31m\]"
local LT_RED="\[\e[1;31m\]"
local BROWN="\[\e[0;33m\]"
local LT_BROWN="\[\e[1;33m\]"
local GREEN="\[\e[0;32m\]"
local LT_GREEN="\[\e[1;32m\]"
local BLUE="\[\e[0;34m\]"
local LT_BLUE="\[\e[1;34m\]"
local PURPLE="\[\e[0;35m\]"
local LT_PURPLE="\[\e[1;35m\]"
local CYAN="\[\e[0;36m\]"
local LT_CYAN="\[\e[1;36m\]"
local WHITE="\[\e[1;37m\]"
local LT_GRAY="\[\e[0,37m\]"

echo_color() {
  local color="$1"
  local msg="$2"
  echo "${color}${msg}${NO_COLOR}"
}

error() { echo_color "${RED}" "$1" }
warn() { echo_color "${LT_BROWN}" "$1" }
info() { echo_color "${LT_BLUE}" "$1" }

fail() { error "$1"; exit 1 }

function run() {
  info "Running $@..."
  $@ || ( error "$@ failed"; return 1 )
}

if [ -z "$GHC_VERSION" ]; then
  fail "GHC_VERSION not set"
fi

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
mkdir -p tmp
export TMP=$TOP/tmp
export TEMP=$TOP/tmp

# Extract GHC toolchain
setup() {
  if [ -d "`pwd`/cabal-cache" ]; then
      info "Extracting cabal cache..."
      cp -Rf cabal-cache $APPDATA/cabal
  fi

  if [ ! -e $toolchain/bin/ghc ]; then
      url="https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-$triple.tar.xz"
      info "Fetching GHC binary distribution from $url..."
      curl $url | tar -xJ
      mv ghc-$GHC_VERSION/* toolchain
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
    ALEX=$toolchain/bin/alex \
    || ( cat config.log; fail "configure failed" )
}

build_make() {
  echo "include mk/flavours/${BUILD_FLAVOUR}.mk" > mk/build.mk
  echo 'GhcLibHcOpts+=-haddock' >> mk/build.mk
  run make -j`mk/detect-cpu-count.sh`
  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
  ls -lh ghc.tar.xz
}

fetch_perf_notes() {
  info "Fetching perf notes..."
  git fetch \
    https://gitlab.haskell.org/ghc/ghc-performance-notes.git \
    refs/notes/perf:refs/notes/perf \
    || warn "warning: Failed to fetch perf notes"
}

test_make() {
  run make binary-dist-prep TAR_COMP_OPTS=-1
  run make test_bindist TEST_PREP=YES
  run make V=0 test \
    THREADS=`mk/detect-cpu-count.sh` \
    JUNIT_FILE=../../junit.xml
}

clean_make() {
  rm -R tmp
  make clean || true
}

build_hadrian() {
  run hadrian/build.cabal.sh \
    --flavour=$FLAVOUR \
    -j`mk/detect-cpu-count.sh` \
    --flavour=Quick \
    --docs=no-sphinx \
    binary-dist

  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
}

test_hadrian() {
  cd _build/bindist/ghc-*/
  run ./configure --prefix=$TOP/_build/install
  run make install
  cd ../../../

  # skipping perf tests for now since we build a quick-flavoured GHC,
  # which might result in some broken perf tests?
  run hadrian/build.cabal.sh \
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
  *) fail "unknown mode $1" ;;
esac
