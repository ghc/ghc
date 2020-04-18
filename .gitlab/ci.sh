#!/usr/bin/env bash
# shellcheck disable=SC2230

# This is the primary driver of the GitLab CI infrastructure.

set -e -o pipefail

# Configuration:
hackage_index_state="@1579718451"

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

export LANG=C.UTF-8
export LC_ALL=C.UTF-8

# GitLab Pipelines log section delimiters
# https://gitlab.com/gitlab-org/gitlab-foss/issues/14664
start_section() {
  name="$1"
  echo -e "section_start:$(date +%s):$name\015\033[0K"
}

end_section() {
  name="$1"
  echo -e "section_end:$(date +%s):$name\015\033[0K"
}

echo_color() {
  local color="$1"
  local msg="$2"
  echo -e "\033[${color}m${msg}\033[0m"
}

error() { echo_color "${RED}" "$1"; }
warn() { echo_color "${LT_BROWN}" "$1"; }
info() { echo_color "${LT_BLUE}" "$1"; }

fail() { error "error: $1"; exit 1; }

function run() {
  info "Running $*..."
  "$@" || ( error "$* failed"; return 1; )
}

TOP="$(pwd)"

function mingw_init() {
  case "$MSYSTEM" in
    MINGW32)
      triple="i386-unknown-mingw32"
      boot_triple="i386-unknown-mingw32" # triple of bootstrap GHC
      ;;
    MINGW64)
      triple="x86_64-unknown-mingw32"
      boot_triple="x86_64-unknown-mingw32" # triple of bootstrap GHC
      ;;
    *)
      fail "win32-init: Unknown MSYSTEM $MSYSTEM"
      ;;
  esac

  # Bring mingw toolchain into PATH.
  # This is extracted from /etc/profile since this script inexplicably fails to
  # run under gitlab-runner.
  # shellcheck disable=SC1091
  source /etc/msystem
  MINGW_MOUNT_POINT="${MINGW_PREFIX}"
  PATH="$MINGW_MOUNT_POINT/bin:$PATH"

  # We always use mingw64 Python to avoid path length issues like #17483.
  export PYTHON="/mingw64/bin/python3"
}

# This will contain GHC's local native toolchain
toolchain="$TOP/toolchain"
mkdir -p "$toolchain/bin"
PATH="$toolchain/bin:$PATH"

export METRICS_FILE="$CI_PROJECT_DIR/performance-metrics.tsv"

cores="$(mk/detect-cpu-count.sh)"

# Use a local temporary directory to ensure that concurrent builds don't
# interfere with one another
mkdir -p "$TOP/tmp"
export TMP="$TOP/tmp"
export TEMP="$TOP/tmp"

function darwin_setup() {
  # It looks like we already have python2 here and just installing python3
  # does not work.
  brew upgrade python
  brew install ghc cabal-install ncurses gmp

  pip3 install sphinx
  # PDF documentation disabled as MacTeX apparently doesn't include xelatex.
  #brew cask install mactex
}

function show_tool() {
  local tool="$1"
  info "$tool = ${!tool}"
  ${!tool} --version
}

function set_toolchain_paths() {
  needs_toolchain=1
  case "$(uname)" in
    Linux) needs_toolchain="" ;;
    *) ;;
  esac

  if [[ -n "$needs_toolchain" ]]; then
      # These are populated by setup_toolchain
      GHC="$toolchain/bin/ghc$exe"
      CABAL="$toolchain/bin/cabal$exe"
      HAPPY="$toolchain/bin/happy$exe"
      ALEX="$toolchain/bin/alex$exe"
  else
      GHC="$(which ghc)"
      CABAL="/usr/local/bin/cabal"
      HAPPY="$HOME/.cabal/bin/happy"
      ALEX="$HOME/.cabal/bin/alex"
  fi
  export GHC
  export CABAL
  export HAPPY
  export ALEX
}

# Extract GHC toolchain
function setup() {
  if [ -d "$TOP/cabal-cache" ]; then
      info "Extracting cabal cache..."
      mkdir -p "$cabal_dir"
      cp -Rf cabal-cache/* "$cabal_dir"
  fi

  if [[ -n "$needs_toolchain" ]]; then
    setup_toolchain
  fi
  case "$(uname)" in
    Darwin) darwin_setup ;;
    *) ;;
  esac

  # Make sure that git works
  git config user.email "ghc-ci@gitlab-haskell.org"
  git config user.name "GHC GitLab CI"

  info "====================================================="
  info "Toolchain versions"
  info "====================================================="
  show_tool GHC
  show_tool CABAL
  show_tool HAPPY
  show_tool ALEX
}

function fetch_ghc() {
  local v="$GHC_VERSION"
  if [[ -z "$v" ]]; then
      fail "GHC_VERSION is not set"
  fi

  if [ ! -e "$GHC" ]; then
      start_section "fetch GHC"
      url="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-${boot_triple}.tar.xz"
      info "Fetching GHC binary distribution from $url..."
      curl "$url" > ghc.tar.xz || fail "failed to fetch GHC binary distribution"
      tar -xJf ghc.tar.xz || fail "failed to extract GHC binary distribution"
      case "$(uname)" in
        MSYS_*|MINGW*)
          cp -r "ghc-${GHC_VERSION}"/* "$toolchain"
          ;;
        *)
          pushd "ghc-${GHC_VERSION}"
          ./configure --prefix="$toolchain"
          "$MAKE" install
          popd
          ;;
      esac
      rm -Rf "ghc-${GHC_VERSION}" ghc.tar.xz
      end_section "fetch GHC"
  fi

}

function fetch_cabal() {
  local v="$CABAL_INSTALL_VERSION"
  if [[ -z "$v" ]]; then
      fail "CABAL_INSTALL_VERSION is not set"
  fi

  if [ ! -e "$CABAL" ]; then
      start_section "fetch GHC"
      case "$(uname)" in
        # N.B. Windows uses zip whereas all others use .tar.xz
        MSYS_*|MINGW*)
          case "$MSYSTEM" in
            MINGW32) cabal_arch="i386" ;;
            MINGW64) cabal_arch="x86_64" ;;
            *) fail "unknown MSYSTEM $MSYSTEM" ;;
          esac
          url="https://downloads.haskell.org/~cabal/cabal-install-$v/cabal-install-$v-$cabal_arch-unknown-mingw32.zip"
          info "Fetching cabal binary distribution from $url..."
          curl "$url" > "$TMP/cabal.zip"
          unzip "$TMP/cabal.zip"
          mv cabal.exe "$CABAL"
          ;;
        *)
          local base_url="https://downloads.haskell.org/~cabal/cabal-install-$v/"
          case "$(uname)" in
            Darwin) cabal_url="$base_url/cabal-install-$v-x86_64-apple-darwin17.7.0.tar.xz" ;;
            FreeBSD)
              #cabal_url="$base_url/cabal-install-$v-x86_64-portbld-freebsd.tar.xz" ;;
              cabal_url="http://home.smart-cactus.org/~ben/ghc/cabal-install-3.0.0.0-x86_64-portbld-freebsd.tar.xz" ;;
            *) fail "don't know where to fetch cabal-install for $(uname)"
          esac
          echo "Fetching cabal-install from $cabal_url"
          curl "$cabal_url" > cabal.tar.xz
          tar -xJf cabal.tar.xz
          mv cabal "$toolchain/bin"
          ;;
      esac
      end_section "fetch GHC"
  fi
}

# For non-Docker platforms we prepare the bootstrap toolchain
# here. For Docker platforms this is done in the Docker image
# build.
function setup_toolchain() {
  fetch_ghc
  fetch_cabal
  cabal_install="$CABAL v2-install --index-state=$hackage_index_state --installdir=$toolchain/bin"
  # Avoid symlinks on Windows
  case "$(uname)" in
    MSYS_*|MINGW*) cabal_install="$cabal_install --install-method=copy" ;;
    *) ;;
  esac

  if [ ! -e "$HAPPY" ]; then
      info "Building happy..."
      cabal update
      $cabal_install happy
  fi

  if [ ! -e "$ALEX" ]; then
      info "Building alex..."
      cabal update
      $cabal_install alex
  fi
}

function cleanup_submodules() {
  start_section "clean submodules"
  info "Cleaning submodules..."
  # On Windows submodules can inexplicably get into funky states where git
  # believes that the submodule is initialized yet its associated repository
  # is not valid. Avoid failing in this case with the following insanity.
  git submodule sync --recursive || git submodule deinit --force --all
  git submodule update --init --recursive
  git submodule foreach git clean -xdf
  end_section "clean submodules"
}

function prepare_build_mk() {
  if [[ -z "$BUILD_FLAVOUR" ]]; then fail "BUILD_FLAVOUR is not set"; fi
  if [[ -z ${BUILD_SPHINX_HTML:-} ]]; then BUILD_SPHINX_HTML=YES; fi
  if [[ -z ${BUILD_SPHINX_PDF:-} ]]; then BUILD_SPHINX_PDF=YES; fi
  if [[ -z ${INTEGER_LIBRARY:-} ]]; then INTEGER_LIBRARY=integer-gmp; fi

  cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_SPHINX_HTML=$BUILD_SPHINX_HTML
BUILD_SPHINX_PDF=$BUILD_SPHINX_PDF
BeConservative=YES
INTEGER_LIBRARY=$INTEGER_LIBRARY
XZ_CMD=$XZ

BuildFlavour=$BUILD_FLAVOUR
ifneq "\$(BuildFlavour)" ""
include mk/flavours/\$(BuildFlavour).mk
endif
GhcLibHcOpts+=-haddock
EOF

  if [ -n "$HADDOCK_HYPERLINKED_SOURCES" ]; then
    echo "EXTRA_HADDOCK_OPTS += --hyperlinked-source --quickjump" >> mk/build.mk
  fi

  case "$(uname)" in
    Darwin) echo "libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-intree-gmp" >> mk/build.mk ;;
    *) ;;
  esac

  info "build.mk is:"
  cat mk/build.mk
}

function configure() {
  start_section "booting"
  run python3 boot
  end_section "booting"

  local target_args=""
  if [[ -n "$triple" ]]; then
    target_args="--target=$triple"
  fi

  start_section "configuring"
  run ./configure \
    --enable-tarballs-autodownload \
    $target_args \
    $CONFIGURE_ARGS \
    GHC="$GHC" \
    HAPPY="$HAPPY" \
    ALEX="$ALEX" \
    || ( cat config.log; fail "configure failed" )
  end_section "configuring"
}

function build_make() {
  prepare_build_mk
  if [[ -z "$BIN_DIST_PREP_TAR_COMP" ]]; then
    fail "BIN_DIST_PREP_TAR_COMP is not set"
  fi

  echo "include mk/flavours/${BUILD_FLAVOUR}.mk" > mk/build.mk
  echo 'GhcLibHcOpts+=-haddock' >> mk/build.mk
  run "$MAKE" -j"$cores" $MAKE_ARGS
  run "$MAKE" -j"$cores" binary-dist-prep TAR_COMP_OPTS=-1
  ls -lh "$BIN_DIST_PREP_TAR_COMP"
}

function fetch_perf_notes() {
  info "Fetching perf notes..."
  "$TOP/.gitlab/test-metrics.sh" pull
}

function push_perf_notes() {
  info "Pushing perf notes..."
  "$TOP/.gitlab/test-metrics.sh" push
}

function test_make() {
  run "$MAKE" test_bindist TEST_PREP=YES
  run "$MAKE" V=0 test \
    THREADS="$cores" \
    JUNIT_FILE=../../junit.xml
}

function build_hadrian() {
  if [ -z "$FLAVOUR" ]; then
    fail "FLAVOUR not set"
  fi

  run_hadrian binary-dist

  mv _build/bindist/ghc*.tar.xz ghc.tar.xz
}

function test_hadrian() {
  cd _build/bindist/ghc-*/
  run ./configure --prefix="$TOP"/_build/install
  run "$MAKE" install
  cd ../../../

  run_hadrian \
    test \
    --summary-junit=./junit.xml \
    --test-compiler="$TOP"/_build/install/bin/ghc
}

function clean() {
  rm -R tmp
  run "$MAKE" --quiet clean || true
  run rm -Rf _build
}

function run_hadrian() {
  run hadrian/build-cabal \
    --flavour="$FLAVOUR" \
    -j"$cores" \
    --broken-test="$BROKEN_TESTS" \
    $HADRIAN_ARGS \
    $@
}

# A convenience function to allow debugging in the CI environment.
function shell() {
  local cmd=$@
  if [ -z "$cmd" ]; then
    cmd="bash -i"
  fi
  run $cmd
}

# Determine Cabal data directory
case "$(uname)" in
  MSYS_*|MINGW*) exe=".exe"; cabal_dir="$APPDATA/cabal" ;;
  *) cabal_dir="$HOME/.cabal"; exe="" ;;
esac

# Platform-specific environment initialization
MAKE="make"
case "$(uname)" in
  MSYS_*|MINGW*) mingw_init ;;
  Darwin) boot_triple="x86_64-apple-darwin" ;;
  FreeBSD)
    boot_triple="x86_64-portbld-freebsd"
    MAKE="gmake"
    ;;
  Linux) ;;
  *) fail "uname $(uname) is not supported" ;;
esac

set_toolchain_paths

case $1 in
  setup) setup && cleanup_submodules ;;
  configure) configure ;;
  build_make) build_make ;;
  test_make) fetch_perf_notes; test_make; push_perf_notes ;;
  build_hadrian) build_hadrian ;;
  test_hadrian) fetch_perf_notes; test_hadrian; push_perf_notes ;;
  run_hadrian) run_hadrian $@ ;;
  clean) clean ;;
  shell) shell $@ ;;
  *) fail "unknown mode $1" ;;
esac
