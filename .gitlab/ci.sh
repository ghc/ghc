#!/usr/bin/env bash
# shellcheck disable=SC2230
# shellcheck disable=SC1090

# This is the primary driver of the GitLab CI infrastructure.
# Run `ci.sh usage` for usage information.
set -Eeuo pipefail

# Configuration:
HACKAGE_INDEX_STATE="2020-12-21T14:48:20Z" # TODO dedup with yaml's def
MIN_HAPPY_VERSION="1.20"
MIN_ALEX_VERSION="3.2.6"

TOP="$(pwd)"
if [ ! -d "$TOP/.gitlab" ]; then
  echo "This script expects to be run from the root of a ghc checkout"
fi

CABAL_CACHE="$TOP/${CABAL_CACHE:-cabal-cache}"

source "$TOP/.gitlab/common.sh"

function time_it() {
  local name="$1"
  shift
  local start=$(date +%s)
  local res=0
  $@ || res=$?
  local end=$(date +%s)
  local delta=$(expr $end - $start)

  echo "$name took $delta seconds"
  printf "%15s | $delta" > ci-timings
  return $res
}

function usage() {
  cat <<EOF
$0 - GHC continuous integration driver

Common Modes:

  usage         Show this usage message.
  setup         Prepare environment for a build.
  configure     Run ./configure.
  clean         Clean the tree
  shell         Run an interactive shell with a configured build environment.
  save_cache    Preserve the cabal cache

Make build system:

  build_make    Build GHC via the make build system
  test_make     Test GHC via the make build system

Hadrian build system
  build_hadrian Build GHC via the Hadrian build system
  test_hadrian  Test GHC via the Hadrian build system

Environment variables affecting both build systems:

  CROSS_TARGET      Triple of cross-compilation target.
  VERBOSE           Set to non-empty for verbose build output
  RUNTEST_ARGS      Arguments passed to runtest.py
  MSYSTEM           (Windows-only) Which platform to build form (MINGW64 or MINGW32).
  IGNORE_PERF_FAILURES
                    Whether to ignore perf failures (one of "increases",
                    "decreases", or "all")
  HERMETIC          Take measures to avoid looking at anything in \$HOME
  CONFIGURE_ARGS    Arguments passed to configure script.
  INSTALL_CONFIGURE_ARGS
                    Arguments passed to the binary distribution configure script
                    during installation of test toolchain.
  NIX_SYSTEM        On Darwin, the target platform of the desired toolchain
                    (either "x86-64-darwin" or "aarch-darwin")

Environment variables determining build configuration of Make system:

  BUILD_FLAVOUR     Which flavour to build.
  BUILD_SPHINX_HTML Whether to build Sphinx HTML documentation.
  BUILD_SPHINX_PDF  Whether to build Sphinx PDF documentation.
  INTEGER_LIBRARY   Which integer library to use (integer-simple or integer-gmp).
  HADDOCK_HYPERLINKED_SOURCES
                    Whether to build hyperlinked Haddock sources.
  TEST_TYPE         Which test rule to run.

Environment variables determining build configuration of Hadrian system:

  BUILD_FLAVOUR     Which flavour to build.

Environment variables determining bootstrap toolchain (Linux):

  GHC           Path of GHC executable to use for bootstrapping.
  CABAL         Path of cabal-install executable to use for bootstrapping.
  ALEX          Path of alex executable to use for bootstrapping.
  HAPPY         Path of alex executable to use for bootstrapping.

Environment variables determining bootstrap toolchain (non-Linux):

  GHC_VERSION   Which GHC version to fetch for bootstrapping.
  CABAL_INSTALL_VERSION
                Cabal-install version to fetch for bootstrapping.
EOF
}

function setup_locale() {
  # Musl doesn't provide locale support at all...
  if ! which locale > /dev/null; then
    info "No locale executable. Skipping locale setup..."
    return
  fi

  # BSD grep terminates early with -q, consequently locale -a will get a
  # SIGPIPE and the pipeline will fail with pipefail.
  shopt -o -u pipefail
  if locale -a | grep -q C.UTF-8; then
    # Debian
    export LANG=C.UTF-8
  elif locale -a | grep -q C.utf8; then
    # Fedora calls it this
    export LANG=C.utf8
  elif locale -a | grep -q en_US.UTF-8; then
    # Centos doesn't have C.UTF-8
    export LANG=en_US.UTF-8
  elif locale -a | grep -q en_US.utf8; then
    # Centos doesn't have C.UTF-8
    export LANG=en_US.utf8
  else
    error "Failed to find usable locale"
    info "Available locales:"
    locale -a
    fail "No usable locale, aborting..."
  fi
  info "Using locale $LANG..."
  export LC_ALL=$LANG
  shopt -o -s pipefail
}

function mingw_init() {
  case "$MSYSTEM" in
    MINGW32)
      target_triple="i386-unknown-mingw32"
      boot_triple="i386-unknown-mingw32" # triple of bootstrap GHC
      ;;
    MINGW64)
      target_triple="x86_64-unknown-mingw32"
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

export METRICS_FILE="$TOP/performance-metrics.tsv"

cores="$(mk/detect-cpu-count.sh)"

# Use a local temporary directory to ensure that concurrent builds don't
# interfere with one another
mkdir -p "$TOP/tmp"
export TMP="$TOP/tmp"
export TEMP="$TOP/tmp"

function show_tool() {
  local tool="$1"
  info "$tool = ${!tool}"
  ${!tool} --version
}

function set_toolchain_paths() {
  case "$(uname -m)-$(uname)" in
    # Linux toolchains are included in the Docker image
    *-Linux) toolchain_source="env" ;;
    # Darwin toolchains are provided via .gitlab/darwin/toolchain.nix
    *-Darwin) toolchain_source="nix" ;;
    *) toolchain_source="extracted" ;;
  esac

  case "$toolchain_source" in
    extracted)
      # These are populated by setup_toolchain
      GHC="$toolchain/bin/ghc$exe"
      CABAL="$toolchain/bin/cabal$exe"
      HAPPY="$toolchain/bin/happy$exe"
      ALEX="$toolchain/bin/alex$exe"
      ;;
    nix)
      if [[ ! -f toolchain.sh ]]; then
        case "$NIX_SYSTEM" in
          x86_64-darwin|aarch64-darwin) ;;
          *) fail "unknown NIX_SYSTEM" ;;
        esac
        nix build -f .gitlab/darwin/toolchain.nix --argstr system "$NIX_SYSTEM" -o toolchain.sh
        cat toolchain.sh
      fi
      source toolchain.sh ;;
    env)
      # These are generally set by the Docker image but
      # we provide these handy fallbacks in case the
      # script isn't run from within a GHC CI docker image.
      if [ -z "$GHC" ]; then GHC="$(which ghc)"; fi
      if [ -z "$CABAL" ]; then CABAL="$(which cabal)"; fi
      if [ -z "$HAPPY" ]; then HAPPY="$(which happy)"; fi
      if [ -z "$ALEX" ]; then ALEX="$(which alex)"; fi
      ;;
    *) fail "bad toolchain_source"
  esac

  export GHC
  export CABAL
  export HAPPY
  export ALEX
}

function cabal_update() {
  "$CABAL" update --index="$HACKAGE_INDEX_STATE"
}

# Extract GHC toolchain
function setup() {
  echo "=== TIMINGS ===" > ci-timings

  if [ -d "$CABAL_CACHE" ]; then
      info "Extracting cabal cache from $CABAL_CACHE to $CABAL_DIR..."
      mkdir -p "$CABAL_DIR"
      cp -Rf "$CABAL_CACHE"/* "$CABAL_DIR"
  fi

  case $toolchain_source in
    extracted) time_it "setup" setup_toolchain ;;
    *) ;;
  esac

  cabal_update

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
  if [ ! -e "$GHC" ]; then
      local v="$GHC_VERSION"
      if [[ -z "$v" ]]; then
          fail "neither GHC nor GHC_VERSION are not set"
      fi

      start_section "fetch GHC"
      url="https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-${boot_triple}.tar.xz"
      info "Fetching GHC binary distribution from $url..."
      curl "$url" > ghc.tar.xz || fail "failed to fetch GHC binary distribution"
      $TAR -xJf ghc.tar.xz || fail "failed to extract GHC binary distribution"
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
  if [ ! -e "$CABAL" ]; then
      local v="$CABAL_INSTALL_VERSION"
      if [[ -z "$v" ]]; then
          fail "neither CABAL nor CABAL_INSTALL_VERSION are not set"
      fi

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
          $TAR -xJf cabal.tar.xz
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
  cabal_update

  local cabal_install="$CABAL v2-install \
    --with-compiler=$GHC \
    --index-state=$HACKAGE_INDEX_STATE \
    --installdir=$toolchain/bin \
    --overwrite-policy=always"

  # Avoid symlinks on Windows
  case "$(uname)" in
    MSYS_*|MINGW*) cabal_install="$cabal_install --install-method=copy" ;;
    *) ;;
  esac

  info "Building happy..."
  $cabal_install happy --constraint="happy>=$MIN_HAPPY_VERSION"

  info "Building alex..."
  $cabal_install alex --constraint="alex>=$MIN_ALEX_VERSION"
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
  if [[ -z ${BIGNUM_BACKEND:-} ]]; then BIGNUM_BACKEND=gmp; fi

  cat > mk/build.mk <<EOF
BIGNUM_BACKEND=${BIGNUM_BACKEND}
include mk/flavours/${BUILD_FLAVOUR}.mk
GhcLibHcOpts+=-haddock
EOF

  if [ -n "${HADDOCK_HYPERLINKED_SOURCES:-}" ]; then
    echo "EXTRA_HADDOCK_OPTS += --hyperlinked-source --quickjump" >> mk/build.mk
  fi


  info "build.mk is:"
  cat mk/build.mk
}

function configure() {
  start_section "booting"
  run python3 boot
  end_section "booting"

  read -r -a args <<< "${CONFIGURE_ARGS:-}"
  if [[ -n "${target_triple:-}" ]]; then
    args+=("--target=$target_triple")
  fi

  start_section "configuring"
  # See https://stackoverflow.com/questions/7577052 for a rationale for the
  # args[@] symbol-soup below.
  run ./configure \
    --enable-tarballs-autodownload \
    "${args[@]+"${args[@]}"}" \
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
  if [[ -n "${VERBOSE:-}" ]]; then
    MAKE_ARGS="${MAKE_ARGS:-} V=1"
  else
    MAKE_ARGS="${MAKE_ARGS:-} V=0"
  fi

  run "$MAKE" -j"$cores" "$MAKE_ARGS"
  run "$MAKE" -j"$cores" binary-dist-prep TAR_COMP_OPTS=-1
  ls -lh "$BIN_DIST_PREP_TAR_COMP"
}

function fetch_perf_notes() {
  info "Fetching perf notes..."
  "$TOP/.gitlab/test-metrics.sh" pull
}

function push_perf_notes() {
  if [ -n "${CROSS_TARGET:-}" ]; then
    info "Can't test cross-compiled build."
    return
  fi

  info "Pushing perf notes..."
  "$TOP/.gitlab/test-metrics.sh" push
}

# Figure out which commit should be used by the testsuite driver as a
# performance baseline. See Note [The CI Story].
function determine_metric_baseline() {
  if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}" ]; then
    PERF_BASELINE_COMMIT="$CI_MERGE_REQUEST_DIFF_BASE_SHA"
    export PERF_BASELINE_COMMIT
    info "Using $PERF_BASELINE_COMMIT for performance metric baseline..."
  fi
}

function test_make() {
  if [ -n "${CROSS_TARGET:-}" ]; then
    info "Can't test cross-compiled build."
    return
  fi

  run "$MAKE" test_bindist TEST_PREP=YES
  (unset $(compgen -v | grep CI_*);
    run "$MAKE" V=0 VERBOSE=1 test \
      THREADS="$cores" \
      JUNIT_FILE=../../junit.xml \
      EXTRA_RUNTEST_OPTS="${RUNTEST_ARGS:-}")
}

function build_hadrian() {
  if [ -z "${BIN_DIST_NAME:-}" ]; then
    fail "BIN_DIST_NAME not set"
  fi

  # N.B. First build Hadrian, unsetting MACOSX_DEPLOYMENT_TARGET which may warn
  # if the bootstrap libraries were built with a different version expectation.
  MACOSX_DEPLOYMENT_TARGET="" run_hadrian stage1:exe:ghc-bin
  run_hadrian binary-dist -V

  mv _build/bindist/ghc*.tar.xz "$BIN_DIST_NAME.tar.xz"
}

function test_hadrian() {
  if [ -n "${CROSS_TARGET:-}" ]; then
    info "Can't test cross-compiled build."
    return
  fi

  # Ensure that statically-linked builds are actually static
  if [[ "${BUILD_FLAVOUR}" = *static* ]]; then
    bad_execs=""
    for binary in _build/stage1/bin/*; do
      if ldd "${binary}" &> /dev/null; then
        warn "${binary} is not static!"
        ldd "${binary}"
        echo
        bad_execs="$bad_execs $binary"
      fi
    done
    if [ -n "$bad_execs" ]; then
      fail "the following executables contain dynamic-object references: $bad_execs"
    fi
  fi

  cd _build/bindist/ghc-*/
  case "$(uname)" in
    MSYS_*|MINGW*)
      mkdir -p "$TOP"/_build/install
      cp -a * "$TOP"/_build/install
      ;;
    *)
      read -r -a args <<< "${INSTALL_CONFIGURE_ARGS:-}"
      run ./configure --prefix="$TOP"/_build/install "${args[@]}"
      run "$MAKE" install
      ;;
  esac
  cd ../../../

  run_hadrian \
    test \
    --test-root-dirs=testsuite/tests/stage1 \
    --test-compiler=stage1 \
    "runtest.opts+=${RUNTEST_ARGS:-}"

  run_hadrian \
    test \
    --summary-junit=./junit.xml \
    --test-have-intree-files \
    --test-compiler="$TOP/_build/install/bin/ghc$exe" \
    "runtest.opts+=${RUNTEST_ARGS:-}"
}

function cabal_test() {
  if [ -z "$OUT" ]; then
    fail "OUT not set"
  fi

  start_section "Cabal test: $OUT"
  mkdir -p "$OUT"
  run "$HC" \
    -hidir tmp -odir tmp -fforce-recomp \
    -dumpdir "$OUT/dumps" -ddump-timings \
    +RTS --machine-readable "-t$OUT/rts.log" -RTS \
    -ilibraries/Cabal/Cabal/src -XNoPolyKinds Distribution.Simple \
    "$@" 2>&1 | tee $OUT/log
  rm -Rf tmp
  end_section "Cabal test: $OUT"
}

function run_perf_test() {
  if [ -z "$HC" ]; then
    fail "HC not set"
  fi

  mkdir -p out
  git -C libraries/Cabal/ rev-parse HEAD > out/cabal_commit
  $HC --print-project-git-commit-id > out/ghc_commit
  OUT=out/Cabal-O0 cabal_test -O0
  OUT=out/Cabal-O1 cabal_test -O1
  OUT=out/Cabal-O2 cabal_test -O2
}

function save_cache () {
  info "Storing cabal cache from $CABAL_DIR to $CABAL_CACHE..."
  cp -Rf "$CABAL_DIR" "$CABAL_CACHE"
}

function clean() {
  rm -R tmp
  run "$MAKE" --quiet clean || true
  run rm -Rf _build
}

function run_hadrian() {
  if [ -z "${BUILD_FLAVOUR:-}" ]; then
    fail "BUILD_FLAVOUR not set"
  fi
  if [ -z "${BIGNUM_BACKEND:-}" ]; then BIGNUM_BACKEND="gmp"; fi
  read -r -a args <<< "${HADRIAN_ARGS:-}"
  if [ -n "${VERBOSE:-}" ]; then args+=("-V"); fi
  # Before running the compiler, unset variables gitlab env vars as these
  # can destabilise the performance test (see #20341)
  (unset $(compgen -v | grep CI_*);
    run hadrian/build-cabal \
      --flavour="$BUILD_FLAVOUR" \
      -j"$cores" \
      --broken-test="${BROKEN_TESTS:-}" \
      --bignum=$BIGNUM_BACKEND \
      "${args[@]+"${args[@]}"}" \
      "$@")
}

# A convenience function to allow debugging in the CI environment.
function shell() {
  local cmd="*@"
  if [ -z "$cmd" ]; then
    cmd="bash -i"
  fi
  run "$cmd"
}

setup_locale

# Platform-specific environment initialization
if [ -n "${HERMETIC:-}" ]; then
  export CABAL_DIR="$TOP/cabal"
  # We previously set HOME=/nonexistent but apparently nix wants $HOME to exist
  # so sadly we must settle for someplace writable.
  export HOME="$TOP/tmp-home"
else
  BIN_DIST_NAME="${BIN_DIST_NAME:-}"
  case "$(uname)" in
    MSYS_*|MINGW*) CABAL_DIR="$APPDATA/cabal" ;;
    *) CABAL_DIR="$HOME/.cabal" ;;
  esac
fi

case "$(uname)" in
  MSYS_*|MINGW*)
    exe=".exe"
    # N.B. cabal-install expects CABAL_DIR to be a Windows path
    CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
    ;;
  *)
    exe=""
    ;;
esac

MAKE="make"
TAR="tar"
case "$(uname)" in
  MSYS_*|MINGW*) mingw_init ;;
  Darwin) boot_triple="x86_64-apple-darwin" ;;
  FreeBSD)
    boot_triple="x86_64-portbld-freebsd"
    MAKE="gmake"
    TAR="gtar"
    ;;
  Linux) ;;
  *) fail "uname $(uname) is not supported" ;;
esac

if [ -n "${CROSS_TARGET:-}" ]; then
  info "Cross-compiling for $CROSS_TARGET..."
  target_triple="$CROSS_TARGET"
fi

echo "Branch name ${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-}"
# Ignore performance improvements in @marge-bot batches.
# See #19562.
if [ "${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-}" == "wip/marge_bot_batch_merge_job" ]; then
  if [ -z "${IGNORE_PERF_FAILURES:-}" ]; then
    IGNORE_PERF_FAILURES="decreases"
    echo "Ignoring perf failures"
  fi
fi
echo "CI_COMMIT_BRANCH: ${CI_COMMIT_BRANCH:-}"
echo "CI_PROJECT_PATH: ${CI_PROJECT_PATH:-}"
if [ "${CI_COMMIT_BRANCH:-}" == "master" ] &&  [ "${CI_PROJECT_PATH:-}" == "ghc/ghc" ]; then
  if [ -z "${IGNORE_PERF_FAILURES:-}" ]; then
    IGNORE_PERF_FAILURES="decreases"
    echo "Ignoring perf failures"
  fi
fi
if [ -n "${IGNORE_PERF_FAILURES:-}" ]; then
  RUNTEST_ARGS="--ignore-perf-failures=$IGNORE_PERF_FAILURES"
fi

determine_metric_baseline

set_toolchain_paths

case $1 in
  usage) usage ;;
  setup) setup && cleanup_submodules ;;
  configure) time_it "configure" configure ;;
  build_make) time_it "build" build_make ;;
  test_make)
    fetch_perf_notes
    res=0
    time_it "test" test_make || res=$?
    push_perf_notes
    exit $res ;;
  build_hadrian) time_it "build" build_hadrian ;;
  # N.B. Always push notes, even if the build fails. This is okay to do as the
  # testsuite driver doesn't record notes for tests that fail due to
  # correctness.
  test_hadrian)
    fetch_perf_notes
    res=0
    time_it "test" test_hadrian || res=$?
    push_perf_notes
    exit $res ;;
  run_hadrian) shift; run_hadrian "$@" ;;
  perf_test) run_perf_test ;;
  cabal_test) cabal_test ;;
  clean) clean ;;
  save_cache) save_cache ;;
  shell) shell "$@" ;;
  *) fail "unknown mode $1" ;;
esac
