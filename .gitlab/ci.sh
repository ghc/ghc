#!/usr/bin/env bash
# shellcheck disable=SC2230
# shellcheck disable=SC1090

# This is the primary driver of the GitLab CI infrastructure.
# Run `ci.sh usage` for usage information.
set -Eeuo pipefail

# Configuration:
HACKAGE_INDEX_STATE="2024-05-13T15:04:38Z"
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
  set +e
  ( set -e ; $@ )
  res=$?
  set -e
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

  usage             Show this usage message.
  setup             Prepare environment for a build.
  configure         Run ./configure.
  clean             Clean the tree
  shell             Run an interactive shell with a configured build environment.
  save_test_output  Generate unexpected-test-output.tar.gz
  save_cache        Preserve the cabal cache

Hadrian build system
  build_hadrian Build GHC via the Hadrian build system
  test_hadrian  Test GHC via the Hadrian build system

Environment variables affecting both build systems:

  CROSS_TARGET      Triple of cross-compilation target.
  VERBOSE           Set to non-empty for verbose build output
  RUNTEST_ARGS      Arguments passed to runtest.py
  MSYSTEM           (Windows-only) Which platform to build from (CLANG64).
  IGNORE_PERF_FAILURES
                    Whether to ignore perf failures (one of "increases",
                    "decreases", or "all")
  HERMETIC          Take measures to avoid looking at anything in \$HOME
  CONFIGURE_ARGS    Arguments passed to configure script.
  CONFIGURE_WRAPPER Wrapper for the configure script (e.g. Emscripten's emconfigure).
  ENABLE_NUMA       Whether to enable numa support for the build (disabled by default)
  INSTALL_CONFIGURE_ARGS
                    Arguments passed to the binary distribution configure script
                    during installation of test toolchain.
  NIX_SYSTEM        On Darwin, the target platform of the desired toolchain
                    (either "x86-64-darwin" or "aarch-darwin")
  NO_BOOT           Whether to run ./boot or not, used when testing the source dist

Environment variables determining build configuration of Hadrian system:

  BUILD_FLAVOUR     Which flavour to build.
  REINSTALL_GHC     Build and test a reinstalled "stage3" ghc built using cabal-install
                    This tests the "reinstall" configuration
  CROSS_EMULATOR    The emulator to use for testing of cross-compilers.

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
    CLANG64)
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
  # And need to use sphinx-build from the environment
  export SPHINXBUILD="/mingw64/bin/sphinx-build.exe"
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
      if [ "$(uname)" = "FreeBSD" ]; then
        GHC=/usr/local/bin/ghc
      fi
      ;;
    nix)
      if [[ ! -f toolchain.sh ]]; then
        case "$NIX_SYSTEM" in
          x86_64-darwin|aarch64-darwin) ;;
          *) fail "unknown NIX_SYSTEM" ;;
        esac
        info "Building toolchain for $NIX_SYSTEM"
        nix-build --quiet .gitlab/darwin/toolchain.nix --argstr system "$NIX_SYSTEM" -o toolchain.sh
      fi
      source toolchain.sh
      ;;
    env)
      # These are generally set by the Docker image but
      # we provide these handy fallbacks in case the
      # script isn't run from within a GHC CI docker image.
      : ${GHC:=$(which ghc)}
      : ${CABAL:=$(which cabal)}
      : ${HAPPY:=$(which happy)}
      : ${ALEX:=$(which alex)}
      ;;
    *) fail "bad toolchain_source"
  esac

  export GHC
  export CABAL
  export HAPPY
  export ALEX

  if [[ "${CROSS_TARGET:-}" == *"wasm"* ]]; then
    source "/home/ghc/.ghc-wasm/env"
  fi
}

function cabal_update() {
  # In principle -w shouldn't be necessary here but with
  # cabal-install 3.8.1.0 it is, due to cabal#8447.
  run "$CABAL" update -w "$GHC" "hackage.haskell.org,${HACKAGE_INDEX_STATE}"
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

  cabal_update || fail "cabal update failed"

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

  info "====================================================="
  info "ghc --info"
  info "====================================================="
  $GHC --info
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
          cp -r ghc-${GHC_VERSION}*/* "$toolchain"
          ;;
        *)
          pushd ghc-${GHC_VERSION}*
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

      start_section "fetch cabal"
      case "$(uname)" in
        # N.B. Windows uses zip whereas all others use .tar.xz
        MSYS_*|MINGW*)
          case "$MSYSTEM" in
            CLANG64) cabal_arch="x86_64" ;;
            *) fail "unknown MSYSTEM $MSYSTEM" ;;
          esac
          url="https://downloads.haskell.org/~cabal/cabal-install-$v/cabal-install-$v-$cabal_arch-windows.zip"
          info "Fetching cabal binary distribution from $url..."
          curl "$url" > "$TMP/cabal.zip"
          unzip "$TMP/cabal.zip"
          mv cabal.exe "$CABAL"
          ;;
        *)
          local base_url="https://downloads.haskell.org/~cabal/cabal-install-$v/"
          case "$(uname)" in
            Darwin) cabal_url="$base_url/cabal-install-$v-x86_64-apple-darwin17.7.0.tar.xz" ;;
            FreeBSD) cabal_url="$base_url/cabal-install-$v-x86_64-freebsd13.tar.xz" ;;
            *) fail "don't know where to fetch cabal-install for $(uname)"
          esac
          echo "Fetching cabal-install from $cabal_url"
          curl "$cabal_url" > cabal.tar.xz
          $TAR -xJf cabal.tar.xz
          mv cabal "$toolchain/bin"
          ;;
      esac
      end_section "fetch cabal"
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
    --ignore-project \
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
  if [ -d .git ]; then
    info "Cleaning submodules..."
    # On Windows submodules can inexplicably get into funky states where git
    # believes that the submodule is initialized yet its associated repository
    # is not valid. Avoid failing in this case with the following insanity.
    git submodule sync || git submodule deinit --force --all
    git submodule update --init
    git submodule foreach git clean -xdf
  else
    info "Not cleaning submodules, not in a git repo"
  fi;
  end_section "clean submodules"
}

function configure() {
  case "${CONFIGURE_WRAPPER:-}" in
    emconfigure) source "$EMSDK/emsdk_env.sh" ;;
    *) ;;
  esac

  if [[ -z "${NO_BOOT:-}" ]]; then
    start_section "booting"
    run python3 boot
    end_section "booting"
  fi

  read -r -a args <<< "${CONFIGURE_ARGS:-}"
  if [[ -n "${target_triple:-}" ]]; then
    args+=("--target=$target_triple")
  fi
  if [[ -n "${ENABLE_NUMA:-}" ]]; then
    args+=("--enable-numa")
    else
    args+=("--disable-numa")
  fi
  if [[ -n ${HAPPY:-} ]]; then
    args+=("HAPPY=$HAPPY")
  fi
  if [[ -n ${ALEX:-} ]]; then
    args+=("ALEX=$ALEX")
  fi

  start_section "configuring"
  # See https://stackoverflow.com/questions/7577052 for a rationale for the
  # args[@] symbol-soup below.
  run ${CONFIGURE_WRAPPER:-} ./configure \
    --enable-tarballs-autodownload \
    "${args[@]+"${args[@]}"}" \
    GHC="$GHC" \
    || ( cat config.log; fail "configure failed" )
  end_section "configuring"
}

function fetch_perf_notes() {
  info "Fetching perf notes..."
  "$TOP/.gitlab/test-metrics.sh" pull
}

function push_perf_notes() {
  if [[ -z "${TEST_ENV:-}" ]]; then
    return
  fi

  if [ -n "${CROSS_TARGET:-}" ] && [ "${CROSS_EMULATOR:-}" != "js-emulator" ]; then
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

function check_msys2_deps() {
  # Ensure that GHC on Windows doesn't have any dynamic dependencies on msys2
  case "$(uname)" in
    MSYS_*|MINGW*)
      sysroot="$(cygpath "$SYSTEMROOT")"
      PATH="$sysroot/System32:$sysroot;$sysroot/Wbem" $@ \
          || fail "'$@' failed; there may be unwanted dynamic dependencies."
      ;;
  esac
}

# If RELEASE_JOB = yes then we skip builds with a validate flavour.
# This has the effect of
#  (1) Skipping validate jobs when trying to do release builds
#  (2) Ensured we don't accidentally build release builds with validate flavour.
#
# We should never try to build a validate build in a release pipeline so this is
# very defensive in case we have made a mistake somewhere.
function check_release_build() {
  if [ "${RELEASE_JOB:-}" == "yes" ] && [[ "${BUILD_FLAVOUR:-}" == *"validate"* ]]
  then
    info "Exiting build because this is a validate build in a release job"
    exit 0;
  fi
}

function build_hadrian() {
  if [ -z "${BIN_DIST_NAME:-}" ]; then
    fail "BIN_DIST_NAME not set"
  fi
  if [ -n "${BIN_DIST_PREP_TAR_COMP:-}" ]; then
    fail "BIN_DIST_PREP_TAR_COMP must not be set for hadrian (you mean BIN_DIST_NAME)"
  fi

  check_release_build

  # Just to be sure, use the same hackage index state when building Hadrian.
  echo "index-state: $HACKAGE_INDEX_STATE" > hadrian/cabal.project.local

  # We can safely enable parallel compression for x64. By the time
  # hadrian calls tar/xz to produce bindist, there's no other build
  # work taking place.
  if [[ "${CI_JOB_NAME:-}" != *"i386"* ]]; then
    export XZ_OPT="${XZ_OPT:-} -T$cores"
  fi

  if [[ -n "${REINSTALL_GHC:-}" ]]; then
    run_hadrian build-cabal -V
  else
    case "$(uname)" in
        MSYS_*|MINGW*)
          run_hadrian test:all_deps reloc-binary-dist -V
          mv _build/reloc-bindist/ghc*.tar.xz "$BIN_DIST_NAME.tar.xz"
          ;;
        *)
          run_hadrian test:all_deps binary-dist -V
          mv _build/bindist/ghc*.tar.xz "$BIN_DIST_NAME.tar.xz"
          ;;
    esac
  fi

}

# run's `make DESTDIR=$1 install` and then
# merges the file tree to the actual destination $2,
# ensuring that `DESTDIR` is properly honoured by the
# build system
function make_install_destdir() {
  local destdir=$1
  local instdir=$2

  mkdir -p "$destdir"
  mkdir -p "$instdir"
  run "$MAKE" DESTDIR="$destdir" install || fail "make install failed"
  # check for empty dir portably
  # https://superuser.com/a/667100
  if find "$instdir" -mindepth 1 -maxdepth 1 | read; then
    fail "$instdir is not empty!"
  fi
  info "merging file tree from $destdir to $instdir"
  cp -a "$destdir/$instdir"/* "$instdir"/
  "$instdir"/bin/${cross_prefix}ghc-pkg recache
}

# install the binary distribution in directory $1 to $2.
function install_bindist() {
  case "${CONFIGURE_WRAPPER:-}" in
    emconfigure) source "$EMSDK/emsdk_env.sh" ;;
    *) ;;
  esac

  local bindist="$1"
  local instdir="$2"
  pushd "$bindist"
  case "$(uname)" in
    MSYS_*|MINGW*)
      mkdir -p "$instdir"
      cp -a * "$instdir"
      ;;
    *)
      read -r -a args <<< "${INSTALL_CONFIGURE_ARGS:-}"

      # FIXME: The bindist configure script shouldn't need to be reminded of
      # the target platform. See #21970.
      if [ -n "${CROSS_TARGET:-}" ]; then
          args+=( "--target=$CROSS_TARGET" "--host=$CROSS_TARGET" )
      fi

      run ${CONFIGURE_WRAPPER:-} ./configure \
          --prefix="$instdir" \
          "${args[@]+"${args[@]}"}" || fail "bindist configure failed"
      make_install_destdir "$TOP"/destdir "$instdir"
      # And check the `--info` of the installed compiler, sometimes useful in CI log.
      "$instdir"/bin/ghc --info
      ;;
  esac
  popd
}

function test_hadrian() {
  check_msys2_deps _build/stage1/bin/ghc --version
  check_release_build

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


  if [[ "${CROSS_EMULATOR:-}" == "NOT_SET" ]]; then
    info "Cannot test cross-compiled build without CROSS_EMULATOR being set."
    return
    # special case for JS backend
  elif [ -n "${CROSS_TARGET:-}" ] && [ "${CROSS_EMULATOR:-}" == "js-emulator" ]; then
    # The JS backend doesn't support CROSS_EMULATOR logic yet
    unset CROSS_EMULATOR
    # run "hadrian test" directly, not using the bindist, even though it did get installed.
    # This is a temporary solution, See !9515 for the status of hadrian support.
    run_hadrian \
      test \
      --summary-junit=./junit.xml \
      --test-have-intree-files    \
      --docs=none                 \
      "runtest.opts+=${RUNTEST_ARGS:-}" \
      "runtest.opts+=--unexpected-output-dir=$TOP/unexpected-test-output" \
      || fail "cross-compiled hadrian main testsuite"
  elif [[ -n "${CROSS_TARGET:-}" ]] && [[ "${CROSS_TARGET:-}" == *"wasm"* ]]; then
    run_hadrian \
      test \
      --summary-junit=./junit.xml \
      "runtest.opts+=${RUNTEST_ARGS:-}" \
      "runtest.opts+=--unexpected-output-dir=$TOP/unexpected-test-output" \
      || fail "hadrian main testsuite targetting $CROSS_TARGET"
  elif [ -n "${CROSS_TARGET:-}" ]; then
    local instdir="$TOP/_build/install"
    local test_compiler="$instdir/bin/${cross_prefix}ghc$exe"
    install_bindist _build/bindist/ghc-*/ "$instdir"
    echo 'main = putStrLn "hello world"' > expected
    run "$test_compiler" -package ghc "$TOP/.gitlab/hello.hs" -o hello
    ${CROSS_EMULATOR:-} ./hello > actual
    run diff expected actual
  elif [[ -n "${REINSTALL_GHC:-}" ]]; then
    run_hadrian \
      test \
      --test-root-dirs=testsuite/tests/stage1 \
      --test-compiler=stage-cabal \
      --test-root-dirs=testsuite/tests/perf \
      --test-root-dirs=testsuite/tests/typecheck \
      "runtest.opts+=${RUNTEST_ARGS:-}" \
      "runtest.opts+=--unexpected-output-dir=$TOP/unexpected-test-output" \
      || fail "hadrian cabal-install test"
  else
    local instdir="$TOP/_build/install"
    local test_compiler="$instdir/bin/${cross_prefix}ghc$exe"
    install_bindist _build/bindist/ghc-*/ "$instdir"

    if [[ "${WINDOWS_HOST}" == "no" ]] && [ -z "${CROSS_TARGET:-}" ]
    then
      run_hadrian \
        test \
        --test-root-dirs=testsuite/tests/stage1 \
        --test-compiler=stage1 \
        "runtest.opts+=${RUNTEST_ARGS:-}" || fail "hadrian stage1 test"
      info "STAGE1_TEST=$?"
    fi

    # Ensure the resulting compiler has the correct bignum-flavour,
    # except for cross-compilers as they may not support the interpreter
    if [ -z "${CROSS_TARGET:-}" ]
    then
      test_compiler_backend=$(${test_compiler} -e "GHC.Num.Backend.backendName")
      if [ $test_compiler_backend != "\"$BIGNUM_BACKEND\"" ]; then
        fail "Test compiler has a different BIGNUM_BACKEND ($test_compiler_backend) than requested ($BIGNUM_BACKEND)"
      fi
    fi

    # If we are doing a release job, check the compiler can build a profiled executable
    if [ "${RELEASE_JOB:-}" == "yes" ]; then
      echo "main = print ()" > proftest.hs
      run ${test_compiler} -prof proftest.hs || fail "hadrian profiled libs test"
      rm proftest.hs
    fi

    run_hadrian \
      test \
      --summary-junit=./junit.xml \
      --test-have-intree-files \
      --test-compiler="${test_compiler}" \
      "runtest.opts+=${RUNTEST_ARGS:-}" \
      "runtest.opts+=--unexpected-output-dir=$TOP/unexpected-test-output" \
      || fail "hadrian main testsuite"

    info "STAGE2_TEST=$?"

  fi
}

function summarise_hi_files() {
  hi_files=$(find . -type f -name "*.hi" | sort)
  for iface in $hi_files; do echo "$iface  $($HC --show-iface "$iface" | grep "  ABI hash:")"; done | tee $OUT/abis
  for iface in $hi_files; do echo "$iface  $($HC --show-iface "$iface" | grep "  interface hash:")"; done | tee $OUT/interfaces
  for iface in $hi_files; do
      fname="$OUT/$(dirname "$iface")"
      mkdir -p "$fname"
      $HC --show-iface "$iface" > "$OUT/$iface"
  done
}

function summarise_o_files() {
  OBJDUMP=$(if test "$(uname)" == "Darwin"; then echo "objdump -m"; else echo "objdump"; fi)
  o_files=$(find . -type f -name "*.o" | sort)
  for o in $o_files; do
      fname="$OUT/objs/$(dirname "$o")"
      mkdir -p "$fname"
      # To later compare object dumps except for the first line which prints the file path
      $OBJDUMP --all-headers "$o" | tail -n+2 > "$OUT/objs/$o.all-headers"
      $OBJDUMP --disassemble-all "$o" | tail -n+2 > "$OUT/objs/$o.disassemble-all"
  done
}

function cabal_abi_test() {
  if [ -z "$OUT" ]; then
    fail "OUT not set"
  fi

  cp -r libraries/Cabal $DIR
  pushd $DIR
  echo $PWD

  start_section "Cabal test: $OUT"
  mkdir -p "$OUT"
  run "$HC" \
    -hidir tmp -odir tmp -fforce-recomp -haddock \
    -iCabal/Cabal/src -XNoPolyKinds Distribution.Simple -j"$cores" \
    "$@" 2>&1 | tee $OUT/log
  summarise_hi_files
  summarise_o_files
  popd
  end_section "Cabal test: $OUT"
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

function check_interfaces(){
  difference=$(diff "$1/$3" "$2/$3") || warn "diff failed"
  if [ -z "$difference" ]
   then
      info "$1 and $2 $3 match"
   else
     echo $difference
     for line in $(echo "$difference" | tr ' ' '\n' | grep ".hi" | sort | uniq); do
       diff "$1/$line" "$2/$line"
     done
     fail "$4"
  fi
}

function check_objects(){

  # Big fast check
  if diff -r "$1" "$2"
  then
    echo "Objects are the same"
  else
    echo "--------------------------------------------------------------------------------"
    echo "Comparing all objects (1. headers, 2. disassembly). Stopping at first failure..."
    echo "--------------------------------------------------------------------------------"

    pushd "$1" >/dev/null
    OBJ_DUMPS=$(find . -type f -name "*.all-headers" -or -name "*.disassemble-all")
    popd >/dev/null

    for dump in $OBJ_DUMPS
    do
      if diff "$1/$dump" "$2/$dump"
      then
        fail "Mismatched object: $dump"
      fi
    done
  fi

}

function abi_test() {
  for i in {1..20}; do info "iteration $i"; run_abi_test; done
}

function run_abi_test() {
  if [ -z "$HC" ]; then
    fail "HC not set"
  fi
  mkdir -p out
  OUT="$PWD/out/run1" DIR=$(mktemp -d XXXX-looooooooong) cabal_abi_test -O0
  OUT="$PWD/out/run2" DIR=$(mktemp -d XXXX-short) cabal_abi_test -O0
  check_interfaces out/run1 out/run2 abis "Mismatched ABI hash"
  check_interfaces out/run1 out/run2 interfaces "Mismatched interface hashes"
  check_objects out/run1 out/run2
}

function save_test_output() {
    tar -czf unexpected-test-output.tar.gz unexpected-test-output
}

function save_cache () {
  info "Storing cabal cache from $CABAL_DIR to $CABAL_CACHE..."
  rm -Rf "$CABAL_CACHE"
  cp -Rf "$CABAL_DIR" "$CABAL_CACHE"
}

function clean() {
  rm -R tmp
  run rm -Rf _build
}

function run_hadrian() {
  if [ -z "${BUILD_FLAVOUR:-}" ]; then
    fail "BUILD_FLAVOUR not set"
  fi
  read -r -a args <<< "${HADRIAN_ARGS:-}"
  if [ -n "${VERBOSE:-}" ]; then args+=("-V"); fi
  # Before running the compiler, unset variables gitlab env vars as these
  # can destabilise the performance test (see #20341)
  (unset $(compgen -v | grep CI_*);
    run "${HADRIAN_PATH:-hadrian/build-cabal}" \
      --flavour="$BUILD_FLAVOUR" \
      -j"$cores" \
      --broken-test="${BROKEN_TESTS:-}" \
      --bignum=$BIGNUM_BACKEND \
      "${args[@]+"${args[@]}"}" \
      "$@")
}

# A convenience function to allow debugging in the CI environment.
function shell() {
  local cmd="${@: 1}"
  if [ -z "$cmd" ]; then
    cmd="bash -i"
  fi
  run $cmd
}

function lint_author(){
  base=$1
  head=$2
  for email in $(git log --format='%ae' $base..$head); do
    if [ $email == "ghc-ci@gitlab-haskell.org" ];
    then
      fail "Commit has GHC CI author, please amend the author information."
    fi
  done
}

function abi_of(){
  DIR=$(realpath $1)
  mkdir -p "$OUT"
  pushd $DIR
  summarise_hi_files
  popd
}

# Checks that the interfaces in folder $1 match the interfaces in folder $2
function compare_interfaces_of(){
  OUT=$PWD/out/run1 abi_of $1
  OUT=$PWD/out/run2 abi_of $2
  check_interfaces out/run1 out/run2 abis "Mismatched ABI hash"
  check_interfaces out/run1 out/run2 interfaces "Mismatched interface hashes"
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
    WINDOWS_HOST="yes"
    ;;
  *)
    exe=""
    WINDOWS_HOST="no"
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
  cross_prefix="$target_triple-"
else
  cross_prefix=""
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

if [[ -z ${BIGNUM_BACKEND:-} ]]; then BIGNUM_BACKEND=gmp; fi

determine_metric_baseline

set_toolchain_paths

case ${1:-help} in
  help|usage) usage ;;
  setup) setup && cleanup_submodules ;;
  configure) time_it "configure" configure ;;
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
  abi_test) abi_test ;;
  cabal_test) cabal_test ;;
  lint_author) shift; lint_author "$@" ;;
  compare_interfaces_of) shift; compare_interfaces_of "$@" ;;
  clean) clean ;;
  save_test_output) save_test_output ;;
  save_cache) save_cache ;;
  shell) shift; shell "$@" ;;
  *) fail "unknown mode $1" ;;
esac
