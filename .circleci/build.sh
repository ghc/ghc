#!/usr/bin/env bash
# vim: sw=2 et

set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BeConservative=YES
EOF

export THREADS=8
export SKIP_PERF_TESTS=YES
export VERBOSE=2

function run_build() {
    mk/get-win32-tarballs.sh download all
    ./boot
    ./configure "$@"
    make sdist
    mkdir -p sdist-build-dir
    pushd sdist-build-dir
    shopt -s extglob
    tar xvfJ ../sdistprep/ghc-*+([[:digit:]])-src.tar.xz sdist-build-dir
    cd ghc-*
    ./boot
    ./configure "$@"
    make -j$THREADS
    make binary-dist
    popd
    mkdir -p bdist-test-dir
    pushd bdist-test-dir
    tar xvfJ ../sdist-build-dir/ghc-*/ghc-*.tar.xz
    tar xvfJ ../sdistprep/ghc*testsuite.tar.xz
    cd ghc-*
    ./configure
    # TODO: JUnit formatting for pretty CircleCI thing.
    make test
}

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        echo 'HADDOCK_DOCS = NO' >> mk/build.mk
        echo 'WERROR=' >> mk/build.mk
        export PATH=/opt/ghc/bin:$PATH
        run_build --target=x86_64-unknown-freebsd10
      else
        fail "TARGET=$target not supported"
      fi
    else
      run_build
    fi
    ;;
  Darwin)
    if [[ -n ${TARGET:-} ]]; then
      fail "uname=$(uname) not supported for cross-compilation"
    fi
    run_build
    ;;
  *)
    fail "uname=$(uname) not supported"
esac
