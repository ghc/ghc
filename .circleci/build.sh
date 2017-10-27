#!/usr/bin/env bash
# vim: sw=2 et

set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

echo 'BUILD_SPHINX_HTML = NO' > mk/validate.mk
echo 'BUILD_SPHINX_PDF = NO' >> mk/validate.mk

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
    ./boot
    ./configure "$@"
    make -j$THREADS
    make test
    make binary-dist
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
