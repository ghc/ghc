#!/usr/bin/env bash
# cabal, with the compiler of this source tree.
#   bench/eps-store/cabal.sh build all                         (_build, devel2)
#   BUILD_ROOT=_default+ipe bench/eps-store/cabal.sh build all (optimised)
#
# Each build root gets its own build dir and cabal store: they all report the
# same GHC version, but their libraries differ.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
broot=${BUILD_ROOT:-_build}
root=$top/$broot
cd "$here"
cmd=$1; shift
exec cabal --store-dir="$here/dist-store/$broot" "$cmd" \
  --builddir="$here/dist-newstyle/$broot" \
  -w "$root/stage1/bin/ghc" --with-hc-pkg="$root/stage1/bin/ghc-pkg" "$@"
