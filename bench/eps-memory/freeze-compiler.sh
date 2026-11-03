#!/usr/bin/env bash
# Freeze the current ghc-debug-enabled compiler of a hadrian build root under a
# label, so later rebuilds don't overwrite it.
#
#   bench/eps-memory/freeze-compiler.sh LABEL [BUILD_ROOT]
#
# BUILD_ROOT defaults to _default+ipe. The stage1 install (bin + lib) is
# relocatable: ghc finds its libdir and shared libraries relative to its
# executable ($ORIGIN RUNPATH, ${pkgroot} package db).
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
label=${1:?usage: freeze-compiler.sh LABEL [BUILD_ROOT]}
root=${2:-_default+ipe}
src="$top/$root/stage1"
dst="$here/compilers/$label"

[ -x "$src/bin/ghc" ] || { echo "no compiler at $src/bin/ghc" >&2; exit 1; }
grep -qa GHC_DEBUG_PAUSE_AT_END "$src/bin/ghc" \
  || { echo "$src/bin/ghc lacks the GHC_DEBUG_PAUSE_AT_END hook (ghc/Main.hs)" >&2; exit 1; }

rm -rf "$dst"
mkdir -p "$dst"
cp -a "$src/bin" "$src/lib" "$dst/"
git -C "$top" rev-parse HEAD > "$dst/commit"
cp "$top/compiler/GHC/Unit/External.hs" "$dst/External.hs"
git -C "$top" diff HEAD -- compiler ghc > "$dst/uncommitted.diff" || true
echo "$root" > "$dst/build-root"
"$dst/bin/ghc" --version
echo "frozen $src -> $dst"
