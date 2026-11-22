#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 4 ]]; then
    echo "Usage: $0 OLD_GHC OLD_LIBDIR NEW_GHC NEW_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
oldlib=$(realpath "$2")
new=$(realpath "$3")
newlib=$(realpath "$4")
src=$(cd -- "$(dirname -- "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
flags=(-O2 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
cp "$src/PreludeMapLengthSize.hs" .
echo 'module Unrelated where; flag :: Bool; flag = True' >Unrelated.hs

"$old" "-B$oldlib" "${flags[@]}" --make -no-link PreludeMapLengthSize.hs Unrelated.hs
"$old" "-B$oldlib" --show-iface PreludeMapLengthSize.hi >old.iface
if grep -Fq 'FinalCoreSourceTable T:minSize' old.iface; then
    echo 'The old compiler already exports the minimum equation.' >&2
    exit 1
fi
sha256sum PreludeMapLengthSize.hs Unrelated.hi >unchanged.sha256

# Add only the proof. The ordinary provider must now publish its equations.
cp "$src"/{PreludeZipMapRuntime,PreludeZipRuntime}.hs .
"$new" "-B$newlib" "${flags[@]}" --make -no-link PreludeZipRuntime.hs Unrelated.hs >upgrade.log 2>&1
grep -Eq 'Compiling +PreludeMapLengthSize ' upgrade.log
if grep -Eq 'Compiling +Unrelated ' upgrade.log; then exit 1; fi
sha256sum --quiet -c unchanged.sha256
"$new" "-B$newlib" --show-iface PreludeMapLengthSize.hi >new.iface
grep -Fq 'FinalCoreSourceTable T:minSize' new.iface

sha256sum ./*.hi >warm.sha256
"$new" "-B$newlib" "${flags[@]}" --make -no-link PreludeZipRuntime.hs Unrelated.hs >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then exit 1; fi
sha256sum --quiet -c warm.sha256
