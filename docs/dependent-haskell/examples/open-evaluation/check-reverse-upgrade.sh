#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 4 ]]; then
    echo "Usage: $0 OLD_GHC NEW_GHC COMMON_LIBDIR GHC_INTERNAL_BUILD" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
lib=$(realpath "$3")
build=$(realpath "$4")
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../../../.." && pwd)
source="$root/libraries/ghc-internal/src/GHC/Internal/List.hs"
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

flags=("-B$lib" -v1 -package-env - -hide-all-packages -package-id rts-1.0.3
       -this-unit-id ghc-internal -XHaskell2010 -XNoImplicitPrelude -frebindable-known-names
       -O1 -DBIGNUM_GMP -haddock -dcore-lint -dstg-lint -ddump-hi-diffs
       -i "-i$build" "-I$lib/include" "-I$root/libraries/ghc-internal/include"
       -optP-include "-optP$build/autogen/cabal_macros.h")
list=("${flags[@]}" -c "$source" -o List.o -ohi List.hi)

snapshot() {
    sha256sum "$1".{hi,o}
    stat -c '%n %y' "$1".{hi,o}
}
checked() {
    grep -Fxq '  FinalCoreSourceTable T:reverse' "$1" &&
    grep -Fxq '  FinalCoreSourceTable T:reverseOnto' "$1"
}
unchecked() {
    ! grep -Eq '(SourceTable|type family) T:reverse(Onto)?\b' "$1"
}

# Upgrade a real prior List interface and object after the worker moves.
mkdir installed
cd installed
cp -p "$build/GHC/Internal/List.hi" "$build/GHC/Internal/List.o" .
"$old" "-B$lib" --show-iface List.hi >prior.iface
unchecked prior.iface
"$new" "${list[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
"$new" "-B$lib" --show-iface List.hi >upgraded.iface
checked upgraded.iface
snapshot List >before-warm
"$new" "${list[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
cmp before-warm <(snapshot List)

# The old compiler can also compile the new ordinary library source.
# Change only the compiler to observe the selective refresh.
mkdir "$work/unchanged"
cd "$work/unchanged"
"$old" "${list[@]}" >old.log 2>&1 || { cat old.log >&2; exit 1; }
"$old" "-B$lib" --show-iface List.hi >old.iface
unchecked old.iface
snapshot List >before-old-warm
"$old" "${list[@]}" >old-warm.log 2>&1 || { cat old-warm.log >&2; exit 1; }
cmp before-old-warm <(snapshot List)
"$new" "${list[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
grep -F 'Compiler now checks list reverse equations' upgrade.log
"$new" "-B$lib" --show-iface List.hi >upgraded.iface
checked upgraded.iface
snapshot List >before-warm
"$new" "${list[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
cmp before-warm <(snapshot List)

# The previous reader understands the existing nominal family wire format.
"$old" "-B$lib" --show-iface List.hi >previous-reader.iface
checked previous-reader.iface

cat >Ordinary.hs <<'HS'
module Ordinary where

number :: Int
number = 42
HS
ordinary=("-B$lib" -package-env - -v1 -O1 --make -no-link Ordinary.hs)
"$old" "${ordinary[@]}" >old-ordinary.log 2>&1
snapshot Ordinary >ordinary.before
"$new" "${ordinary[@]}" >new-ordinary.log 2>&1
cmp ordinary.before <(snapshot Ordinary)

mkdir no-code
cd no-code
"$new" "${flags[@]}" -fno-code -fwrite-interface -c "$source" -ohi List.hi >no-code.log 2>&1
"$new" "-B$lib" --show-iface List.hi >nocode.iface
unchecked nocode.iface
