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
mkdir -p HaskLib/List
cp "$src/../../hasklib/HaskLib/List/Prefix.hs" HaskLib/List/
echo 'module Unrelated where; flag :: Bool; flag = True' >Unrelated.hs
cat >NoEquation.hs <<'END'
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NoEquation (withoutDescent) where
import PreludeMapLengthSize (Nat(..), Size)

withoutDescent :: Size -> [a] -> [a]
withoutDescent Zero _ = []
withoutDescent (Succ _) [] = []
withoutDescent n@(Succ _) xs@(x : _) = x : withoutDescent n xs
END
targets=(HaskLib/List/Prefix.hs NoEquation.hs Unrelated.hs)

"$old" "-B$oldlib" "${flags[@]}" --make -no-link "${targets[@]}"
"$old" "-B$oldlib" --show-iface HaskLib/List/Prefix.hi >old.iface
if grep -Fq 'FinalCoreSourceTable T:takeSize' old.iface; then
    echo 'The old compiler already exports the prefix equations.' >&2
    exit 1
fi
sha256sum HaskLib/List/Prefix.hs NoEquation.hs Unrelated.hi >unchanged.sha256

"$new" "-B$newlib" "${flags[@]}" --make -no-link "${targets[@]}" >upgrade.log 2>&1
grep -Eq 'Compiling +HaskLib.List.Prefix ' upgrade.log
grep -Eq 'Compiling +NoEquation ' upgrade.log
if grep -Eq 'Compiling +Unrelated ' upgrade.log; then exit 1; fi
sha256sum --quiet -c unchanged.sha256
"$new" "-B$newlib" --show-iface HaskLib/List/Prefix.hi >new.iface
grep -Fq 'FinalCoreSourceTable T:takeSize' new.iface
"$new" "-B$newlib" --show-iface NoEquation.hi >rejected.iface
if grep -Fq 'FinalCoreSourceTable T:withoutDescent' rejected.iface; then exit 1; fi

sha256sum ./*.hi HaskLib/List/Prefix.hi >warm.sha256
"$new" "-B$newlib" "${flags[@]}" --make -no-link "${targets[@]}" >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then exit 1; fi
sha256sum --quiet -c warm.sha256

# The previous reader can load the new interface and use the ordinary function.
"$old" "-B$oldlib" --show-iface HaskLib/List/Prefix.hi >previous-reader.iface
grep -Fq 'FinalCoreSourceTable T:takeSize' previous-reader.iface
mkdir interface-only
cp -R HaskLib interface-only/
rm interface-only/HaskLib/List/Prefix.hs
cp PreludeMapLengthSize.{hi,o} interface-only/
cd interface-only
cat >Main.hs <<'END'
module Main where
import HaskLib.List.Prefix (takeSize)
import PreludeMapLengthSize (Nat(..))
main :: IO ()
main = print (takeSize (Succ Zero) (True : error "forced the list beyond the bound"))
END
"$old" "-B$oldlib" "${flags[@]}" -c Main.hs
"$old" "-B$oldlib" "${flags[@]}" Main.o HaskLib/List/Prefix.o PreludeMapLengthSize.o -o observe
./observe | grep -Fxq '[True]'
