#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
if (( $# != 4 )); then
    echo "Usage: $0 OLD_GHC OLD_LIB NEW_GHC NEW_LIB" >&2
    exit 2
fi
old=("$(realpath "$1")" "-B$(realpath "$2")")
new=("$(realpath "$3")" "-B$(realpath "$4")")
flags=(-XHaskell2010 -package-env - -O0 -i -i. -dcore-lint -dstg-lint
       -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{home,installed,upgraded}
cd "$out/home"
cat >Library.hs <<'HS'
{-# LANGUAGE LiberalTypeSynonyms, RankNTypes #-}
module Library (Callback) where
type Callback input output = forall a. input a -> output a
HS
cat >Client.hs <<'HS'
{-# LANGUAGE RankNTypes #-}
module Client where
import Library (Callback)
keep :: Callback [] []
keep x = x
HS
"${old[@]}" "${flags[@]}" --make -no-link Client.hs
cp Library.{hi,o} ../installed/
"${new[@]}" "${flags[@]}" --make -no-link Client.hs >upgrade.log 2>&1
grep -Fq 'Compiling Library' upgrade.log
grep -Fq 'Compiling Client' upgrade.log
cp Library.{hi,o} ../upgraded/
"${new[@]}" --show-iface Library.hi >kind
grep -Fq 'liberal-synonym-callback-v1' kind
sha256sum Library.{hi,o} Client.{hi,o} >warm.sha
"${new[@]}" "${flags[@]}" --make -no-link Client.hs >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
sha256sum --quiet -c warm.sha

cd ../installed
cat >Main.hs <<'HS'
{-# LANGUAGE LiberalTypeSynonyms, RankNTypes #-}
module Main where
import Library (Callback)
type Identity a = a
collect :: Callback Identity []
collect x = [x]
main :: IO ()
main = print (collect True, collect 'q')
HS
cp Main.hs ../upgraded/
if "${new[@]}" "${flags[@]}" -c Main.hs >old-kind.log 2>&1; then exit 1; fi
grep -Fq '[GHC-83865]' old-kind.log
cd ../upgraded
if "${old[@]}" "${flags[@]}" -c Main.hs >old-reader.log 2>&1; then exit 1; fi
grep -Fq '[GHC-47808]' old-reader.log
grep -Fq 'liberal-synonym-callback-v1' old-reader.log
"${new[@]}" "${flags[@]}" -c Main.hs
"${new[@]}" "${flags[@]}" Main.o Library.o -o callback
test "$(./callback)" = '([True],"q")'
echo 'Upgraded callback accepted both argument kinds and reused its warm interface.'
