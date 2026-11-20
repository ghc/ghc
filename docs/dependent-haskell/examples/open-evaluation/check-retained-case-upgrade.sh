#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# != 3 ]]; then
    echo "Usage: $0 OLD_GHC CURRENT_GHC COMMON_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
flags=("-B$(realpath "$3")" -O1 -v1 -fhide-source-paths -i -i. -package-env -
       -dcore-lint -dstg-lint -fdiagnostics-color=never)
build=(--make Main.hs -o run)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat >Provider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Provider (choose, render) where
import Prelude
import qualified Prelude as P (not)
import qualified Prelude as Q (not)

choose :: foreach (b :: Bool) -> (case P.not b of { True -> Int; False -> Bool })
choose False = 42
choose True = False

render :: foreach (b :: Bool) -> (case Q.not b of { True -> Int; False -> Bool }) -> String
render False x = show x
render True x = show x
HS
cat >Plain.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module Plain (tag, erased, retained) where
import Data.Proxy (Proxy(..))

tag :: String
tag = "plain"

erased :: forall (b :: Bool). Proxy b -> Proxy (case b of { True -> Int; False -> Bool })
erased _ = Proxy

retained :: foreach (b :: Bool) -> Bool
retained _ = True
HS
cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Plain (tag)
import Provider (choose, render)

main :: IO ()
main = putStrLn (tag ++ " " ++ render False (choose False))
HS
cat >Neutral.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Data.IORef (newIORef, readIORef)
import Plain (tag)
import Provider (choose, render)

main :: IO ()
main = do
    cell <- newIORef False
    b <- readIORef cell
    putStrLn (tag ++ " " ++ render b (choose b))
HS

"$old" "${flags[@]}" "${build[@]}" >old.log 2>&1 || { cat old.log >&2; exit 1; }
test "$(./run)" = 'plain 42'
mkdir old-artifacts
cp Provider.{hi,o} Plain.{hi,o} old-artifacts/
cp Neutral.hs old-artifacts/Main.hs
(
    cd old-artifacts
    if "$new" "${flags[@]}" -v0 -c Main.hs >neutral.log 2>&1; then
        echo 'Expected the neutral call to fail with the old provider interface.' >&2
        exit 1
    fi
    grep -Fq '[GHC-83865]' neutral.log &&
        grep -Eq 'Expected: the type case in the signature for .render.' neutral.log &&
        grep -Eq 'Actual: the type case in the signature for .choose.' neutral.log ||
        { cat neutral.log >&2; exit 1; }
)

reason='Compiler now shares retained signature type cases'
"$new" "${flags[@]}" -ddump-hi-diffs "${build[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
grep -Fq "Compiling Provider [$reason]" upgrade.log || { cat upgrade.log >&2; exit 1; }
grep -Fq 'Compiling Main [Provider changed]' upgrade.log || { cat upgrade.log >&2; exit 1; }
grep -Fq 'Out of date: choose' upgrade.log || { cat upgrade.log >&2; exit 1; }
if grep -Fq 'Compiling Plain ' upgrade.log; then cat upgrade.log >&2; exit 1; fi
cmp old-artifacts/Plain.hi Plain.hi
cmp old-artifacts/Plain.o Plain.o
test "$(./run)" = 'plain 42'

cksum Main.{hi,o} Plain.{hi,o} Provider.{hi,o} >before-warm
"$new" "${flags[@]}" "${build[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
cksum Main.{hi,o} Plain.{hi,o} Provider.{hi,o} >after-warm
cmp before-warm after-warm

mkdir new-artifacts
cp Provider.{hi,o} Plain.{hi,o} new-artifacts/
cp Neutral.hs new-artifacts/Main.hs
(
    cd new-artifacts
    "$new" "${flags[@]}" -v0 -c Main.hs
    "$new" "${flags[@]}" -v0 Provider.o Plain.o Main.o -o run
    test "$(./run)" = 'plain 42'
)
echo 'The retained provider upgraded once. The neutral client ran using only the rebuilt artifacts.'
