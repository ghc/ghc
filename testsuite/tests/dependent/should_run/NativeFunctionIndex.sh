#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in 0 1; do
    mkdir "O$level"
    (
        cd "O$level"
        # Exercise CSE on nested calls with branch proofs and on retained operands.
        cp "$area"/{PackedVecLibrary,ConstructorCallClient}.hs .
        "${g[@]}" "-O$level" "${flags[@]}" --make ConstructorCallClient.hs \
            -main-is ConstructorCallClient -o calls >calls.log 2>&1 || { cat calls.log; exit 1; }
        ./calls >calls.stdout
        mkdir provider consumer client
        cp "$area/FunctionFieldLibrary.hs" provider/
        cd provider
        "${g[@]}" "-O$level" "${flags[@]}" -ddump-simpl-iterations -ddump-worker-wrapper -c FunctionFieldLibrary.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
        grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
        if [[ "$level" == 1 ]]; then
            grep -Fq 'Worker Wrapper binds' retained.log || { cat retained.log; exit 1; }
        fi
        cp FunctionFieldLibrary.hi "$area/FunctionFieldClient.hs" ../consumer/
        cd ../consumer
        "${g[@]}" -O0 "${flags[@]}" -c FunctionFieldClient.hs
        cp FunctionField{Library,Client}.hi "$area/NativeFunctionIndexClient.hs" ../client/
        cd ../client
        "${g[@]}" -O0 "${flags[@]}" -c NativeFunctionIndexClient.hs
        "${g[@]}" NativeFunctionIndexClient.o ../provider/FunctionFieldLibrary.o ../consumer/FunctionFieldClient.o -o run
        ./run >observed
    )
done
diff -u O0/calls.stdout O1/calls.stdout
diff -u O0/client/observed O1/client/observed
cat O1/client/observed

# A polymorphic retained package keeps both call heads in its proof fields.
cat > PolyIso.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module PolyIso (Iso(..), Bit(..), sample) where

import Data.Type.Equality ((:~:)(Refl))

data Iso a b where
    MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
          -> (foreach (x :: a) -> from (to x) :~: x)
          -> (foreach (y :: b) -> to (from y) :~: y)
          -> Iso a b

pack :: foreach (to :: a -> b) -> foreach (from :: b -> a)
     -> (foreach (x :: a) -> from (to x) :~: x)
     -> (foreach (y :: b) -> to (from y) :~: y)
     -> Iso a b
pack to from left right = MkIso to from left right

data Bit = Clear | Set deriving Show

encode :: Bool -> Bit
encode False = Clear
encode True = Set

decode :: Bit -> Bool
decode Clear = False
decode Set = True

decodeEncode :: foreach (x :: Bool) -> decode (encode x) :~: x
decodeEncode False = Refl
decodeEncode True = Refl

encodeDecode :: foreach (x :: Bit) -> encode (decode x) :~: x
encodeDecode Clear = Refl
encodeDecode Set = Refl

sample :: Iso Bool Bit
sample = pack encode decode decodeEncode encodeDecode
HS

cat > ReadIso.hs <<'HS'
{-# LANGUAGE CPP, ExperimentalDependentTypes, GADTs #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import PolyIso

symmetric :: Iso a b -> Iso b a
symmetric (MkIso to from left right) = MkIso from to right left

#ifdef WRONG_FIELD
wrong :: Iso a a -> Iso a a
wrong (MkIso to from left right) = MkIso from to left right
#endif

main :: IO ()
main = case symmetric sample of
    MkIso to from left right ->
        print (to Clear, from True, case left Set of Refl -> case right False of Refl -> True)
HS

for level in 0 2; do
    mkdir -p "iso-O$level/"{provider,client}
    (
        cd "iso-O$level/provider"
        cp "$tmp/PolyIso.hs" .
        "${g[@]}" "-O$level" "${flags[@]}" -c PolyIso.hs
        cp PolyIso.hi "$tmp/ReadIso.hs" ../client/
        cd ../client
        "${g[@]}" "-O$level" -c ReadIso.hs
        "${g[@]}" ReadIso.o ../provider/PolyIso.o -o observe
        [[ $(./observe) == '(False,Set,True)' ]]
        if "${g[@]}" "-O$level" -fforce-recomp -DWRONG_FIELD -c ReadIso.hs >wrong.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' wrong.log; then
            cat wrong.log >&2
            exit 1
        fi
    )
done
