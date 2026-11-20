#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,proof,producer}
cd "$out/provider"
cp "$src"/PreludeMapLength{Size,Source,Proof}.hs "$src"/Prelude{Append,Reverse}LengthSource.hs .
"${ghc[@]}" -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -c \
    PreludeMapLengthSize.hs PreludeMapLengthSource.hs PreludeMapLengthProof.hs \
    PreludeAppendLengthSource.hs PreludeReverseLengthSource.hs
cd "$out/proof"
cp ../provider/*.hi "$src/PreludeReverseLengthProof.hs" .
cat >PublicProof.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}
module PublicProof (proof) where

import Data.Type.Equality ((:~:))
import qualified Prelude as P
import PreludeMapLengthProof (Spine)
import PreludeMapLengthSource (Count)
import PreludeReverseLengthProof (lengthReverse)

proof :: Spine xs -> Count (P.reverse xs) :~: Count xs
proof = lengthReverse
HS
cat >Main.hs <<'HS'
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import PreludeReverseLengthProof (showPreludeReverseLength)

{-# NOINLINE firstOfReverse #-}
firstOfReverse :: [a] -> Maybe a
firstOfReverse xs = case reverse xs of
    [] -> Nothing
    x : _ -> Just x

main :: IO ()
main = do
    putStrLn showPreludeReverseLength
    print (firstOfReverse [error "reverse evaluated a skipped element", True])
    print (reverse [1, 2, 3 :: Int])
    partial <- try @ErrorCall (evaluate (firstOfReverse (True : error "reverse demanded the left tail")))
    case partial of
        Left exception | "reverse demanded the left tail" `isInfixOf` displayException exception ->
            putStrLn "reverse demanded the left tail before its first cons"
        _ -> fail "reverse returned before demanding the left tail"
HS
for optimization in -O0 -O2; do
    "${ghc[@]}" "$optimization" -fforce-recomp -c PreludeReverseLengthProof.hs PublicProof.hs Main.hs
    "${ghc[@]}" Main.o PreludeReverseLengthProof.o ../provider/*.o -o proof-run
    ./proof-run >"native$optimization.stdout"
    [[ $(cat "native$optimization.stdout") == $'(Refl,Succ (Succ Zero))\nJust True\n[3,2,1]\nreverse demanded the left tail before its first cons' ]]
done
"${ghc[@]}" --show-iface PublicProof.hi >PublicProof.iface
grep -Fq 'T:reverse xs' PublicProof.iface
if grep -q 'T:reverseOnto' PublicProof.iface; then
    exit 1
fi

for module in Prelude Data.List GHC.List; do
    printf 'module Hidden where\nimport %s (reverseOnto)\n' "$module" >Hidden.hs
    if "${ghc[@]}" -O1 -c Hidden.hs >hidden.log 2>&1 ||
       ! grep -Fq '[GHC-61689]' hidden.log; then
        cat hidden.log >&2
        exit 1
    fi
done

sed -e 's/^        Refl -> case plusSucc xs (Proxy @(Count acc)) of$/        Refl -> Refl/' \
    -e '/^            Refl -> Refl$/d' PreludeReverseLengthProof.hs >MissingSuccessor.hs
if "${ghc[@]}" -O1 -c MissingSuccessor.hs >missing-successor.log 2>&1 ||
   ! grep -Fq 'GHC-05617' missing-successor.log; then
    cat missing-successor.log >&2
    exit 1
fi

# Preserve the existing executable path even for a nested, empty reversal.
cat >ClosedReverse.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module ClosedReverse where

import Data.Type.Equality ((:~:)(Refl))
import qualified Prelude as P

closed :: P.reverse '[2, 3, 5] :~: '[5, 3, 2]
closed = Refl

nested :: P.max (P.reverse '[]) (P.reverse '[ 'P.Just 'x']) :~: '[ 'P.Just 'x']
nested = Refl
HS
"${ghc[@]}" -fexternal-interpreter -O0 -c ClosedReverse.hs

# Check both owner identity and disappearance of every rejected family.
cd "$out/producer"
cp "$src/NativeOpenInstalledReverseProducer.hs" .
build() {
    local label=$1
    shift
    "${ghc[@]}" -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -fforce-recomp \
        -ddump-rule-firings -dppr-cols=160 -Wno-x-partial -c NativeOpenInstalledReverseProducer.hs "$@" >"$label.rules"
    "${ghc[@]}" --show-iface NativeOpenInstalledReverseProducer.hi >"$label.iface"
    grep -Fq 'type family UserReverse' "$label.iface"
}
certified() {
    grep -Fxq "  FinalCoreSourceTable T:$2" "$1.iface"
}
absent() {
    if grep -Eq "(SourceTable|type family) T:$2\b" "$1.iface"; then
        printf '%s unexpectedly certified %s\n' "$1" "$2" >&2
        exit 1
    fi
}

build valid
for term in worker twin wrapper; do certified valid "$term"; done
for term in wrongTail wrongHead wrongAcc wrongOwner wrongWrapper forceAcc; do absent valid "$term"; done

build no-code -fno-code -fwrite-interface
for term in worker twin wrapper; do absent no-code "$term"; done

build changed -DCHANGE_WORKER
grep -Fq 'Rule fired: drop reverse traversal' changed.rules
absent changed worker
absent changed wrapper
certified changed twin

build swapped -DSWAP_WORKER
grep -Fq 'Rule fired: replace reverse wrapper worker' swapped.rules
certified swapped worker
certified swapped twin
absent swapped wrapper
