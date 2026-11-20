#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -fdiagnostics-color=never -ferror-spans)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

reject() {
    local file=$1 code=$2 message=$3
    shift 3
    local log=${file%.hs}.log
    if "${ghc[@]}" "$@" -c "$file" >"$log" 2>&1 ||
       ! grep -Fq "[GHC-$code]" "$log" ||
       ! grep -Fq "$message" "$log"; then
        cat "$log" >&2
        exit 1
    fi
}

mkdir "$out"/{provider,client}
for opt in -O0 -O1; do
    cd "$out/provider"
    cp "$src/PackedVecLibrary.hs" .
    interfaces=()
    if [[ $opt == -O0 ]]; then
        interfaces=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" --show-iface PackedVecLibrary.hi > provider.iface
    grep -Fq 'FinalCoreSourceTable T:appendVec' provider.iface

    # Compile the proof with only the producer interface.
    cd "$out/client"
    cp ../provider/PackedVecLibrary.hi "$src/OpenVectorAppend.hs" .
    test ! -e PackedVecLibrary.hs && test ! -e PackedVecLibrary.o
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -c OpenVectorAppend.hs
    mv OpenVectorAppend.hs ../provider/

    # The ordinary consumer reads both interfaces with its local rule policy.
    cp "$src/OrdinaryOpenVectorAppend.hs" .
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import OrdinaryOpenVectorAppend (observeAppendEmpty)
import qualified PackedVecLibrary as P

main :: IO ()
main = do
    print observeAppendEmpty
    left <- try (evaluate (P.appendVec (error "forced left" :: P.Vec Bool 'P.Z) P.VNil))
      :: IO (Either SomeException (P.Vec Bool 'P.Z))
    case left of
      Left _ -> pure ()
      Right _ -> fail "append did not inspect its left operand"
HS
    "${ghc[@]}" "$opt" -fforce-recomp -c OrdinaryOpenVectorAppend.hs Main.hs
    "${ghc[@]}" Main.o OrdinaryOpenVectorAppend.o OpenVectorAppend.o ../provider/PackedVecLibrary.o -o append-run
    [[ $(./append-run) == '("Refl",True)' ]]
done

cat >WrongTail.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module WrongTail where

import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

wrongTail :: forall (xs :: P.Vec Bool ('P.S 'P.Z)) (ys :: P.Vec Bool ('P.S 'P.Z)).
             P.appendVec 'P.VNil xs :~: ys
wrongTail = Refl
HS
reject WrongTail.hs 25897 'wrongTail = Refl' -O1

cat >OpaqueLeft.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module OpaqueLeft where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

opaqueLeft :: forall (left :: P.Vec Bool 'P.Z) (xs :: P.Vec Bool ('P.S 'P.Z)).
              Proxy left -> P.appendVec left xs :~: xs
opaqueLeft _ = Refl
HS
reject OpaqueLeft.hs 25897 'opaqueLeft _ = Refl' -O1

sed '/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}$/d' "$src/OpenVectorAppend.hs" > OpenVectorAppend.hs
reject OpenVectorAppend.hs 83605 'Use -fno-enable-rewrite-rules' -O1 -fenable-rewrite-rules

# Closed calls use the ordinary evaluator even with this certificate present.
cp ../provider/PackedVecLibrary.o .
cat >ClosedCons.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module ClosedCons where

import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

appendProof :: P.appendVec ('P.VCons 'True 'P.VNil) ('P.VCons 'False 'P.VNil)
            :~: 'P.VCons 'True ('P.VCons 'False 'P.VNil)
appendProof = Refl
HS
"${ghc[@]}" -O1 -c ClosedCons.hs

# A source change that inspects the carried vector must remove the row.
cd "$out/provider"
sed 's/appendVec VNil ys = ys/appendVec VNil ys = case ys of { VNil -> VNil; VCons hd rest -> hd `seq` VCons hd rest }/' \
    "$src/PackedVecLibrary.hs" > PackedVecLibrary.hs
"${ghc[@]}" -O1 -fforce-recomp -fwrite-source-equations -c PackedVecLibrary.hs
"${ghc[@]}" --show-iface PackedVecLibrary.hi > strict.iface
if grep -Fq 'D:T:appendVec' strict.iface; then
    printf '%s\n' 'a producer that forces the right head exported an empty-branch equation' >&2
    exit 1
fi
cd "$out/client"
cp ../provider/PackedVecLibrary.hi "$src/OpenVectorAppend.hs" .
reject OpenVectorAppend.hs 70565 'This operand contains an open or unsupported field.' -O1 -fforce-recomp
