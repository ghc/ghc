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
for opt in -O0 -O1 -O2; do
    cd "$out/provider"
    cp "$src/PackedVecLibrary.hs" .
    interfaces=()
    if [[ $opt == -O0 ]]; then
        interfaces=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" --show-iface PackedVecLibrary.hi > provider.iface
    grep -Fq 'FinalCoreSourceTable T:appendVec' provider.iface
    grep -Fq "T:appendVec ('VCons" provider.iface

    # Compile the proof with only the producer interface.
    cd "$out/client"
    cp ../provider/PackedVecLibrary.hi "$src/OpenVectorAppend.hs" .
    test ! -e PackedVecLibrary.hs && test ! -e PackedVecLibrary.o
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -c OpenVectorAppend.hs
    mv OpenVectorAppend.hs ../provider/

    # The ordinary consumer reads both interfaces with its local rule policy.
    cp "$src/OrdinaryOpenVectorAppend.hs" .
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, void)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import OpenVectorAppend (appendEmptyAt)
import OrdinaryOpenVectorAppend (appendContents, observeAppendCons, observeAppendEmpty)
import qualified PackedVecLibrary as P

raises :: a -> IO Bool
raises value = do
    result <- try (void (evaluate value)) :: IO (Either SomeException ())
    pure (case result of Left _ -> True; Right _ -> False)

main :: IO ()
main = do
    xs <- newIORef (P.VCons True (P.VCons False P.VNil)) >>= readIORef
    let proofOnly :: P.Vec Bool n -> Bool
        proofOnly v = case appendEmptyAt v of Refl -> True
    print (observeAppendEmpty, appendContents xs, proofOnly (error "forced proof input" :: P.Vec Bool 'P.Z))
    putStrLn observeAppendCons
    let bottom = error "forced vector" :: P.Vec Bool 'P.Z
    left <- raises (P.appendVec bottom P.VNil)
    unless left (fail "append did not inspect its left operand")
    case P.appendVec (P.VCons (error "forced head") bottom) bottom of
      P.VCons headValue tailValue -> do
        headDemand <- raises headValue
        leftTailDemand <- raises tailValue
        unless (headDemand && leftTailDemand) (fail "append changed left field demand")
    case P.appendVec (P.VCons True P.VNil) bottom of
      P.VCons True tailValue -> do
        rightDemand <- raises tailValue
        unless rightDemand (fail "append changed right field demand")
      P.VCons False _ -> fail "append changed the head"
HS
    "${ghc[@]}" "$opt" -fforce-recomp -c OrdinaryOpenVectorAppend.hs Main.hs
    "${ghc[@]}" Main.o OrdinaryOpenVectorAppend.o OpenVectorAppend.o ../provider/PackedVecLibrary.o -o append-run
    [[ $(./append-run) == $'(("Refl",True),[True,False],True)\nRefl' ]]
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

cat >WrongRuntime.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module WrongRuntime where
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P
wrong :: foreach (xs :: P.Vec Bool 'P.Z) -> foreach (ys :: P.Vec Bool 'P.Z)
      -> P.appendVec 'P.VNil xs :~: ys
wrong xs ys = Refl
HS
reject WrongRuntime.hs 83865 'wrong xs ys = Refl' -O1

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

cat >AnnotatedOperands.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module AnnotatedOperands where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

annotatedEmpty :: forall (n :: P.Nat) (xs :: P.Vec Bool n).
                  P.appendVec ('P.VNil :: P.Vec Bool 'P.Z) xs :~: xs
annotatedEmpty = Refl

annotatedCons :: forall (n :: P.Nat) (m :: P.Nat) (x :: Bool)
                        (xs :: P.Vec Bool n) (ys :: P.Vec Bool m).
                 Proxy xs -> Proxy ys ->
                 P.appendVec ('P.VCons x xs :: P.Vec Bool ('P.S n)) (ys :: P.Vec Bool m)
                   :~: 'P.VCons x (P.appendVec xs ys)
annotatedCons _ _ = Refl
HS
"${ghc[@]}" -O1 -c AnnotatedOperands.hs

cat >BadIndexAnnotation.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module BadIndexAnnotation where

import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

wrongIndex :: forall (n :: P.Nat) (xs :: P.Vec Bool n).
              P.appendVec ('P.VNil :: P.Vec Bool ('P.S 'P.Z)) xs :~: xs
wrongIndex = Refl
HS
reject BadIndexAnnotation.hs 83865 "'P.VNil" -O1

cat >WrongCons.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, PolyKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module WrongCons where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

wrongHead :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a) (x' :: a)
                     (xs :: P.Vec a n) (ys :: P.Vec a m).
              Proxy xs -> Proxy ys ->
              P.appendVec ('P.VCons x xs) ys :~: 'P.VCons x' (P.appendVec xs ys)
wrongHead _ _ = Refl

wrongCarried :: forall a (n :: P.Nat) (m :: P.Nat) (x :: a)
                        (xs :: P.Vec a n) (ys :: P.Vec a m) (zs :: P.Vec a m).
                 Proxy xs -> Proxy ys -> Proxy zs ->
                 P.appendVec ('P.VCons x xs) ys :~: 'P.VCons x (P.appendVec xs zs)
wrongCarried _ _ _ = Refl
HS
reject WrongCons.hs 25897 'wrongHead _ _ = Refl' -O1
grep -Fq 'wrongCarried _ _ _ = Refl' WrongCons.log

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

# Preserve the empty equation if the cons branch forces a field or
# changes the recursive call's carried operand.
for mutant in force-head change-carried; do
    cd "$out/provider"
    case "$mutant" in
      force-head)
        replacement='appendVec (VCons x xs) ys = x `seq` VCons x (appendVec xs ys)' ;;
      change-carried)
        replacement='appendVec (VCons x xs) ys = VCons x (appendVec xs (error "changed right operand" `asTypeOf` ys))' ;;
    esac
    sed "s/^appendVec (VCons x xs) ys = .*/$replacement/" "$src/PackedVecLibrary.hs" > PackedVecLibrary.hs
    "${ghc[@]}" -O1 -fforce-recomp -fwrite-source-equations -c PackedVecLibrary.hs
    "${ghc[@]}" --show-iface PackedVecLibrary.hi > "$mutant.iface"
    grep -Fq 'FinalCoreSourceTable T:appendVec' "$mutant.iface"
    if grep -Fq "T:appendVec ('VCons" "$mutant.iface"; then
        printf '%s unexpectedly exported a cons equation\n' "$mutant" >&2
        exit 1
    fi
    cd "$out/client"
    cp ../provider/PackedVecLibrary.hi .
    sed -e '/^-- | Appending a cons/,$d' -e 's/OpenVectorAppend (appendEmpty, appendCons)/EmptyOnly (appendEmpty)/'         "$src/OpenVectorAppend.hs" > EmptyOnly.hs
    "${ghc[@]}" -O1 -fforce-recomp -c EmptyOnly.hs
    reject OpenVectorAppend.hs 70565 'This operand contains an open or unsupported field.' -O1 -fforce-recomp
done
