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

import Control.Exception (ErrorCall(..), SomeException, evaluate, try)
import Control.Monad (unless, void)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import OpenVectorAppend (appendConsAt, appendEmptyAt, appendEmptyAtBoth)
import OrdinaryOpenVectorAppend (appendContents, observeAppendCons, observeAppendEmpty, observeAppendPair)
import qualified PackedVecLibrary as P

raises :: a -> IO Bool
raises value = do
    result <- try (void (evaluate value)) :: IO (Either SomeException ())
    pure (case result of Left _ -> True; Right _ -> False)

pairAtZero :: P.Vec Bool 'P.Z -> P.Vec Bool 'P.Z -> Bool
pairAtZero xs ys = case appendEmptyAtBoth P.Z xs ys of
    (Refl, Refl) -> True

pairAtOne :: P.Vec Bool ('P.S 'P.Z) -> P.Vec Bool ('P.S 'P.Z) -> Bool
pairAtOne xs ys = case appendEmptyAtBoth (P.S P.Z) xs ys of
    (Refl, Refl) -> True

pairAtTwo :: P.Vec Bool ('P.S ('P.S 'P.Z)) -> P.Vec Bool ('P.S ('P.S 'P.Z)) -> Bool
pairAtTwo xs ys = case appendEmptyAtBoth (P.S (P.S P.Z)) xs ys of
    (Refl, Refl) -> True

main :: IO ()
main = do
    xs <- newIORef (P.VCons True (P.VCons False P.VNil)) >>= readIORef
    let proofOnly :: P.Vec Bool n -> Bool
        proofOnly v = case appendEmptyAt v of Refl -> True
    print (observeAppendEmpty, appendContents xs, proofOnly (error "forced proof input" :: P.Vec Bool 'P.Z))
    putStrLn observeAppendCons
    let bottom = error "forced vector" :: P.Vec Bool 'P.Z
        bottomOne = error "forced vector" :: P.Vec Bool ('P.S 'P.Z)
        bottomTwo = error "forced vector" :: P.Vec Bool ('P.S ('P.S 'P.Z))
    zero <- newIORef P.Z >>= readIORef
    one <- newIORef (P.S (error "forced predecessor")) >>= readIORef
    cut <- try (evaluate (observeAppendPair (error "forced cut")))
    let demandedCut = case cut of
            Left (ErrorCall "forced cut") -> True
            _ -> False
    print (pairAtZero bottom bottom, pairAtOne bottomOne bottomOne, pairAtTwo bottomTwo bottomTwo,
           observeAppendPair zero, observeAppendPair one, demandedCut)
    let consProof :: a -> P.Vec a n -> P.Vec a m -> Bool
        consProof hd tl ys = case appendConsAt hd tl ys of Refl -> True
    unless (consProof (error "forced proof head") bottom bottom) (fail "missing recursive proof")
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
    [[ $(./append-run) == $'(("Refl",True),[True,False],True)\nRefl\n(True,True,True,True,True,True)' ]]
done

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/"{PackedVecLibrary,OpenVectorAppend,OrdinaryOpenVectorAppend}.hs .
cons_row=$(awk '/^appendVec \(VCons x xs\) ys =/ {print NR}' PackedVecLibrary.hs)
test -n "$cons_row"
if ! {
    printf 'print (observeAppendEmpty, observeAppendCons)\n'
    printf ':break PackedVecLibrary %s\n' "$cons_row"
    printf 'print (consContents False P.VNil (P.VCons True P.VNil))\n:continue\n:quit\n'
} | "${ghc[@]}" --interactive -ignore-dot-ghci -O0 -fbyte-code -fbreak-points \
          -fwrite-interface -fomit-interface-pragmas -fno-write-if-simplified-core \
          '*OrdinaryOpenVectorAppend.hs' >interactive.log 2>&1; then
    cat interactive.log >&2
    exit 1
fi
if ! grep -Fxq '(("Refl",True),"Refl")' interactive.log ||
   ! grep -Fq 'Stopped in PackedVecLibrary.appendVec' interactive.log ||
   ! grep -Fxq '[False,True]' interactive.log; then
    cat interactive.log >&2
    exit 1
fi
"${ghc[@]}" --show-iface PackedVecLibrary.hi >provider.iface
grep -Fq "T:appendVec ('VCons" provider.iface

cd "$out/client"
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

sed -e 's/^module OpenVectorAppend .*/module WrongAppendPair where/' \
    -e "s/appendEmptyAt @a @'P.Z ys)/appendEmptyAt @a @'P.Z xs)/" \
    "$src/OpenVectorAppend.hs" >WrongAppendPair.hs
reject WrongAppendPair.hs 05617 "In the expression: appendEmptyAt @a @'P.Z xs" -O1

cat >AmbiguousRetainedWidth.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module AmbiguousRetainedWidth where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P

type EmptyProof a (n :: P.Nat) (xs :: P.Vec a n) = P.appendVec 'P.VNil xs :~: xs

ambiguous :: forall m a. foreach (cut :: P.Nat) -> foreach (xs :: P.Vec a (P.Add cut m))
          -> EmptyProof a (P.Add cut m) xs
ambiguous _ _ = Refl
HS
reject AmbiguousRetainedWidth.hs 83865 'GHC cannot determine the arguments of the function' -O1
grep -Fq 'In the ambiguity check for' AmbiguousRetainedWidth.log

sed -e 's/^module AmbiguousRetainedWidth/module DeterminedRetainedWidth/' \
    -e 's/^ambiguous/determined/' \
    -e 's/-> EmptyProof a (P.Add cut m) xs/-> (Proxy m, EmptyProof a (P.Add cut m) xs)/' \
    -e 's/= Refl$/= (Proxy, Refl)/' \
    AmbiguousRetainedWidth.hs >DeterminedRetainedWidth.hs
cat >>DeterminedRetainedWidth.hs <<'HS'

nested :: forall a. (forall n. foreach (xs :: P.Vec a n) -> EmptyProof a n xs) -> Int
nested _ = 0
HS
"${ghc[@]}" -O1 -c DeterminedRetainedWidth.hs

cat >WrongConsAt.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module WrongConsAt where
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P
wrong :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (xs :: P.Vec Bool n)
      -> foreach (ys :: P.Vec Bool m)
      -> P.appendVec ('P.VCons x xs) ys :~: 'P.VCons y (P.appendVec xs ys)
wrong x y xs ys = Refl
HS
reject WrongConsAt.hs 83865 'wrong x y xs ys = Refl' -O1

cat >AnnotatedField.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module AnnotatedField where
import Data.Type.Equality ((:~:)(Refl))
import qualified PackedVecLibrary as P
proof :: forall n. foreach (xs :: P.Vec Bool n)
      -> P.appendVec ('P.VCons 'True (xs :: P.Vec Bool n)) 'P.VNil
         :~: 'P.VCons 'True (P.appendVec xs 'P.VNil)
proof _ = Refl
HS
"${ghc[@]}" -O1 -c AnnotatedField.hs

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
reject OpenVectorAppend.hs 70565 'This argument is a type variable.' -O1 -fforce-recomp

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
    sed -e '/^-- | Appending a cons/,$d' -e 's/^module OpenVectorAppend .*/module EmptyOnly (appendEmpty, appendEmptyAt) where/' \
        "$src/OpenVectorAppend.hs" > EmptyOnly.hs
    "${ghc[@]}" -O1 -fforce-recomp -c EmptyOnly.hs
    reject OpenVectorAppend.hs 70565 'This operand contains an open or unsupported field.' -O1 -fforce-recomp
done
