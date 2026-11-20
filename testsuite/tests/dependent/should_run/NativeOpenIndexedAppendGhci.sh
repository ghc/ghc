#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -Werror=inaccessible-code)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)
interactive=(--interactive -ignore-dot-ghci -fbyte-code -fbreak-points -fwrite-interface)

mkdir "$out"/{correct,strict,counting}
cd "$out/correct"
cp "$src/"{PackedVecLibrary,OpenVectorAppend,OrdinaryOpenVectorAppend}.hs .
nil_row=$(awk '/^appendVec VNil ys = ys$/ {print NR}' PackedVecLibrary.hs)
test -n "$nil_row"
if ! {
    printf 'print observeAppendEmpty\n:module + PackedVecLibrary\n'
    printf ':break PackedVecLibrary %s\n' "$nil_row"
    printf 'print (appendContents (VCons True VNil))\n:continue\n:quit\n'
} | "${ghc[@]}" "${interactive[@]}" "${strip[@]}" -i -i. -outputdir . \
          '*OrdinaryOpenVectorAppend.hs' >interactive.log 2>&1; then
    cat interactive.log >&2
    exit 1
fi
if ! grep -Fxq '("Refl",True)' interactive.log ||
   ! grep -Fq 'Stopped in PackedVecLibrary.appendVec' interactive.log; then
    cat interactive.log >&2
    exit 1
fi
"${ghc[@]}" --show-iface PackedVecLibrary.hi >provider.iface
grep -Fq 'FinalCoreSourceTable T:appendVec' provider.iface

cd "$out/strict"
cp "$src/OpenVectorAppend.hs" .
sed 's/appendVec VNil ys = ys/appendVec VNil ys = case ys of { VNil -> VNil; VCons hd rest -> hd `seq` VCons hd rest }/' \
    "$src/PackedVecLibrary.hs" >PackedVecLibrary.hs
printf ':quit\n' | "${ghc[@]}" "${interactive[@]}" "${strip[@]}" -i -i. -outputdir . \
    '*OpenVectorAppend.hs' >strict.log 2>&1
if ! grep -Fq '[GHC-70565]' strict.log; then
    cat strict.log >&2
    exit 1
fi
"${ghc[@]}" --show-iface PackedVecLibrary.hi >strict.iface
if grep -Fq 'FinalCoreSourceTable T:appendVec' strict.iface; then
    echo 'A breakpoint around a strict branch exported an equation' >&2
    exit 1
fi

cd "$out/counting"
cat >CountingAppend.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
module CountingAppend (Nat(..), Vec(..), appendVec) where

data Nat = Z | S Nat

type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add 'Z m = m
    Add ('S n) m = 'S (Add n m)

data Vec a (n :: Nat) where
    VNil :: Vec a 'Z
    VCons :: a -> Vec a n -> Vec a ('S n)

appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)
HS
"${ghc[@]}" "${strip[@]}" -i -i. -c CountingAppend.hs
"${ghc[@]}" --show-iface CountingAppend.hi >plain.iface
grep -Fq 'FinalCoreSourceTable T:appendVec' plain.iface
"${ghc[@]}" "${strip[@]}" -fforce-recomp -fhpc -hpcdir . -i -i. -c CountingAppend.hs
"${ghc[@]}" --show-iface CountingAppend.hi >counting.iface
if grep -Fq 'FinalCoreSourceTable T:appendVec' counting.iface; then
    echo 'A counting tick qualified as a debugger breakpoint' >&2
    exit 1
fi
