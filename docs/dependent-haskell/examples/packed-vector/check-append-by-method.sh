#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
ghc=$1
libdir=$2
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client,upgrade}
    cd "$out/$opt/provider"
    flags=("-B$libdir" -v0 -package-env - "$opt" -i -i. -dcore-lint -dstg-lint)
    if [[ $opt == -O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    cp "$src/"{PackedVecLibrary,RetainedSumOperator}.hs .
    "$ghc" "${flags[@]}" -c PackedVecLibrary.hs RetainedSumOperator.hs
    cp PackedVecLibrary.hi RetainedSumOperator.hi "$src/AppendByMethodClient.hs" ../client/
    cd ../client
    cat > Main.hs <<'HS'
module Main where
import AppendByMethodClient (appendTwoAndOneByMethod)
main :: IO ()
main = print appendTwoAndOneByMethod
HS
    "$ghc" "${flags[@]}" -c AppendByMethodClient.hs Main.hs
    "$ghc" "-B$libdir" -v0 Main.o AppendByMethodClient.o \
        ../provider/PackedVecLibrary.o ../provider/RetainedSumOperator.o -o run
    ./run | tee actual
    printf '%s\n' '("aab",'"'a'"',"")' | cmp - actual

    sed 's/Length (2 N.+ 1)/Length (2 N.+ 2)/' AppendByMethodClient.hs > WrongLength.hs
    sed 's/headVec joined/headVec empty/' AppendByMethodClient.hs > EmptyHead.hs
    if "$ghc" "${flags[@]}" -c WrongLength.hs > wrong.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' wrong.log
    grep -Fq 'appendByMethod 2 1 two one' wrong.log
    if "$ghc" "${flags[@]}" -c EmptyHead.hs > empty.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' empty.log
    grep -Fq 'headVec empty' empty.log

    cat > PeanoEvidence.hs <<'HS'
{-# LANGUAGE DataKinds, FlexibleContexts, NoExperimentalDependentTypes, TypeApplications #-}
module PeanoEvidence where
import GHC.TypeNats.Experimental (FromNatural)
import PackedVecLibrary (Nat, Vec, replicateNative)

ordinary :: Nat
ordinary = fromInteger @Nat 2

ordinaryGiven :: Num Nat => Nat
ordinaryGiven = fromInteger @Nat 2

bare :: Vec Char (FromNatural Nat 2)
bare = replicateNative 2 'p'

explicit :: Vec Char (FromNatural Nat 2)
explicit = replicateNative (fromInteger @Nat 2) 'p'
HS
    "$ghc" "${flags[@]}" -c PeanoEvidence.hs
    sed 's/ :: Vec Char/ :: Num Nat => Vec Char/' PeanoEvidence.hs > PeanoGiven.hs
    if "$ghc" "${flags[@]}" -c PeanoGiven.hs > given.log 2>&1; then exit 1; fi
    test "$(grep -Fc 'This call needs a known' given.log)" = 2
    grep -Fq "replicateNative 2 'p'" given.log
    grep -Fq "replicateNative (fromInteger @Nat 2) 'p'" given.log

    # The imported type operator returns its first index. Num.(+) does not.
    cat > TypePlus.hs <<'HS'
{-# LANGUAGE ExplicitNamespaces, TypeOperators #-}
module TypePlus (type (+)) where
import PackedVecLibrary (Nat)
type (x :: Nat) + (y :: Nat) = x
HS
    cat > TypePlusClient.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ExplicitNamespaces, TypeOperators #-}
module TypePlusClient where
import PackedVecLibrary (Nat, Vec)
import Prelude ((+))
import TypePlus (type (+))
first :: foreach (n :: Nat) -> foreach (m :: Nat)
      -> Vec a n -> Vec a m -> Vec a (n + m)
first _ _ xs _ = xs
HS
    "$ghc" "${flags[@]}" -c TypePlus.hs TypePlusClient.hs
    sed '/^import TypePlus /d' TypePlusClient.hs > MethodInsteadOfType.hs
    if "$ghc" "${flags[@]}" -c MethodInsteadOfType.hs > precedence.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' precedence.log

    cd ../upgrade
    cp ../provider/PackedVecLibrary.hs .
    # This producer remains well typed when the method changes.
    cat > MethodLength.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}
module MethodLength (methodLength) where
import PackedVecLibrary (Nat, Vec, replicateNative)
import Prelude (Bool(True), (+))
methodLength :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
methodLength n m = replicateNative (n + m) True
HS
    cat > StaleLength.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module StaleLength where
import qualified GHC.TypeNats as N
import GHC.TypeNats.Experimental (FromNatural)
import MethodLength (methodLength)
import PackedVecLibrary (Nat, Vec)
three :: Vec Bool (FromNatural Nat (2 N.+ 1))
three = methodLength 2 1
HS
    "$ghc" "${flags[@]}" --make StaleLength.hs
    sed -i.bak 's/^    (+) = plus$/    (+) = times/' PackedVecLibrary.hs
    # Recheck unchanged clients without forcing recompilation.
    if "$ghc" "${flags[@]}" --make StaleLength.hs > upgrade.log 2>&1; then exit 1; fi
    grep -Fq 'StaleLength.hs:' upgrade.log
    grep -Fq '[GHC-83865]' upgrade.log
    grep -Fq 'methodLength 2 1' upgrade.log
    sed 's/(2 N.+ 1)/2/' StaleLength.hs > UpdatedLength.hs
    "$ghc" "${flags[@]}" -c UpdatedLength.hs
done
