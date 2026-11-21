#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,alias,client,no-code}
cp "$src/ConcreteGuardedList.hs" "$out/provider/"
cp "$src/ConcreteGuardedListSource.hs" "$out/alias/"
cp "$src/ConcreteGuardedListProof.hs" "$out/client/"
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
cat >"$out/client/Main.hs" <<'HS'
module Main where

import ConcreteGuardedList (Signal(..), dropBool, dropMarkers)
import ConcreteGuardedListProof (showConcreteGuardedLists)

main :: IO ()
main = do
    putStrLn showConcreteGuardedLists
    print ( take 1 (dropBool (const True) (undefined : True : undefined))
          , hasCons (dropMarkers (const False) (undefined : undefined)) )
  where
    hasCons (_ : _) = True
    hasCons [] = False
HS
expected=$'(Refl,Refl,[True],[Entry])\n([True],True)'
for level in -O0 -O2; do
    cd "$out/provider"
    "${ghc[@]}" "$level" "${stripped[@]}" -fforce-recomp -c ConcreteGuardedList.hs
    "${ghc[@]}" --show-iface ConcreteGuardedList.hi >owner.iface
    for name in dropBool dropMarkers; do
        grep -Fq "FinalCoreSourceTable T:$name" owner.iface
        grep -Fq "T:#guard:$name" owner.iface
    done
    cd "$out/alias"
    cp ../provider/ConcreteGuardedList.hi .
    "${ghc[@]}" "$level" "${stripped[@]}" -fforce-recomp -c ConcreteGuardedListSource.hs
    cd "$out/client"
    cp ../alias/*.hi .
    "${ghc[@]}" "$level" -fforce-recomp -c ConcreteGuardedListProof.hs Main.hs
    "${ghc[@]}" Main.o ConcreteGuardedListProof.o ../alias/*.o ../provider/*.o -o run
    [[ $(./run) == "$expected" ]]
done

# Even a representationally equal callback domain is the wrong nominal index.
cat >WrongCallback.hs <<'HS'
{-# LANGUAGE DataKinds, TypeFamilies #-}
module WrongCallback where

import ConcreteGuardedListSource (DropBool)

newtype WrappedBool = WrappedBool Bool

type family WrappedPredicate (x :: WrappedBool) :: Bool where
    WrappedPredicate _ = 'True

type Wrong xs = DropBool WrappedPredicate xs
HS
line=$(awk '/^type Wrong / { print NR }' WrongCallback.hs)
if "${ghc[@]}" -O0 -c WrongCallback.hs >wrong-callback.log 2>&1 ||
   ! grep -Fq "WrongCallback.hs:$line:" wrong-callback.log ||
   ! grep -Fq '[GHC-83865]' wrong-callback.log ||
   ! grep -Fq 'WrappedBool' wrong-callback.log; then
    cat wrong-callback.log >&2
    exit 1
fi

cd "$out/no-code"
sed '/^{-# OPTIONS_GHC / s/ -fobject-code//' ../provider/ConcreteGuardedList.hs >ConcreteGuardedList.hs
"${ghc[@]}" -fno-code -fwrite-interface -c ConcreteGuardedList.hs
"${ghc[@]}" --show-iface ConcreteGuardedList.hi >no-code.iface
if grep -Eq 'T:(#guard:)?(dropBool|dropMarkers)' no-code.iface; then
    cat no-code.iface >&2
    exit 1
fi

# Keep the element types fixed and add a head demand to the Boolean branch.
cd "$out/provider"
sed 's/| p x = xs/| p x = x `seq` xs/' ConcreteGuardedList.hs >Strict.hs
mv Strict.hs ConcreteGuardedList.hs
"${ghc[@]}" -O2 "${stripped[@]}" -fforce-recomp -c ConcreteGuardedList.hs
"${ghc[@]}" --show-iface ConcreteGuardedList.hi >strict.iface
grep -Fq 'FinalCoreSourceTable T:dropMarkers' strict.iface
if grep -Eq 'T:(#guard:)?dropBool' strict.iface; then
    cat strict.iface >&2
    exit 1
fi
cd "$out/alias"
cp ../provider/ConcreteGuardedList.hi .
line=$(awk '/^type DropBool p xs =/ { print NR }' ConcreteGuardedListSource.hs)
if "${ghc[@]}" -O2 -fforce-recomp -c ConcreteGuardedListSource.hs >strict.log 2>&1 ||
   ! grep -Fq "ConcreteGuardedListSource.hs:$line:" strict.log ||
   ! grep -Fq '[GHC-70565]' strict.log ||
   ! grep -Fq 'dropBool p xs' strict.log; then
    cat strict.log >&2
    exit 1
fi
