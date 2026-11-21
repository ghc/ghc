#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v1 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@" --interactive -ignore-dot-ghci -fexternal-interpreter -fbyte-code
     -fomit-interface-pragmas -fno-write-if-simplified-core)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/{NativeOpenListFoldrProducer,ListFoldrSource,ListFoldrProof,PreludeFoldrProof}.hs .
cp "$src"/{PreludeMapLengthSize,PreludeMapLengthSource,PreludeMapLengthProof,PreludeAppendLengthSource}.hs .
cat >UserFoldSource.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module UserFoldSource (FoldDirect, FoldLocal) where
import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F

type FoldDirect :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type FoldDirect k z xs = F.direct k z xs

type FoldLocal :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type FoldLocal k z xs = F.local k z xs
HS
cat >UserFoldProof.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module UserFoldProof where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import UserFoldSource (FoldDirect, FoldLocal)

nil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
       Proxy k -> Proxy z -> FoldDirect k z '[] :~: z
nil _ _ = Refl

direct :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
          Proxy k -> Proxy z -> Proxy x -> Proxy xs
       -> FoldDirect k z (x ': xs) :~: k x (FoldDirect k z xs)
direct _ _ _ _ = Refl

local :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
         Proxy k -> Proxy z -> Proxy x -> Proxy xs
      -> FoldLocal k z (x ': xs) :~: k x (FoldLocal k z xs)
local _ _ _ _ = Refl
HS
"${ghc[@]}" -ddump-simpl -ddump-to-file '*UserFoldProof.hs' '*ListFoldrProof.hs' '*PreludeFoldrProof.hs' >lazy.log 2>&1 <<'GHCi' || { cat lazy.log >&2; exit 1; }
:module + PreludeFoldrProof NativeOpenListFoldrProducer
:{
print ( PreludeFoldrProof.example
      , NativeOpenListFoldrProducer.direct undefined 's' ([] :: [Char])
      , NativeOpenListFoldrProducer.local (\x _ -> x) undefined ('h' : undefined) )
:}
GHCi
if ! grep -Fq "(Refl,'s','h')" lazy.log || grep -Fq 'error:' lazy.log; then
    cat lazy.log >&2
    exit 1
fi
producer=NativeOpenListFoldrProducer
grep -Fq "break<$producer," "$producer.dump-simpl"
for term in direct local; do
    grep -Fq "FinalCoreSourceTable $producer.T:$term" "$producer.dump-simpl"
done
for term in forceStep forceSeed; do
    sed "s/F.direct/F.$term/" UserFoldSource.hs >StrictFold.hs
    "${ghc[@]}" '*StrictFold.hs' </dev/null >strict.log 2>&1 || true
    if ! grep -Fq 'StrictFold.hs:7:' strict.log ||
       ! grep -Fq '[GHC-70565]' strict.log ||
       ! grep -Fq "F.$term k z xs" strict.log; then
        cat strict.log >&2
        exit 1
    fi
done
[[ -z $(find . -name '*.o' -print -quit) ]]
