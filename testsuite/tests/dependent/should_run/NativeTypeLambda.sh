#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
   -fdiagnostics-color=never)

cd "$out/provider"
cp "$src/TypeLambda.hs" .
"${g[@]}" -c TypeLambda.hs
"${g[@]}" --show-iface TypeLambda.hi >provider.iface
if sed -n '/^exports:/,/^defaults:/p' provider.iface | grep -F '$lambda:'; then exit 1; fi
cp TypeLambda.hi "$out/client/"
cd "$out/client"
cat >Ordinary.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Ordinary where
import Data.Type.Equality ((:~:)(Refl))
import TypeLambda (L, Captured, Shadowed)
import Data.Proxy (Proxy)
result :: L :~: '[Maybe Int, Either String Int, IO Int]
result = Refl
capture :: Captured Ordering :~: '[Either Ordering Bool, Either Ordering Char]
capture = Refl
shadowed :: Shadowed Bool 'True :~: '[Either Char (Proxy 'True)]
shadowed = Refl
HS
"${g[@]}" -c Ordinary.hs

cat >Disabled.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Disabled where
type Bad = (\x -> x)
HS
if "${g[@]}" -c Disabled.hs >disabled.log 2>&1 ||
   ! grep -Eq '^Disabled[.]hs:3:12: error: \[GHC-27184\]' disabled.log ||
   ! grep -Fq 'Type lambdas require -XExperimentalDependentTypes' disabled.log; then
    cat disabled.log >&2
    exit 1
fi

cat >Signature.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Signature where
import Data.Proxy (Proxy)
witness :: Proxy (\x -> x)
witness = undefined
HS
if "${g[@]}" -c Signature.hs >context.log 2>&1 ||
   ! grep -Eq '^Signature[.]hs:4:18: error: \[GHC-27185\]' context.log ||
   ! grep -Fq 'top-level synonym' context.log; then
    cat context.log >&2
    exit 1
fi

cat >Matchable.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module Matchable where
import Data.Kind (Type)
import GHC.Exts (M)
type Bad = ((\x -> x) :: Type %M -> Type)
HS
if "${g[@]}" -c Matchable.hs >matchable.log 2>&1 ||
   ! grep -Eq '^Matchable[.]hs:5:6: error: \[GHC-83865\]' matchable.log ||
   ! grep -Fq 'GHC.Internal.Types.Unmatchable' matchable.log ||
   ! grep -Fq 'GHC.Internal.Types.Matchable' matchable.log; then
    cat matchable.log >&2
    exit 1
fi

# Changing the hidden family equation must invalidate the public alias's client.
cp Ordinary.hs "$out/provider"
cd "$out/provider"
"${g[@]}" --make -no-link Ordinary.hs
"${g[@]}" -v1 --make -no-link Ordinary.hs >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
sed 's/ -> Either a b)/ -> Either b a)/' TypeLambda.hs >changed.hs
mv changed.hs TypeLambda.hs
if "${g[@]}" -v1 --make -no-link Ordinary.hs >changed.log 2>&1 ||
   ! grep -Fq 'Compiling Ordinary' changed.log ||
   ! grep -Eq "^Ordinary[.]hs:$(awk '/^capture = Refl$/ {print NR}' Ordinary.hs):[0-9]+: error: " changed.log; then
    cat changed.log >&2
    exit 1
fi
