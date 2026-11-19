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
cp "$src"/{TypeLambda,NativeTypeLambdaScope}.hs .
"${g[@]}" -fomit-interface-pragmas -c TypeLambda.hs
"${g[@]}" --show-iface TypeLambda.hi >provider.iface
if sed -n '/^exports:/,/^defaults:/p' provider.iface | grep -F '$lambda:'; then exit 1; fi
"${g[@]}" -fomit-interface-pragmas -c NativeTypeLambdaScope.hs
cp TypeLambda.hi NativeTypeLambdaScope.hi "$out/client/"
cd "$out/client"
cat >Ordinary.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Ordinary where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
import TypeLambda (L, Captured, Shadowed, WithError, Mixed, Held, CaseShadow, Rebuild)
import Data.Proxy (Proxy)
result :: L :~: '[Maybe Int, Either String Int, IO Int]
result = Refl
capture :: Captured Ordering :~: '[Either Ordering Bool, Either Ordering Char]
capture = Refl
shadowed :: Shadowed Bool 'True :~: '[Either Char (Proxy 'True)]
shadowed = Refl
partial :: WithError Char :~: Either String Char
partial = Refl
nested :: Levels Bool 'True :~:
    ((Proxy 'True, Bool), (Proxy 'True, Int, Ordering))
nested = Refl
lifted :: Levels Type Ordering :~:
    ((Proxy Ordering, Bool), (Proxy Ordering, Int, Ordering))
lifted = Refl
adjacent :: Adjacent Bool 'False :~: (Proxy 'False, Ordering)
adjacent = Refl
inferred :: InferredLevels 'True :~:
    ((Proxy 'True, Bool), (Proxy 'True, Int, Ordering))
inferred = Refl
usesKind :: UsesKind 'True :~: (Proxy 'True, Proxy Bool, Bool, Char)
usesKind = Refl
mappedCase :: Mixed :~: '[Int, Bool]
mappedCase = Refl
branchScope :: CaseShadow 'LT :~: '[Proxy 'True, Proxy 'LT]
branchScope = Refl
knownCase :: Held 'True :~: Int
knownCase = Refl
rebuildNothing :: Rebuild ('Nothing :: Maybe Ordering) :~: Proxy ('Nothing :: Maybe Ordering)
rebuildNothing = Refl
rebuildJust :: Rebuild ('Just 'LT) :~: Proxy ('Just 'LT)
rebuildJust = Refl
ordinaryLambda :: a -> b -> a
ordinaryLambda x = (\_ -> x)
HS
"${g[@]}" -c Ordinary.hs

cat >RigidCase.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module RigidCase where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TypeLambda (Held)
wrong :: forall b. Proxy b -> Held b :~: Int
wrong _ = Refl
HS
if "${g[@]}" -c RigidCase.hs >rigid-case.log 2>&1 ||
   ! grep -Fq 'RigidCase.hs:7:11: error: [GHC-83865]' rigid-case.log; then
    cat rigid-case.log >&2
    exit 1
fi

cat >CaseFields.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}
module CaseFields where
import Data.Proxy
type Fields (outer :: k) =
  (\tag -> case tag of { Just x -> Proxy x; Nothing -> Proxy outer }) ('Just 'True)
type Whole =
  (\tag -> case tag of { Just _ -> Proxy tag; Nothing -> Proxy tag }) ('Just 'True)
HS
"${g[@]}" -Werror=unused-type-patterns -c CaseFields.hs
sed 's/Just _ -> Proxy tag/Just field -> Proxy tag/' CaseFields.hs >Unused.hs
if "${g[@]}" -Werror=unused-type-patterns -c Unused.hs >unused.log 2>&1 ||
   ! grep -Fq 'Unused.hs:7:26: error: [GHC-40910]' unused.log; then
    cat unused.log >&2
    exit 1
fi

cat >Quoted.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeFamilies #-}
module Quoted where
import Data.Proxy
import Language.Haskell.TH
quoted :: Q [Dec]
quoted = [d|
  type QuotedCase = (\tag -> case tag of { Nothing -> Proxy tag; Just _ -> Proxy tag }) 'Nothing
  |]
HS
if "${g[@]}" -c Quoted.hs >quote.log 2>&1 ||
   ! grep -Fq 'Quoted.hs:7:30: error: [GHC-65904]' quote.log ||
   ! grep -Fq 'Type case syntax' quote.log || grep -Fq '$lambda' quote.log; then
    cat quote.log >&2
    exit 1
fi

cat >WrongScope.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module WrongScope where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
wrong :: Levels Bool 'True :~:
    ((Proxy 'True, Int), (Proxy 'True, Bool, Ordering))
wrong = Refl
HS
if "${g[@]}" -c WrongScope.hs >scope.log 2>&1 ||
   ! grep -Fq 'WrongScope.hs:8:9: error: [GHC-83865]' scope.log; then
    cat scope.log >&2
    exit 1
fi

cat >Duplicate.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Duplicate where
type Duplicate = (\a a -> a) Int Bool
HS
if "${g[@]}" -c Duplicate.hs >duplicate.log 2>&1 ||
   ! grep -Fq 'Duplicate.hs:3:20: error: [GHC-10498]' duplicate.log; then
    cat duplicate.log >&2
    exit 1
fi

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
