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
"${g[@]}" -Werror=unused-type-patterns -fomit-interface-pragmas -c TypeLambda.hs
"${g[@]}" --show-iface TypeLambda.hi >provider.iface
if sed -n '/^exports:/,/^defaults:/p' provider.iface | grep -F '$lambda:'; then exit 1; fi
"${g[@]}" -fomit-interface-pragmas -c NativeTypeLambdaScope.hs
sed 's/, UndecidableInstances//; s/module TypeLambda/module Termination/' TypeLambda.hs >Termination.hs
if "${g[@]}" -c Termination.hs >termination.log 2>&1 ||
   ! grep -Fq 'error: [GHC-22979]' termination.log ||
   ! grep -Fq 'An argument to a type family contains a type lambda.' termination.log ||
   grep -Fq '$lambda:' termination.log; then
    cat termination.log >&2
    exit 1
fi
cp TypeLambda.hi NativeTypeLambdaScope.hi "$out/client/"
cd "$out/client"
cat >Ordinary.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Ordinary where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
import TypeLambda (L, Captured, Shadowed, WithError, Prefixes, ScopedRow, Mixed, Held, CaseShadow, Rebuild,
                   IndexedTag(..), IndexedCase, IndexedCaseInferred)
import Data.Proxy (Proxy)
result :: L :~: '[Maybe Int, Either String Int, IO Int]
result = Refl
capture :: Captured Ordering :~: '[Either Ordering Bool, Either Ordering Char]
capture = Refl
shadowed :: Shadowed Bool 'True :~: '[Either Char (Proxy 'True)]
shadowed = Refl
partial :: WithError Char :~: Either String Char
partial = Refl
capturedRow :: Prefixes ('Just Ordering) '[Char, Bool] :~: '[Either Ordering Char, Either Ordering Bool]
capturedRow = Refl
independentRow :: Prefixes 'Nothing '[Char, Bool] :~: '[Maybe Char, Maybe Bool]
independentRow = Refl
scopedRow :: ScopedRow Char '[Bool] :~: '[(Maybe Char, Either Char Bool)]
scopedRow = Refl
scopedKind :: LetKind Ordering 'GT :~: (Maybe Ordering, Proxy 'GT, Char)
scopedKind = Refl
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
indexedBool :: IndexedCase @Bool ('BooleanTag 'False) :~: Proxy 'False
indexedBool = Refl
indexedOrdering :: IndexedCase @Ordering ('OrderingTag 'GT) :~: Proxy 'GT
indexedOrdering = Refl
indexedInferred :: IndexedCaseInferred ('OrderingTag 'LT) :~: Proxy 'LT
indexedInferred = Refl
selected :: Selected 'False :~: '[Ordering]
selected = Refl
siblingsTrue :: Siblings 'True :~: '(Bool, Char)
siblingsTrue = Refl
siblingsFalse :: Siblings 'False :~: '(Ordering, Int)
siblingsFalse = Refl
ordinaryCases :: OrdinaryCases 'True :~: ([Bool], Maybe Char)
ordinaryCases = Refl
consed :: Consed 'False :~: '[Ordering, Int]
consed = Refl
ordinaryLambda :: a -> b -> a
ordinaryLambda x = (\_ -> x)
HS
"${g[@]}" -c Ordinary.hs

cat >RowsMain.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import Ordinary (capturedRow, independentRow, scopedRow)
import TypeLambda (Prefixes, ScopedRow)

data Fields xs where
  End :: Fields '[]
  Field :: a -> Fields xs -> Fields (a ': xs)

withError :: Fields (Prefixes ('Just Ordering) '[Char, Bool])
withError = case capturedRow of Refl -> Field (Left GT) (Field (Right True) End)

withoutError :: Fields (Prefixes 'Nothing '[Char, Bool])
withoutError = case independentRow of Refl -> Field (Just 'a') (Field (Just True) End)

main :: IO ()
main = case (capturedRow, independentRow, withError, withoutError) of
  (Refl, Refl, Field a (Field b End), Field c (Field d End)) -> do
    print (a, b, c, d)
    let row :: Fields (ScopedRow Char '[Bool])
        row = case scopedRow of Refl -> Field (Just 'x', Right True) End
    case row of Field value End -> print value
HS
"${g[@]}" -c RowsMain.hs
"${g[@]}" RowsMain.o Ordinary.o -o rows
printf '%s\n' "(Left GT,Right True,Just 'a',Just True)" "(Just 'x',Right True)" >rows.expected
./rows >rows.actual
cmp rows.expected rows.actual

cat >LaterLet.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module LaterLet where
type Bad e = let wrap = (\a -> Either later a) in let later = Maybe e in wrap Char
HS
if "${g[@]}" -c LaterLet.hs >later.log 2>&1; then exit 1; fi
grep -Fq 'GHC-76037' later.log
grep -Eq "Not in scope: type variable .later." later.log

cat >RowScope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module RowScope where
import Data.Kind (Type)
type Scope :: Maybe Type -> Type
type family Scope tag where
  Scope ('Just sibling) = (\argument -> Either sibling argument) Int
  Scope 'Nothing = (\argument -> Either sibling argument) Char
HS
if "${g[@]}" -c RowScope.hs >row-scope.log 2>&1 ||
   ! grep -Fq 'RowScope.hs:7:41: error: [GHC-76037]' row-scope.log ||
   ! grep -Fq 'Not in scope: type variable' row-scope.log ||
   grep -Fq '$lambda:' row-scope.log; then
    cat row-scope.log >&2
    exit 1
fi

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

cat >ClientAlias.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module ClientAlias where
import Data.Proxy
import Data.Type.Equality
import TypeLambda (Rebuild)
type family Opaque (t :: Bool) :: Maybe Ordering
type instance Opaque 'True = 'Just 'GT
type Named t = Rebuild t
named :: forall t. Proxy t -> Named (Opaque t) :~: Proxy (Opaque t)
named _ = Refl
distinct :: forall (p :: Maybe Ordering) (q :: Maybe Ordering).
            Proxy p -> Proxy q -> Rebuild p :~: Rebuild q
distinct _ _ = Refl
HS
if "${g[@]}" -c ClientAlias.hs >alias.log 2>&1 ||
   ! grep -Fq 'ClientAlias.hs:10:11: error: [GHC-83865]' alias.log ||
   ! grep -Fq 'Actual: Named (Opaque t) :~: Named (Opaque t)' alias.log ||
   ! grep -Fq 'ClientAlias.hs:13:16: error: [GHC-83865]' alias.log ||
   ! grep -Fq 'Expected: Rebuild p :~: Rebuild q' alias.log ||
   ! grep -Fq 'GHC cannot determine the arguments of the type lambda' alias.log ||
   grep -Fq '$lambda:' alias.log; then
    cat alias.log >&2
    exit 1
fi

cat >NestedMismatch.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module NestedMismatch where
import Data.Proxy
import Data.Type.Equality
import NativeTypeLambdaScope (SelectedFirst)
wrong :: forall b. Proxy b -> SelectedFirst b Char :~: Maybe (Either Int Bool)
wrong _ = Refl
HS
if "${g[@]}" -c NestedMismatch.hs >nested.log 2>&1 ||
   ! grep -Fq 'NestedMismatch.hs:7:11: error: [GHC-83865]' nested.log ||
   ! grep -Fq 'case b of {True -> Either Char Bool;' nested.log ||
   ! grep -Fq 'False -> Either Bool Char}' nested.log ||
   grep -Fq '$lambda:' nested.log; then
    cat nested.log >&2
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

cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module WrongIndex where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TypeLambda (IndexedCase, IndexedTag(..))
wrong :: IndexedCase @Bool ('OrderingTag 'LT) :~: Proxy 'LT
wrong = Refl
HS
if "${g[@]}" -c WrongIndex.hs >index.log 2>&1 ||
   ! grep -Fq 'WrongIndex.hs:6:29: error: [GHC-83865]' index.log; then
    cat index.log >&2
    exit 1
fi

cat >WrongVisibility.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module WrongVisibility where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TypeLambda (IndexedCaseInferred, IndexedTag(..))
wrong :: IndexedCaseInferred @Bool ('BooleanTag 'True) :~: Proxy 'True
wrong = Refl
HS
if "${g[@]}" -c WrongVisibility.hs >visibility.log 2>&1 ||
   ! grep -Fq 'WrongVisibility.hs:6:10: error: [GHC-20967]' visibility.log; then
    cat visibility.log >&2
    exit 1
fi

cat >SiblingScope.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, DataKinds, TypeFamilies #-}
module SiblingScope where
import Data.Proxy
type Bad (xs :: Maybe Bool) =
  '( (\x -> case x of { Just field -> Proxy field; Nothing -> Proxy 'False }) xs
   , (\y -> Proxy field) Ordering )
HS
if "${g[@]}" -c SiblingScope.hs >sibling.log 2>&1 ||
   ! grep -Fq 'SiblingScope.hs:6:19: error: [GHC-76037]' sibling.log ||
   ! grep -Fq 'Not in scope: type variable' sibling.log; then
    cat sibling.log >&2
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
   ! grep -Eq '^Disabled[.]hs:3:13: error: \[GHC-27184\]' disabled.log ||
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
   ! grep -Eq '^Signature[.]hs:4:19: error: \[GHC-27185\]' context.log ||
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
