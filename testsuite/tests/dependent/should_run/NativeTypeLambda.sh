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
cp "$src"/{TypeLambda,SchemaColumns,NativeTypeLambdaScope}.hs .
"${g[@]}" -Werror=unused-type-patterns -fomit-interface-pragmas -c TypeLambda.hs
sed -e 's/^module TypeLambda$/module OpenTypeLambda/' \
    -e 's/^type family Map f xs where$/type family Map f xs/' \
    -e 's/^  Map \(.* = .*\)$/type instance Map \1/' TypeLambda.hs >OpenTypeLambda.hs
"${g[@]}" -Werror=unused-type-patterns -fomit-interface-pragmas -c OpenTypeLambda.hs
"${g[@]}" --show-iface TypeLambda.hi >provider.iface
if sed -n '/^exports:/,/^defaults:/p' provider.iface | grep -E '[$](lambda|signature-lambda):'; then exit 1; fi
"${g[@]}" -fomit-interface-pragmas -c NativeTypeLambdaScope.hs
"${g[@]}" -fomit-interface-pragmas -c SchemaColumns.hs
cat >RepeatedProvider.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module RepeatedProvider where
import Data.Kind (Type)
import GHC.Matchability (U)
type LeftCallback :: Type -> Type %U -> Type
type LeftCallback e = \a -> Either e a
type RightCallback :: Type -> Type %U -> Type
type RightCallback e = \field -> Either e field
HS
"${g[@]}" -fomit-interface-pragmas -c RepeatedProvider.hs
sed 's/, UndecidableInstances//; s/module TypeLambda/module Termination/' TypeLambda.hs >Termination.hs
if "${g[@]}" -c Termination.hs >termination.log 2>&1 ||
   ! grep -Fq 'error: [GHC-22979]' termination.log ||
   ! grep -Fq 'An argument to a type family contains a type lambda.' termination.log ||
   grep -Fq '$lambda:' termination.log; then
    cat termination.log >&2
    exit 1
fi
cp TypeLambda.hi SchemaColumns.hi OpenTypeLambda.hi NativeTypeLambdaScope.hi RepeatedProvider.hi "$out/client/"
cd "$out/client"
test ! -e TypeLambda.hs
cp "$src/TypeLambdaClient.hs" .
"${g[@]}" -fomit-interface-pragmas -c TypeLambdaClient.hs
sed -e 's/module TypeLambdaClient/module OpenTypeLambdaClient/' \
    -e 's/import TypeLambda /import OpenTypeLambda /' TypeLambdaClient.hs >OpenTypeLambdaClient.hs
"${g[@]}" -fomit-interface-pragmas -c OpenTypeLambdaClient.hs

cat >RepeatedClient.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
module RepeatedClient where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import RepeatedProvider
type Same :: (Type %U -> Type) -> (Type %U -> Type) -> Bool
type family Same x y where
    Same x x = 'True
    Same _ _ = 'False
test :: Same (LeftCallback Char) (RightCallback Char) :~: 'True
test = Refl
HS
"${g[@]}" -c RepeatedClient.hs
sed -e 's/module RepeatedClient/module WrongRepeat/' -e "s/:~: 'True/:~: 'False/" \
    RepeatedClient.hs >WrongRepeat.hs
if "${g[@]}" -c WrongRepeat.hs >repeat.log 2>&1 ||
   ! grep -Fq 'error: [GHC-83865]' repeat.log; then
    cat repeat.log >&2
    exit 1
fi

sed -e 's/module TypeLambdaClient/module ChangedLambda/' \
    -e 's/Either e field/Either field e/' TypeLambdaClient.hs >ChangedLambda.hs
if "${g[@]}" -c ChangedLambda.hs >changed.log 2>&1 ||
   ! grep -Fq 'error: [GHC-83865]' changed.log ||
   ! grep -Fq 'withErrors p fields' changed.log; then
    cat changed.log >&2
    exit 1
fi

cat >Ordinary.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Ordinary where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
import TypeLambda (Fields(..), L, Captured, Shadowed, WithError, Prefixes, ScopedRow, Mixed, Held, CaseShadow, Rebuild,
                   IndexedTag(..), IndexedCase, IndexedCaseInferred)
import Data.Proxy (Proxy(..))
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
checkedField :: CheckedField String '[Bool]
checkedField = CheckedField { checkedFields = Field (Right True) End }
indexedField :: IndexedField Bool 'True
indexedField = IndexedField (Field (Right Proxy) End)
ordinaryLambda :: a -> b -> a
ordinaryLambda x = (\_ -> x)
HS
"${g[@]}" -c Ordinary.hs

cat >RowsMain.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import Ordinary (capturedRow, independentRow, scopedRow)
import Data.Proxy (Proxy(..))
import TypeLambda (Columns, Fields(..), Optional, Prefixes, ScopedRow)
import SchemaColumns (Checked)
import TypeLambdaClient (forwardErrors)

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
    let input = Field 'x' (Field (error "unused field" :: Bool) End)
    case forwardErrors (Proxy :: Proxy String) input of
      Field x (Field _ End) -> print x
    let optional :: Fields (Columns Optional '[Int])
        optional = Field (Just 42) End
        checked :: Fields (Columns (Checked String) '[Bool])
        checked = Field (Left "missing") End
    case (optional, checked) of
      (Field x End, Field y End) -> print (x, y)
HS
"${g[@]}" -c RowsMain.hs
"${g[@]}" RowsMain.o Ordinary.o TypeLambdaClient.o ../provider/TypeLambda.o \
    ../provider/SchemaColumns.o ../provider/NativeTypeLambdaScope.o -o rows
printf '%s\n' "(Left GT,Right True,Just 'a',Just True)" "(Just 'x',Right True)" "Right 'x'" '(Just 42,Left "missing")' >rows.expected
./rows >rows.actual
cmp rows.expected rows.actual

cat >Conflict.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module Conflict where
import SchemaColumns (Checked)
import TypeLambda (Columns, Map)
type instance Columns (Checked String) fields = Map (\a -> Maybe a) fields
HS
if "${g[@]}" -c Conflict.hs >conflict.log 2>&1 ||
   ! grep -Fq 'Conflicting family instance declarations' conflict.log ||
   ! grep -Eq 'type lambda in an instance of .Columns.' conflict.log ||
   ! grep -Fq 'Defined at Conflict.hs:5:15' conflict.log ||
   grep -Fq '$callback:' conflict.log; then
    cat conflict.log >&2
    exit 1
fi

cat >WrongCallback.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module WrongCallback where
import Data.Proxy (Proxy)
import TypeLambda (Fields, Map, withErrors)
reversed :: Proxy e -> Fields xs -> Fields (Map (\a -> Either a e) xs)
reversed p xs = withErrors p xs
HS
if "${g[@]}" -c WrongCallback.hs >callback.log 2>&1 ||
   ! grep -Fq 'WrongCallback.hs:6:17: error: [GHC-83865]' callback.log ||
   ! grep -Eq "type lambda in the signature for .withErrors." callback.log ||
   ! grep -Eq "type lambda in the signature for .reversed." callback.log ||
   grep -Fq '$signature-lambda:' callback.log; then
    cat callback.log >&2
    exit 1
fi

cat >FieldCallback.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module FieldCallback where
import NativeTypeLambdaScope (CheckedField(..))
import TypeLambda (Fields, Map)
forward :: Fields (Map (\value -> Either e value) xs) -> CheckedField e xs
forward = CheckedField
HS
"${g[@]}" -c FieldCallback.hs
sed -e 's/module FieldCallback/module WrongFieldCallback/' \
    -e 's/Either e value/Either value e/' FieldCallback.hs >WrongFieldCallback.hs
if "${g[@]}" -c WrongFieldCallback.hs >wrong-field.log 2>&1 ||
   ! grep -Fq 'WrongFieldCallback.hs:6:11: error: [GHC-83865]' wrong-field.log ||
   ! grep -Eq "type lambda in a field of .CheckedField." wrong-field.log ||
   grep -Fq '$field-lambda:' wrong-field.log; then
    cat wrong-field.log >&2
    exit 1
fi

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
import Data.Proxy (Proxy(..))
outer = witness where
  witness :: Proxy (\x -> x)
  witness = Proxy
HS
if "${g[@]}" -c Signature.hs >context.log 2>&1 ||
   ! grep -Eq '^Signature[.]hs:5:21: error: \[GHC-27185\]' context.log ||
   ! grep -Fq 'top-level value signature' context.log; then
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
