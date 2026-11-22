#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
cp "$area"/Native*.hs .
cp "$area/OpenMapWorker.hs" .
plain=("$1" "-B$2" -v0 -package-env - -i -i. -fdiagnostics-color=never)
g=("${plain[@]}" -dcore-lint -dstg-lint)
reject() {
  local file=$1 span=$2 message=$3 status=0
  "${g[@]}" -c "$file.hs" >bad.log 2>&1 || status=$?
  if [[ $status != 1 ]] || ! grep -Fq "$file.hs:$span:" bad.log ||
      ! grep -Fq "$message" bad.log; then cat bad.log >&2; exit 1; fi
}
"${g[@]}" -fomit-interface-pragmas -c OpenMapWorker.hs
"${g[@]}" -c NativeCase.hs
"${g[@]}" -c NativeTypeCaseFallback.hs
"${g[@]}" -fomit-interface-pragmas -c NativeNestedCase.hs
"${g[@]}" --show-iface NativeNestedCase.hi >nested.iface
if sed -n '/^exports:/,/^defaults:/p' nested.iface | grep -E '\$(case(-node)?|field-case|lambda):'; then exit 1; fi
"${g[@]}" --show-iface NativeCase.hi >case.iface
if sed -n '/^exports:/,/^defaults:/p' case.iface | grep -E '\$case(:|-let-rhs:)'; then exit 1; fi
mkdir interfaces
cp NativeCase.hi NativeNestedCase.hi OpenMapWorker.hi NativeTypeCaseFallback.hi NativeTypeCase{Client,Rigid,LetWrong,FallbackRigid}.hs interfaces/
(cd interfaces && reject NativeTypeCaseFallbackRigid 9:13 'GHC-83865' && grep -Fq 'Force b' bad.log)
(cd interfaces && "${g[@]}" -c NativeTypeCaseClient.hs &&
   reject NativeTypeCaseRigid 10:13 'case a of {True -> Int; False -> Int}' &&
   grep -Fq "Just x1 -> '(x, x1)" bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:16:12: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:19:10: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:22:13: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:27:14: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:30:19: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:33:18: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:36:19: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:39:16: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:42:21: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:45:21: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:48:17: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:52:22: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:55:31: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:59:23: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:63:21: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:67:22: error: [GHC-83865]' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:73:21: error: [GHC-83865]' bad.log &&
   grep -Eq 'the type case in .*ForallResult' bad.log &&
   grep -Eq 'the type case in .*IndexedFieldOrder' bad.log &&
   grep -Fq 'blockedTransport _ = Refl' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:81:21: error: [GHC-83865]' bad.log &&
   grep -Eq 'the type case in .*RefinedBranchPick' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:85:24: error: [GHC-83865]' bad.log &&
   grep -Eq 'the type case in .*NestedLetCase' bad.log &&
   grep -Fq 'NativeTypeCaseRigid.hs:89:24: error: [GHC-83865]' bad.log &&
   grep -Eq 'the type case in .*ArrowDomain' bad.log &&
   ! grep -Eq '\$(case(-node)?|lambda):' bad.log &&
   reject NativeTypeCaseLetWrong 8:9 'GHC-83865')
"${g[@]}" -fomit-interface-pragmas -c NativeTypeCaseHeadProvider.hs
cp NativeTypeCaseHeadProvider.hi NativeTypeCaseHeads.hs NativeTypeCaseSynonymCatchAll.hs interfaces/
(cd interfaces && "${g[@]}" -Werror=unused-imports -c NativeTypeCaseHeads.hs)
reject NativeTypeCaseFamilyHead 4:24 'GHC-73138'
grep -Fq 'Illegal type synonym family application' bad.log
(cd interfaces && reject NativeTypeCaseSynonymCatchAll 8:39 'GHC-55789'
grep -Fq 'After expanding type synonyms, the pattern has no constructor head' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:9:39: error: [GHC-55789]' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:10:35: error: [GHC-55789]' bad.log
grep -Fq 'NativeTypeCaseSynonymCatchAll.hs:11:41: error: [GHC-55789]' bad.log)
reject NativeTypeCaseInvalid 6:3 'Not in scope: data constructor'
reject NativeTypeCaseEscape 6:14 'Not in scope: type variable'
reject NativeTypeCaseTupleKindEscape 9:14 'Not in scope: type variable'
grep -Fq 'missing' bad.log
reject NativeTypeCaseTupleKindRhsEscape 7:41 'Not in scope: type variable'
grep -Fq 'ghost' bad.log
reject NativeTypeCaseForwardKind 7:23 'Not in scope: type variable'
grep -Fq 'payload' bad.log
reject NativeTypeCaseBadPattern 5:3 'Expected a constructor pattern, a variable, or a wildcard'
grep -Fq '[GHC-55789]' bad.log
reject NativeTypeCaseDisabled 4:12 'GHC-98931'
reject NativeTypeCaseBranchKind 6:12 "In the type case in"
grep -Fq 'NativeTypeCaseBranchKind.hs:10:12: error: [GHC-83865]' bad.log
grep -Fq "In the type case in" bad.log
grep -Fq "BadArrow" bad.log
grep -Fq 'NativeTypeCaseBranchKind.hs:14:12: error: [GHC-83865]' bad.log
grep -Fq "BadForall" bad.log
grep -Fq 'NativeTypeCaseBranchKind.hs:18:12: error: [GHC-83865]' bad.log
grep -Eq 'the type case in .*BadArrowDomain' bad.log
grep -Fq 'NativeTypeCaseBranchKind.hs:21:25: error: [GHC-83865]' bad.log
grep -Eq 'the type case in .*BadAscription' bad.log
reject NativeTypeCaseForallResult 4:60 '[GHC-97233]'
reject NativeTypeCaseSignatureDistinct 9:26 '[GHC-83865]'
grep -Fq 'NativeTypeCaseSignatureDistinct.hs:15:25: error: [GHC-83865]' bad.log
grep -Fq 'NativeTypeCaseSignatureDistinct.hs:20:31: error: [GHC-83865]' bad.log
grep -Fq 'the type case in the signature for' bad.log
! grep -Fq '$signature-case:' bad.log
reject NativeTypeCaseScrutinee 4:15 'Not in scope: type constructor or class'
reject NativeTypeCaseLetMonomorphic 6:22 'GHC-83865'
reject NativeTypeCaseLetUnowned 4:26 'A type let used in a case body cannot generalize the kind of its right-hand side.'
grep -Fq 'NativeTypeCaseLetUnowned.hs:9:15: error:' bad.log
test "$(grep -Fc 'cannot generalize the kind of its right-hand side' bad.log)" = 2

reject NativeTypeCaseDominatedRhs 6:20 'GHC-76037'
grep -Fq 'Not in scope: type constructor or class' bad.log
reject NativeTypeCaseDominatedWrongKind 8:20 'GHC-83865'
grep -Eq 'has kind .*Bool' bad.log
reject NativeTypeCaseNestedAliasHead 5:8 'GHC-55789'
reject NativeTypeCaseOpaqueIndex 14:9 'GHC-83865'
grep -Fq 'NativeTypeCaseOpaqueIndex.hs:27:9: error: [GHC-83865]' bad.log
grep -Fq 'NativeTypeCaseOpaqueIndex.hs:40:3: error: [GHC-79642]' bad.log
test "$(grep -c 'error: \[GHC-79642\]' bad.log)" = 1
grep -Fq 'Cannot determine which constructors are possible' bad.log
reject NativeTypeCaseErasedAlias 8:3 'GHC-30337'
grep -Fq 'appear in an injective position' bad.log
reject NativeTypeCaseErasedWildcard 8:3 'GHC-30337'
grep -Fq 'appear in an injective position' bad.log
reject NativeTypeCaseHiddenAlias 11:11 "GHC-83865"
grep -Fq "In the expression: Refl" bad.log
reject NativeTypeCaseHiddenErased 6:3 'GHC-30337'

# A stuck family still blocks refinement without Core Lint.
g=("${plain[@]}")
reject NativeTypeCaseOpaqueIndex 40:3 'GHC-79642'
test "$(grep -c 'error: \[GHC-79642\]' bad.log)" = 1
! grep -Fq 'Core Lint errors' bad.log


# Read the constructor field from its interface with the extension off.
(
    cd interfaces
    cat >NativeTypeCaseFieldRigid.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, GADTs, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module NativeTypeCaseFieldRigid where
import Data.Proxy (Proxy(..))
import NativeNestedCase

rigid :: CaseField ('Just bit) -> Int
rigid (CaseField n) = n

shadow :: ShadowedCaseField 'True ('Just 'False) -> Proxy 'True
shadow (ShadowedCaseField bit) = bit

ranked :: forall (bit :: Bool). Proxy bit -> OrderedRankedCaseField 'True -> Int
ranked _ (OrderedRankedCaseField f) = f @('Just bit) Proxy

rankedShadow :: RankedCaseField 'False -> Char
rankedShadow (RankedCaseField f) = f @'True Proxy

badCapture :: CapturedKindCaseField Bool 'True 'False -> Proxy Int
badCapture (CapturedKindCaseField f) = f @Int @'True Proxy
HS
    reject NativeTypeCaseFieldRigid 7:23 '[GHC-83865]'
    grep -Eq 'the type case in a field of .*CaseField' bad.log
    grep -Fq 'NativeTypeCaseFieldRigid.hs:10:34: error: [GHC-83865]' bad.log
    grep -Fq 'NativeTypeCaseFieldRigid.hs:13:39: error: [GHC-83865]' bad.log
    grep -Fq 'NativeTypeCaseFieldRigid.hs:16:36: error: [GHC-83865]' bad.log
    grep -Fq 'NativeTypeCaseFieldRigid.hs:19:40: error: [GHC-83865]' bad.log
    ! grep -Fq '$field-case:' bad.log
)

cat >NativeTypeCaseFieldEscape.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module NativeTypeCaseFieldEscape where
import Data.Proxy (Proxy)
data Escape (d :: Maybe Bool) where
  Escape :: forall (d :: Maybe Bool).
    (case d of { Just bit -> Proxy bit; Nothing -> Proxy bit }) -> Escape d
HS
reject NativeTypeCaseFieldEscape 6:6 '[GHC-97233]'

# Keep both outer kind arguments even though the forall shadows them.
cat >NativeTypeCaseFieldCompoundKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes #-}
module NativeTypeCaseFieldCompoundKind where
import Data.Kind (Type)
import Data.Proxy (Proxy)

data Tag k l = Tag

data Compound (k :: Type) (l :: Type) (a :: Tag k l) = Compound
  (forall (k :: Type) (l :: Type) (b :: Bool). Proxy b ->
    (case b of { True -> Proxy a; False -> Proxy '(k, l) }))
HS
"${g[@]}" -fomit-interface-pragmas -c NativeTypeCaseFieldCompoundKind.hs
cp NativeTypeCaseFieldCompoundKind.hi interfaces/
(
    cd interfaces
    cat >CompoundKindClient.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module CompoundKindClient where
import Data.Proxy (Proxy(..))
import NativeTypeCaseFieldCompoundKind

outer :: forall k l (a :: Tag k l). Compound k l a -> Proxy a
outer (Compound f) = f @Int @Double @'True Proxy

inner :: forall k l (a :: Tag k l). Compound k l a -> Proxy '(Int, Double)
inner (Compound f) = f @Int @Double @'False Proxy
HS
    "${g[@]}" -c CompoundKindClient.hs
    cat >CompoundKindWrong.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, PolyKinds, RankNTypes, TypeApplications #-}
module CompoundKindWrong where
import Data.Proxy (Proxy(..))
import NativeTypeCaseFieldCompoundKind

swapped :: forall k l. Compound k l ('Tag :: Tag k l) -> Proxy ('Tag :: Tag l k)
swapped (Compound f) = f @Int @Double @'True Proxy
HS
    reject CompoundKindWrong 7:24 '[GHC-25897]'
)

# A family application in a capture kind can still require a signature.
cat >NativeTypeCaseFieldFamilyKind.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, RankNTypes, TypeFamilies #-}
module NativeTypeCaseFieldFamilyKind where
import Data.Kind (Type)
import Data.Proxy (Proxy)
type family PayloadKind (k :: Type) (l :: Type) :: Type
data Compound (k :: Type) (l :: Type) (a :: PayloadKind k l) = Compound
  (forall (k :: Type) (l :: Type) (b :: Bool). Proxy b ->
    (case b of { True -> Proxy a; False -> Proxy '(k, l) }))
HS
reject NativeTypeCaseFieldFamilyKind 6:40 '[GHC-83865]'
grep -Fq 'Cannot check the kind of' bad.log
grep -Fq 'in the type case in a field of' bad.log
grep -Fq 'non-injective type family' bad.log
grep -Fq 'Use a named type synonym with a standalone kind signature for the case.' bad.log
if grep -Eq '\$field-(kind|case):' bad.log; then cat bad.log >&2; exit 1; fi

cat >LocalComputedField.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase #-}
module LocalComputedField where
import Prelude hiding (not)

data Field where
  Field :: (foreach (b :: Bool) -> (case not b of { True -> Int; False -> Char })) -> Field

value :: Field
value = Field (\case True -> 41; False -> 'f')

{-# NOINLINE not #-}
not :: Bool -> Bool
not True = True
not False = False
#ifdef CHANGE_HEAD
{-# RULES "change local head" forall x. not x = False #-}
#endif
HS
"${g[@]}" -O2 -c LocalComputedField.hs
if "${g[@]}" -O2 -fforce-recomp -DCHANGE_HEAD -c LocalComputedField.hs >bad.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' bad.log || ! grep -Fq 'rewrite rule' bad.log; then
    cat bad.log >&2
    exit 1
fi

# Keep the written Bool domain and the runtime arguments in the interface.
cat >NativeTypeCaseRetainedField.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, RankNTypes #-}
module NativeTypeCaseRetainedField where

data RetainedField where
  RetainedField :: (foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })) -> RetainedField

selected :: RetainedField
selected = RetainedField (\case True -> 11; False -> 'x')

data ClosedField where
  ClosedField :: (foreach (b :: Bool) -> (case True of { True -> Int; False -> Char })) -> ClosedField

constant :: ClosedField
constant = ClosedField (\b -> if b then 11 else 23)

data StoredField where
  StoredField :: foreach (b :: Bool) -> (case b of { True -> Int; False -> Int }) -> StoredField

stored :: StoredField
stored = StoredField False 23
HS
"${g[@]}" -fomit-interface-pragmas -c NativeTypeCaseRetainedField.hs
"${g[@]}" --show-iface NativeTypeCaseRetainedField.hi >retained-field.iface
grep -Fq 'foreach (b :: GHC.Internal.Types.Bool)' retained-field.iface
cp NativeTypeCaseRetainedField.hi interfaces/
(
    cd interfaces
    cat >NativeTypeCaseRetainedFieldClient.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, LinearTypes, NoExperimentalDependentTypes, TypeApplications #-}
module Main where
import Control.Exception (ErrorCall(..), evaluate, try)
import Control.Monad (forM_)
import Data.IORef
import Data.Proxy (Proxy(..))
import GHC.Types (Multiplicity(Many, One))
import NativeTypeCaseRetainedField
import qualified NativeNestedCase as Field

negated :: Bool -> String
negated b = case Field.negatedCaseField of
  Field.NegatedCaseField f -> case b of
    True -> [f b]
    False -> show (f b :: Int)

pick :: Bool -> RetainedField -> String
pick b (RetainedField f) = case b of
  True -> show (f b :: Int)
  False -> [f b]

guarded :: Bool -> Bool -> RetainedField -> String
guarded b enabled (RetainedField f) = case b of
  True | enabled -> show (f True :: Int)
  True -> "fallthrough"
  False -> [f False]

sameType :: a -> a -> String
sameType _ _ = "same"

cachedResult :: Bool -> RetainedField -> String
cachedResult b (RetainedField f) =
  let cached = f b
  in case b of
    True -> sameType cached (f b)
    False -> "unused"

closed :: Bool -> ClosedField -> Int
closed b (ClosedField f) = f b

readStored :: StoredField -> Int
readStored (StoredField True n) = n
readStored (StoredField False n) = n

ignoreOn :: Field.ArrowDomain 'LT ('Just ('Just 'True))
ignoreOn _ = 17

ignoreOff :: Field.ArrowDomain 'GT ('Just ('Just 'False))
ignoreOff _ = 29

linearInput :: Field.LinearArrowDomain 'True
linearInput x = x

polyInput :: Field.MultiplicityArrowDomain 'True
polyInput x = x

useOne :: Proxy 'One %1 -> Proxy 'One
useOne x = polyInput @'One x

main :: IO ()
main = do
  input <- newIORef False
  forM_ [False, True] $ \b -> do
    writeIORef input b
    value <- readIORef input
    print (pick value selected, closed value constant, negated value)
  print (readStored stored)
  print (guarded True False selected, cachedResult True selected)
  print (ignoreOn (error "unused on"), ignoreOff (error "unused off"))
  print (linearInput 31, useOne Proxy, polyInput @'Many Proxy)
  result <- try (evaluate (pick (error "strict scrutinee") selected)) :: IO (Either ErrorCall String)
  print (either (\(ErrorCall message) -> message == "strict scrutinee") (const False) result)
HS
    "${g[@]}" -c NativeTypeCaseRetainedFieldClient.hs
    "${g[@]}" NativeTypeCaseRetainedFieldClient.o ../NativeTypeCaseRetainedField.o ../NativeNestedCase.o -o run-retained-field
    ./run-retained-field >retained-field.out
    printf '("x",23,"41")\n("11",11,"t")\n23\n("fallthrough","same")\n(17,29)\n(31,Proxy,Proxy)\nTrue\n' | diff -u - retained-field.out

    cat >BadArrowUse.hs <<'HS'
{-# LANGUAGE DataKinds, LinearTypes, NoExperimentalDependentTypes #-}
module BadArrowUse where
import Data.Proxy (Proxy(..))
import qualified NativeNestedCase as Field

dropInput :: Field.LinearArrowDomain 'True
dropInput _ = 0

duplicateInput :: Field.LinearArrowDomain 'True
duplicateInput x = x + x

dropPolyInput :: Field.MultiplicityArrowDomain 'True
dropPolyInput _ = Proxy
HS
    reject BadArrowUse 7:11 '[GHC-18872]'
    grep -Fq 'BadArrowUse.hs:10:16: error: [GHC-18872]' bad.log
    grep -Fq 'BadArrowUse.hs:13:15: error: [GHC-25897]' bad.log
    grep -Fq 'rigid type variable bound by' bad.log

    cat >BadRetainedField.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module BadRetainedField where
import NativeTypeCaseRetainedField

wrong :: RetainedField -> Char
wrong (RetainedField f) = f True

neutral :: Bool -> RetainedField -> Int
neutral b (RetainedField f) = f b

unrelated :: Bool -> Bool -> RetainedField -> String
unrelated b other (RetainedField f) = case b of
  True -> show (f other :: Int)
  False -> [f b]

guarded :: Bool -> Bool -> RetainedField -> String
guarded b enabled (RetainedField f) = case b of
  True | enabled -> show (f b :: Int)
  True -> "fallthrough"
  False -> [f False]
HS
    reject BadRetainedField 6:27 '[GHC-83865]'
    grep -Fq 'BadRetainedField.hs:9:31: error: [GHC-83865]' bad.log
    grep -Fq 'BadRetainedField.hs:13:17: error: [GHC-05617]' bad.log
    grep -Fq 'BadRetainedField.hs:18:27: error: [GHC-83865]' bad.log
    grep -Fq 'the type case in a field of' bad.log
)

# A speculative branch gets one family step. Checking a written use
# of the same constructor still reports the solver's reduction limit.
cat >NativeTypeCaseRecursiveIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, UndecidableInstances #-}
module NativeTypeCaseRecursiveIndex where

import Data.Kind (Type)

type family Loop (tag :: Bool) :: Type where
  Loop 'True = Loop 'True
  Loop 'False = Ordering

data Gate k (tag :: Bool) where
  Known :: Bool -> Gate Bool 'True
  Recursive :: Gate (Loop tag) tag

type Pick (d :: Maybe (Gate Bool 'True)) = case d of
  Just (Known True) -> Int
  Just _ -> Bool
  Nothing -> ()
HS
g=("${plain[@]}" -dcore-lint -dstg-lint -freduction-depth=8)
reject NativeTypeCaseRecursiveIndex 15:3 'GHC-79642'
sed 's/Just (Known True) -> Int/Just Recursive -> Int/' NativeTypeCaseRecursiveIndex.hs >recursive.hs
mv recursive.hs NativeTypeCaseRecursiveIndex.hs
reject NativeTypeCaseRecursiveIndex 15:8 'GHC-40404'
grep -Fq 'Reduction stack overflow; size = 9' bad.log

# An optimized constructor unfolding produces a join with a retained index.
mkdir -p retained-join/client
(
    cd retained-join
    flags=("-B$2" -O2 -v0 -i -i. -package-env - -dcore-lint -dstg-lint)
    cp "$area/NativeRetainedJoinProvider.hs" .
    "$1" "${flags[@]}" -c NativeRetainedJoinProvider.hs
    cd client
    cp ../NativeRetainedJoinProvider.hi "$area/NativeRetainedJoin.hs" .
    "$1" "${flags[@]}" -c NativeRetainedJoin.hs
    "$1" "-B$2" -v0 NativeRetainedJoin.o ../NativeRetainedJoinProvider.o -o run
    test "$(./run)" = '("37","d")'
)
