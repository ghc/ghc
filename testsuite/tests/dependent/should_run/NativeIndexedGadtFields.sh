#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

# Compile the provider and read its unfolding without its source in scope.
for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{PackedVecLibrary,NativeIndexedGadtFieldsProvider,NativeNonuniformGadtFieldsProvider}.hs .
    "${ghc[@]}" "-$opt" -c PackedVecLibrary.hs
    if [[ $opt = O2 ]]; then
        "${ghc[@]}" "-$opt" -ddump-stg-from-core -dsuppress-uniques -dsuppress-idinfo \
            -dppr-cols=160 -c NativeIndexedGadtFieldsProvider.hs > provider.stg
        # Keep the original equality token and the two fields only.
        awk '/PackedVecLibrary[.]VCons[[:space:]].*->/ {
                 sub(/^.*PackedVecLibrary[.]VCons[[:space:]]+/, "")
                 sub(/[[:space:]]+->.*$/, "")
                 seen = 1
                 if (split($0, binders, /[[:space:]]+/) != 3) {
                     print
                     bad = 1
                 }
             }
             END { exit (bad || !seen) }' provider.stg
        "${ghc[@]}" --show-iface NativeIndexedGadtFieldsProvider.hi > provider.iface
        grep -Fq 'indexed-constructor-case+indexed-constructor-fields' provider.iface
    else
        "${ghc[@]}" "-$opt" -c NativeIndexedGadtFieldsProvider.hs
    fi
    "${ghc[@]}" "-$opt" -c NativeNonuniformGadtFieldsProvider.hs
    if [[ $opt = O2 ]]; then
        "${ghc[@]}" --show-iface NativeNonuniformGadtFieldsProvider.hi >nonuniform.iface
        grep -Fq 'indexed-diagonal-constructor-case' nonuniform.iface
        grep -Fq 'opaque-ordinary-constructor-case' nonuniform.iface
    fi
    cp ./*.hi "$src/NativeIndexedGadtFields.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c NativeIndexedGadtFields.hs
    "${ghc[@]}" "-$opt" NativeIndexedGadtFields.o \
        ../provider/{PackedVecLibrary,NativeIndexedGadtFieldsProvider,NativeNonuniformGadtFieldsProvider}.o -o run
    test "$(./run)" = $'(True,True,False,True)\n(True,True,True,True,True,True)'

    mkdir iso
    cd iso
    cp "$src/NativeIndexedIsoOperand.hs" .
    "${ghc[@]}" "-$opt" NativeIndexedIsoOperand.hs -o run
    test "$(./run)" = True
    if "${ghc[@]}" "-$opt" -fforce-recomp -DWRONG_INDEX -c NativeIndexedIsoOperand.hs >wrong.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done

cd "$out/O2/provider"
mkdir legacy
"${ghc[@]}" -O2 -DLEGACY_ONLY -fforce-recomp -outputdir legacy -c NativeNonuniformGadtFieldsProvider.hs
"${ghc[@]}" --show-iface legacy/NativeNonuniformGadtFieldsProvider.hi >legacy.iface
grep -Fq 'retained-constructor-case-v1' legacy.iface
if grep -Fq 'opaque-ordinary-constructor-case' legacy.iface; then
    exit 1
fi

# Force readback of the legacy structural tag before and after mutation.
cd "$out/O2/provider/legacy"
mkdir reader
cp "$src/NativeLegacyGadtFieldsMutate.hs" .
"${ghc[@]}" -O0 -package ghc NativeLegacyGadtFieldsMutate.hs -o mutate
cd reader
cp ../NativeNonuniformGadtFieldsProvider.hi .
cat >LegacyReader.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module LegacyReader where
import NativeNonuniformGadtFieldsProvider

observesNext :: Swap Bool Bool -> Bool
observesNext term = case inspectSwap term of
    Stopped -> False
    Advanced -> True
HS
"${ghc[@]}" -O2 -ddump-inlinings -c LegacyReader.hs >valid.log 2>&1
grep -Fq 'Inlining done: NativeNonuniformGadtFieldsProvider.inspectSwap' valid.log
../mutate "$2" ../NativeNonuniformGadtFieldsProvider.hi NativeNonuniformGadtFieldsProvider.hi
if "${ghc[@]}" -O2 -fforce-recomp -c LegacyReader.hs >duplicate.log 2>&1; then
    exit 1
fi
grep -Fq 'Missing or unsupported constructor proof in interface alternative: Next' duplicate.log

cd "$out/O0/provider"
if "${ghc[@]}" -O0 -fforce-recomp -DWRONG_NONUNIFORM -c NativeNonuniformGadtFieldsProvider.hs >negative.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' negative.log ||
   ! grep -Fq 'PApp (parReflCase r)' negative.log; then
    cat negative.log >&2
    exit 1
fi
for wrong in WRONG_HEAD WRONG_SCRUTINEE; do
    if "${ghc[@]}" -O0 -fforce-recomp "-D$wrong" -c NativeIndexedGadtFieldsProvider.hs > negative.log 2>&1 ||
       ! grep -Eq '\[GHC-(05617|83865)\]' negative.log ||
       ! grep -Fq 'ConsSeen @' negative.log; then
        cat negative.log >&2
        exit 1
    fi
done

for wrong in WRONG_FAMILY_ENDPOINT MISSING_FAMILY_MATCH; do
    if "${ghc[@]}" -O0 -fforce-recomp "-D$wrong" -c NativeIndexedGadtFieldsProvider.hs >negative.log 2>&1 ||
       ! grep -Eq '\[GHC-(05617|83865|25897)\]' negative.log; then
        cat negative.log >&2
        exit 1
    fi
done

# Verify nested checked-call rule tracking & warm recompilation invalidation under closed-family refinement.
mkdir -p "$out/nested-rules"
cd "$out/nested-rules"
cp "$out/O0/provider/PackedVecLibrary.hi" .

cat >NestedOrigin.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module NestedOrigin
    ( NestedAlias, NestedFields, DirectChecked, WrapAlias, ConsumeAlias ) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import PackedVecLibrary (Nat(..))

type NestedAlias = Prelude.fmap Prelude.not ('Just 'True)

type family NestedChecked :: Maybe Bool where
    NestedChecked = Prelude.fmap Prelude.not ('Just 'True)

type family NestedFields (n :: Nat) :: Maybe Bool where
    NestedFields 'Z = NestedChecked
    NestedFields ('S _) = 'Nothing

type family DirectChecked (n :: Nat) :: Maybe Bool where
    DirectChecked 'Z = Prelude.fmap Prelude.not ('Just 'True)
    DirectChecked ('S _) = 'Nothing

type family WrapAlias (n :: Nat) :: Type where
    WrapAlias 'Z = Proxy ('Just 'False)
    WrapAlias ('S _) = ()

type family ConsumeAlias (m :: Maybe Bool) :: Bool where
    ConsumeAlias ('Just 'False) = 'True
    ConsumeAlias _ = 'False
HS

cat >MidNestedProof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module MidNestedProof (proveNestedFields, closedNestedProof) where
import Data.Type.Equality ((:~:) (Refl))
import NestedOrigin (NestedFields)
import PackedVecLibrary (Nat(..))

proveNestedFields :: foreach (n :: Nat) -> n :~: 'Z -> NestedFields n :~: 'Just 'False
proveNestedFields _ proof = case proof of
    Refl -> Refl

closedNestedProof :: NestedFields 'Z :~: 'Just 'False
closedNestedProof = proveNestedFields Z Refl
HS

cat >ConflictingFmapRule.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module ConflictingFmapRule (ruleMarker) where
ruleMarker :: Int
ruleMarker = 1
{-# RULES "r03/fmap-just-true" [~1] forall f. fmap f (Just True) = Nothing #-}
HS

cat >GateAliasNoExp.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module GateAliasNoExp (GateAlias) where
import NestedOrigin (ConsumeAlias, NestedAlias)
import PackedVecLibrary (Nat(..))

type family GateAlias (n :: Nat) :: Bool where
    GateAlias 'Z = ConsumeAlias NestedAlias
    GateAlias ('S _) = 'False
HS

cat >NestedRuleChecks.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NestedRuleChecks where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import GateAliasNoExp (GateAlias)
import MidNestedProof (proveNestedFields)
import NestedOrigin (DirectChecked, NestedAlias, NestedFields, WrapAlias)
import PackedVecLibrary (Nat(..))
#ifdef WITH_RULE
import ConflictingFmapRule ()
#endif

-- Congruence and non-checked branches stay valid even when fmap has a conflicting rule.
congDirect :: foreach (a :: Nat) -> foreach (b :: Nat) -> a :~: b -> DirectChecked a :~: DirectChecked b
congDirect _ _ Refl = Refl

succDirect :: foreach (a :: Nat) -> foreach (b :: Nat) -> a :~: 'S b -> DirectChecked a :~: 'Nothing
succDirect _ _ Refl = Refl

reflAlias :: NestedAlias :~: NestedAlias
reflAlias = Refl

#if defined(CHECK_NESTED_BRANCH)
badNested :: foreach (n :: Nat) -> n :~: 'Z -> NestedFields n :~: 'Just 'False
badNested _ Refl = Refl
#elif defined(CHECK_NESTED_RETAINED_TARGET)
badRetainedTarget :: foreach (n :: Nat) -> foreach (m :: Maybe Bool) ->
                     n :~: 'Z -> m :~: 'Just 'False -> NestedFields n :~: m
badRetainedTarget _ _ Refl Refl = Refl
#elif defined(CHECK_WRAP_ALIAS)
badWrapAlias :: foreach (n :: Nat) -> n :~: 'Z -> WrapAlias n :~: Proxy NestedAlias
badWrapAlias _ Refl = Refl
#elif defined(CHECK_CONSUME_ALIAS)
badConsumeAlias :: foreach (n :: Nat) -> n :~: 'Z -> GateAlias n :~: 'True
badConsumeAlias _ Refl = Refl
#elif defined(CHECK_DIRECT_BRANCH)
badDirect :: foreach (n :: Nat) -> n :~: 'Z -> DirectChecked n :~: 'Just 'False
badDirect _ Refl = Refl
#elif defined(CHECK_IMPORTED_PROOF)
badImported :: String
badImported = case proveNestedFields Z Refl of Refl -> "unexpected"
#endif
HS

cat >ImportedProofClient.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module ImportedProofClient where
import Data.Type.Equality ((:~:) (Refl))
import MidNestedProof (closedNestedProof)
#ifdef WITH_IMPORTED_RULE
import ConflictingFmapRule ()
#endif
#ifdef WITH_LOCAL_RULE
{-# RULES "r03/local-fmap-just-true" [~1] forall f. fmap f (Just True) = Nothing #-}
#endif

useImported :: String
useImported = case closedNestedProof of Refl -> "ok"
HS

"${ghc[@]}" -O0 -c NestedOrigin.hs
"${ghc[@]}" -O0 -c MidNestedProof.hs
"${ghc[@]}" -O2 -c ConflictingFmapRule.hs
"${ghc[@]}" -O0 -c GateAliasNoExp.hs

"${ghc[@]}" -O2 -fforce-recomp -DWITH_RULE -c NestedRuleChecks.hs
for check in CHECK_NESTED_BRANCH CHECK_NESTED_RETAINED_TARGET CHECK_WRAP_ALIAS \
             CHECK_CONSUME_ALIAS CHECK_DIRECT_BRANCH CHECK_IMPORTED_PROOF; do
    "${ghc[@]}" -O2 -fforce-recomp "-D$check" -c NestedRuleChecks.hs
    if "${ghc[@]}" -O2 -fforce-recomp -DWITH_RULE "-D$check" \
         -c NestedRuleChecks.hs >rule-reject.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' rule-reject.log; then
        cat rule-reject.log >&2
        exit 1
    fi
done

for opt in "-O0 -fenable-rewrite-rules" "-O2"; do
    read -ra opt_flags <<<"$opt"
    "${ghc[@]}" "${opt_flags[@]}" -fforce-recomp -c ImportedProofClient.hs
    if "${ghc[@]}" "${opt_flags[@]}" -fforce-recomp -DWITH_LOCAL_RULE \
         -c ImportedProofClient.hs >imported-reject.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' imported-reject.log; then
        cat imported-reject.log >&2
        exit 1
    fi
done
if "${ghc[@]}" -O2 -fforce-recomp -DWITH_IMPORTED_RULE \
     -c ImportedProofClient.hs >imported-reject.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' imported-reject.log; then
    cat imported-reject.log >&2
    exit 1
fi

# Warm --make -O2 recompilation invalidation for both nested method proofs and nested SourceTableIds.
mkdir -p "$out/warm-recomp"
cd "$out/warm-recomp"
cp "$src/PackedVecLibrary.hs" .
cat >StepSource.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module StepSource (stepNat) where
import PackedVecLibrary (Nat(..))

{-# NOINLINE stepNat #-}
stepNat :: Nat -> Nat
stepNat Z = S Z
stepNat (S n) = n
HS
cat >NestedProvider.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module NestedProvider (Tag(..), ChooseStep, OuterMethod) where
import Data.Kind (Type)
import PackedVecLibrary (Nat(..))
import StepSource (stepNat)

data Tag (n :: Nat) = MkTag

type family ChooseStep (n :: Nat) :: Type where
    ChooseStep 'Z = Tag (stepNat 'Z)
    ChooseStep ('S _) = ()

type family InnerMethod :: Maybe Bool where
    InnerMethod = Prelude.fmap Prelude.not ('Just 'True)

type family OuterMethod (n :: Nat) :: Maybe Bool where
    OuterMethod 'Z = InnerMethod
    OuterMethod ('S _) = 'Nothing
HS
cat >WarmRules.hs <<'HS'
module WarmRules (marker) where
marker :: Int
marker = 1
HS
cat >WarmClientMethod.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module WarmClientMethod where
import Data.Type.Equality ((:~:) (Refl))
import NestedProvider (OuterMethod)
import PackedVecLibrary (Nat(..))
import WarmRules (marker)

proofMethod :: foreach (n :: Nat) -> n :~: 'Z -> OuterMethod n :~: 'Just 'False
proofMethod _ eq = case eq of Refl -> Refl

observed :: Int
observed = marker
HS
cat >WarmClientStep.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module WarmClientStep where
import Data.Type.Equality ((:~:) (Refl))
import NestedProvider (ChooseStep, Tag(..))
import PackedVecLibrary (Nat(..))
import WarmRules (marker)

proofStep :: foreach (n :: Nat) -> n :~: 'Z -> ChooseStep n -> Tag ('S 'Z)
proofStep _ Refl x = x

observed :: Int
observed = marker
HS

if ! "${ghc[@]}" -O2 -v1 --make WarmClientMethod.hs WarmClientStep.hs >warm-init.log 2>&1; then
    cat warm-init.log >&2
    exit 1
fi
"${ghc[@]}" --show-iface WarmClientMethod.hi >warm-method.iface
grep -Fq 'homeRuleInterface WarmRules ' warm-method.iface
"${ghc[@]}" -O2 -v1 --make WarmClientMethod.hs WarmClientStep.hs >warm-noop.log 2>&1
if grep -Fq 'Compiling ' warm-noop.log; then
    cat warm-noop.log >&2
    exit 1
fi

cat >WarmRules.hs <<'HS'
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module WarmRules (marker) where
import PackedVecLibrary (Nat(..))
import StepSource (stepNat)
marker :: Int
marker = 1
{-# RULES "r03/warm-fmap" [~1] forall f. fmap f (Just True) = Nothing #-}
{-# RULES "r03/warm-step" [~1] stepNat Z = Z #-}
HS

for target in WarmClientMethod.hs WarmClientStep.hs; do
    if "${ghc[@]}" -O2 -v1 --make "$target" >warm-reject.log 2>&1 ||
       ! grep -Fq '[GHC-83605]' warm-reject.log; then
        cat warm-reject.log >&2
        exit 1
    fi
done
