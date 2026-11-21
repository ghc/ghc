#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -i -i. -package-env - -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider marked ordinary
cp "$src/NativeConstrainedFamilyProvider.hs" provider/
for reader in marked ordinary; do
    mode=-XExperimentalDependentTypes
    if [[ $reader == ordinary ]]; then
        mode=-XNoExperimentalDependentTypes
    fi
    (cd provider && "${ghc[@]}" -fforce-recomp "$mode" -c NativeConstrainedFamilyProvider.hs)
    cp provider/NativeConstrainedFamilyProvider.hi "$src/NativeConstrainedFamilyClient.hs" "$reader/"
    (
        cd "$reader"
        "${ghc[@]}" -c NativeConstrainedFamilyClient.hs
        "${ghc[@]}" -fforce-recomp -fdefer-type-errors -c NativeConstrainedFamilyClient.hs
        missing_premise=(-fforce-recomp -fno-code -DOMIT_DOMAIN_PREMISE -c NativeConstrainedFamilyClient.hs)
        if [[ $reader == ordinary ]]; then
            "${ghc[@]}" "${missing_premise[@]}"
        elif "${ghc[@]}" "${missing_premise[@]}" >premise.log 2>&1 ||
             ! grep -Fq '[GHC-39999]' premise.log ||
             ! grep -Fq 'Allowed a' premise.log; then
            cat premise.log
            exit 1
        fi
        cat >MissingField.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies #-}
module MissingField where
import Data.Proxy
import NativeConstrainedFamilyProvider (Collects(..))
data Unqualified c = Unqualified (Proxy (Elem c))
HS
        if [[ $reader == ordinary ]]; then
            "${ghc[@]}" -c MissingField.hs
        elif "${ghc[@]}" -c MissingField.hs >field.log 2>&1 ||
             ! grep -Fq '[GHC-39999]' field.log ||
             ! grep -Fq 'Collects c' field.log; then
            cat field.log
            exit 1
        fi
        cat >MissingGround.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module MissingGround where
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Collects(..))
missing :: Elem [Int] :~: Int
missing = Refl
HS
        for defer in -fno-defer-type-errors -fdefer-type-errors; do
            if [[ $reader == ordinary ]]; then
                "${ghc[@]}" "$defer" -c MissingGround.hs
            elif "${ghc[@]}" "$defer" -c MissingGround.hs >missing.log 2>&1 ||
                 ! grep -Fq 'MissingGround.hs:5:12: error: [GHC-39999]' missing.log ||
                 ! grep -Fq 'Allowed Int' missing.log; then
                cat missing.log
                exit 1
            fi
        done
        cat >PartialDomain.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes, PartialTypeSignatures, TypeFamilies, TypeOperators #-}
module PartialDomain where
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Collects(..))
#ifdef VALID
type Item = Bool
#else
type Item = Int
#endif
#ifdef BINDING
proof :: Elem [Item] :~: _
proof = Refl
#else
proof = (Refl :: Elem [Item] :~: _)
#endif
HS
        for form in BINDING ANNOTATION; do
            partial=(-fforce-recomp -Wno-partial-type-signatures "-D$form" -c PartialDomain.hs)
            "${ghc[@]}" "${partial[@]}" -DVALID
            for defer in -fno-defer-type-errors -fdefer-type-errors; do
                if [[ $reader == ordinary ]]; then
                    "${ghc[@]}" "${partial[@]}" "$defer"
                elif "${ghc[@]}" "${partial[@]}" "$defer" >partial.log 2>&1 ||
                     ! grep -Fq '[GHC-39999]' partial.log ||
                     ! grep -Fq 'Allowed Int' partial.log; then
                    cat partial.log
                    exit 1
                fi
            done
        done
        if [[ $reader == marked ]]; then
            cat >DeferredTerm.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies #-}
module DeferredTerm where
import NativeConstrainedFamilyProvider (Collects(..))
deferred :: Elem [Bool]
deferred = 'x'
HS
            "${ghc[@]}" -fdefer-type-errors -c DeferredTerm.hs >deferred-term.log 2>&1
            grep -Fq '[-Wdeferred-type-errors]' deferred-term.log
        fi
    )
done

# A local signature cannot cause a missing domain to be inferred for its owner.
cat >marked/MissingLocal.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module MissingLocal where
import Data.Proxy (Proxy(..))
import NativeConstrainedFamilyProvider (Collects(..))
outer (_ :: Proxy c) =
    let local :: Proxy (Elem c)
        local = Proxy
    in local `seq` ()
HS
cp marked/MissingLocal.hs ordinary/
if (cd marked && "${ghc[@]}" -c MissingLocal.hs) >missing-local.log 2>&1 ||
   ! grep -Fq '[GHC-39999]' missing-local.log ||
   ! grep -Fq 'Collects c' missing-local.log; then
    cat missing-local.log
    exit 1
fi
(cd ordinary && "${ghc[@]}" -c MissingLocal.hs)

# Coverage follows the imported declaration even when the instance writer
# has the experimental flag off.
cat >marked/MissingEquation.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies #-}
module MissingEquation where
import NativeConstrainedFamilyProvider (Collects)
instance Collects Bool
HS
cp marked/MissingEquation.hs ordinary/
if (cd marked && "${ghc[@]}" -Wno-missing-methods -c MissingEquation.hs) >missing-equation.log 2>&1 ||
   ! grep -Fq 'must provide an associated type equation or default' missing-equation.log; then
    cat missing-equation.log
    exit 1
fi
(cd ordinary && "${ghc[@]}" -Wno-missing-methods -c MissingEquation.hs)

cat >provider/MissingBinder.hs <<'HS'
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module MissingBinder where
class C a b where
    type F a
HS
(cd provider && "${ghc[@]}" -XNoExperimentalDependentTypes -c MissingBinder.hs)
if (cd provider && "${ghc[@]}" -XExperimentalDependentTypes -c MissingBinder.hs) >missing-binder.log 2>&1 ||
   ! grep -Fq 'same binders as its class' missing-binder.log; then
    cat missing-binder.log
    exit 1
fi

cat >marked/Partial.hs <<'HS'
{-# LANGUAGE Modifiers, TypeFamilies #-}
module Partial where
import Data.Kind (Type)
import GHC.Matchability (U)
import NativeConstrainedFamilyProvider (Collects(..))
data Uses (f :: Type %U -> Type) = Uses
partial :: Uses Elem
partial = Uses
HS
for mode in NoExperimentalDependentTypes ExperimentalDependentTypes; do
    if (cd marked && "${ghc[@]}" -X"$mode" -c Partial.hs) >partial.log 2>&1 ||
       ! grep -Fq 'must be fully applied' partial.log; then
        cat partial.log
        exit 1
    fi
done
