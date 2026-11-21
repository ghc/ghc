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
    cp provider/NativeConstrainedFamilyProvider.hi "$src/"{NativeConstrainedFamilyClient,NativePartialFamilyScope,NativeImprovedFamilyScope,NativeFamilyEquationScope}.hs "$reader/"
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
        equation=(-fforce-recomp -XExperimentalDependentTypes -c NativeFamilyEquationScope.hs)
        "${ghc[@]}" "${equation[@]}"
        for site in WRONG_OWNER WRITER_ONLY WRONG_ARGUMENT REPRESENTATIONAL MISSING_PREMISE; do
            case "$site" in
                WRONG_OWNER) predicate='Output c Bool' ;;
                WRITER_ONLY) predicate='Collects (Key' ;;
                WRONG_ARGUMENT) predicate='Collects b' ;;
                REPRESENTATIONAL) predicate='Collects a' ;;
                MISSING_PREMISE) predicate='Allowed a' ;;
            esac
            if [[ $reader == ordinary ]]; then
                "${ghc[@]}" "${equation[@]}" "-D$site"
            elif "${ghc[@]}" "${equation[@]}" "-D$site" -fdefer-type-errors >equation.log 2>&1 ||
                 ! grep -Eq '^NativeFamilyEquationScope.hs:[0-9]+:[0-9]+: error: \[GHC-90647\]' equation.log ||
                 ! grep -Fq "$predicate" equation.log; then
                cat equation.log
                exit 1
            fi
        done
        improved=(-fforce-recomp -c NativeImprovedFamilyScope.hs)
        "${ghc[@]}" "${improved[@]}"
        for site in ALIAS_GUARD MISSING_OWNER WRONG_EXPLICIT EXPLICIT_PREFIX; do
            case "$site" in
                ALIAS_GUARD) message='Allowed Int' ;;
                MISSING_OWNER) message='Output c' ;;
                WRONG_EXPLICIT) message='Output (Key Bool) Char' ;;
                EXPLICIT_PREFIX) message='to determine the instance of' ;;
            esac
            if [[ $reader == ordinary && $site != EXPLICIT_PREFIX ]]; then
                "${ghc[@]}" "${improved[@]}" "-D$site"
            else
                code=39999
                if [[ $site == EXPLICIT_PREFIX ]]; then
                    code=90647
                    if [[ $reader == ordinary ]]; then code=27346; message='partial type family application'; fi
                fi
                if "${ghc[@]}" "${improved[@]}" "-D$site" -fdefer-type-errors >owner.log 2>&1 ||
                   ! grep -Fq "[GHC-$code]" owner.log || ! grep -Fq "$message" owner.log; then
                    cat owner.log
                    exit 1
                fi
            fi
        done
        scoped=(-fforce-recomp -Wno-partial-type-signatures -c NativePartialFamilyScope.hs)
        "${ghc[@]}" "${scoped[@]}"
        for site in MISSING_BINDING MISSING_ANNOTATION CONTEXT_WILDCARD SIBLING STATIC SPLICE_ESCAPE OTHER_INDEX INFERRED_ESCAPE; do
            predicate='Collects c'
            case "$site" in
                MISSING_*) predicate='Allowed a' ;;
                OTHER_INDEX) predicate='Collects other' ;;
                SPLICE_ESCAPE) predicate='Collects Int' ;;
            esac
            if [[ $reader == ordinary ]]; then
                "${ghc[@]}" "${scoped[@]}" "-D$site"
            elif "${ghc[@]}" "${scoped[@]}" "-D$site" -fdefer-type-errors >scope.log 2>&1 ||
                 ! grep -Eq '^NativePartialFamilyScope.hs:[0-9]+:[0-9]+: error: \[GHC-39999\]' scope.log ||
                 ! grep -Fq "$predicate" scope.log; then
                cat scope.log
                exit 1
            fi
        done
        cat >AliasUse.hs <<'HS'
{-# LANGUAGE CPP, FlexibleInstances, NoExperimentalDependentTypes, TypeFamilies #-}
module AliasUse where
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Key, KeyElement)
instance {-# OVERLAPPING #-} Allowed a => Collects (Key [a]) where
    type Elem (Key [a]) = Bool
#ifdef MISSING
value :: (KeyElement [Bool], KeyElement [Int])
#else
value :: (KeyElement [Bool], KeyElement [Bool])
#endif
value = (True, True)
HS
        "${ghc[@]}" -c AliasUse.hs
        if [[ $reader == ordinary ]]; then
            "${ghc[@]}" -fforce-recomp -DMISSING -c AliasUse.hs
        elif "${ghc[@]}" -fforce-recomp -DMISSING -fdefer-type-errors -c AliasUse.hs >alias.log 2>&1 ||
             ! grep -Fq '[GHC-39999]' alias.log || ! grep -Fq 'Allowed Int' alias.log; then
            cat alias.log
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
{-# LANGUAGE CPP, ExplicitNamespaces, NoExperimentalDependentTypes, PartialTypeSignatures #-}
{-# LANGUAGE RequiredTypeArguments, TypeApplications, TypeFamilies, TypeOperators #-}
module PartialDomain where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Collects(..))
#ifdef VALID
type Item = Bool
#else
type Item = Int
#endif
#if defined(VISIBLE)
proof = Proxy @(Elem [Item]) `seq` ()
#elif defined(REQUIRED)
takeType :: forall a -> Proxy a
takeType (type a) = Proxy
proof = takeType (type (Elem [Item])) `seq` ()
#elif defined(BINDING)
proof :: Elem [Item] :~: _
proof = Refl
#else
proof = (Refl :: Elem [Item] :~: _)
#endif
HS
        for form in BINDING ANNOTATION VISIBLE REQUIRED; do
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
   ! grep -Fq 'Missing class parameters in the family declaration' missing-binder.log ||
   ! grep -Eq 'Missing class parameters in the family declaration.*\bb\b' missing-binder.log; then
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
       ! grep -Fq 'Supply ‘c’ to determine the instance of ‘Collects’.' partial.log; then
        cat partial.log
        exit 1
    fi
done
