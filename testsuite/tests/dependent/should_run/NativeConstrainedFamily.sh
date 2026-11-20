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
        cat >MissingGround.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module MissingGround where
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Collects(..))
missing :: Elem [Int] :~: Int
missing = Refl
HS
        if [[ $reader == ordinary ]]; then
            "${ghc[@]}" -c MissingGround.hs
        elif "${ghc[@]}" -c MissingGround.hs >missing.log 2>&1 ||
             ! grep -Fq '[GHC-39999]' missing.log ||
             ! grep -Fq 'Allowed Int' missing.log; then
            cat missing.log
            exit 1
        fi
    )
done

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
