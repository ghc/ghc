#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -i. -fomit-interface-pragmas
     -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client

cat >provider/ReifyProvider.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, NoLiberalTypeSynonyms, NoPolyKinds #-}
module ReifyProvider (Apply) where

type Apply f a = f a
HS
(cd provider && "${ghc[@]}" -c ReifyProvider.hs)
cp provider/ReifyProvider.{hi,o} client/
cd client

cat >ReifyClient.hs <<'HS'
{-# LANGUAGE CPP, NoExperimentalDependentTypes, NoLiberalTypeSynonyms #-}
{-# LANGUAGE GADTs, PolyKinds, StandaloneKindSignatures, TemplateHaskell, TypeOperators #-}
#ifdef EXPLICIT
{-# LANGUAGE TypeApplications #-}
#endif
module ReifyClient (Copied) where

import Data.Type.Equality ((:~:)(Refl))
#ifdef EXPLICIT
import GHC.Matchability (M)
#endif
import Language.Haskell.TH
import ReifyProvider (Apply)

$(do TyConI (TySynD _ binders rhs) <- reify ''Apply
     let copied = mkName "Copied"
#ifdef PLAIN
     pure [TySynD copied binders rhs]
#else
     kind <- reifyType ''Apply
     let stripKind (KindedTV name visibility _) = PlainTV name visibility
         stripKind binder = binder
     pure [KiSigD copied kind, TySynD copied (map stripKind binders) rhs]
#endif
 )

#ifdef EXPLICIT
check :: Copied @M Maybe Bool :~: Maybe Bool
#else
check :: Copied Maybe Bool :~: Maybe Bool
#endif
check = Refl
HS

"${ghc[@]}" -c ReifyClient.hs
cat >ReifyFamilyClient.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module ReifyFamilyClient where

import Data.Type.Equality ((:~:)(Refl))
import ReifyClient (Copied)

type family Identity a where
    Identity a = a

check :: Copied Identity Bool :~: Bool
check = Refl
HS
"${ghc[@]}" -c ReifyFamilyClient.hs

# The standalone kind signature preserves the inferred binder.
if "${ghc[@]}" -fforce-recomp -DEXPLICIT -c ReifyClient.hs >explicit.log 2>&1 ||
   ! grep -Fq '[GHC-20967]' explicit.log || ! grep -Fq 'Copied @M Maybe Bool' explicit.log; then
    cat explicit.log >&2
    exit 1
fi

# A plain TySynD rebinds the omitted index as specified.
if "${ghc[@]}" -fforce-recomp -DPLAIN -c ReifyClient.hs >plain.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' plain.log || ! grep -Fq 'Copied Maybe Bool' plain.log; then
    cat plain.log >&2
    exit 1
fi
"${ghc[@]}" -fforce-recomp -DPLAIN -DEXPLICIT -c ReifyClient.hs
