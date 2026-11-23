#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i.
     -fexternal-interpreter -fdiagnostics-color=never -dcore-lint -dstg-lint)

cd "$out"
cat > Ordinary.hs <<'HS'
{-# LANGUAGE TemplateHaskell #-}
module Ordinary where
import qualified Language.Haskell.TH as TH

data Ordinary = Ordinary { ordinaryField :: Bool }

$(do TH.TyConI (TH.DataD cxt _ params kind [TH.RecC _ [(_, bang, ty)]] derivs) <-
        TH.reify ''Ordinary
     let record = TH.mkName "Recovered"
         field = TH.mkName "recoveredField"
     pure [TH.DataD cxt record params kind [TH.RecC record [(field, bang, ty)]] derivs])

observe :: Bool
observe = recoveredField (Recovered { recoveredField = True })
HS
"${ghc[@]}" --make -no-link Ordinary.hs

cp "$src/"{PackedVecLibrary,NamedSiblingColumns}.hs .
cat > Named.hs <<'HS'
{-# LANGUAGE TemplateHaskell #-}
module Named where
import qualified Language.Haskell.TH as TH
import NamedSiblingColumns (NamedColumns)

$(do TH.TyConI (TH.DataD cxt _ params kind [TH.RecC _ fields] derivs) <-
        TH.reify ''NamedColumns
     let record = TH.mkName "Recovered"
     pure [TH.DataD cxt record params kind [TH.RecC record fields] derivs])
HS
if "${ghc[@]}" --make -no-link Named.hs > rejected.log 2>&1 ||
   ! grep -Fq 'named retained record constructor' rejected.log; then
    cat rejected.log >&2
    exit 1
fi
