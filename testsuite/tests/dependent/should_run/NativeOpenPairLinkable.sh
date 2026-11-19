#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -fno-write-if-simplified-core
     -fdiagnostics-color=never -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
ways=()
if [[ " $* " != *" -dynamic "* ]] &&
   "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi

run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
cd "$run"
mkdir artifacts just-interfaces root-object complete
cat >artifacts/Auxiliary.hs <<'HS'
module Auxiliary (marker) where
marker :: Bool
marker = True
HS
cat >artifacts/PairWorker.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module PairWorker (choose, marker) where
import qualified Auxiliary
marker :: Bool
marker = Auxiliary.marker
{-# NOINLINE [1] choose #-}
choose :: (a, a) -> a
choose (first, _) = first
HS
(
    cd artifacts
    "${ghc[@]}" "${ways[@]}" -c Auxiliary.hs
    "${ghc[@]}" "${ways[@]}" -i. -c PairWorker.hs
    "${ghc[@]}" --show-iface PairWorker.hi >worker.iface
    grep -Fq 'T:choose' worker.iface
)
cat > Mixed.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module Mixed where
import Data.Type.Equality ((:~:)(Refl))
import qualified PairWorker as W
direct :: W.choose (Prelude.quotRem 17 5) :~: 3
direct = Refl
nested :: Prelude.id (W.choose (Prelude.quotRem 17 5)) :~: 3
nested = Refl
HS
sed -e 's/module Mixed where/module LateRule where/' \
    -e '/^module /a import Numeric.Natural (Natural)' Mixed.hs >LateRule.hs
cat >>LateRule.hs <<'HS'

{-# RULES "choose/late" [~1] forall a b. W.choose @Natural (a, b) = b #-}
HS
line=$(awk '/^direct = Refl$/ {print NR; exit}' LateRule.hs)
for mode in just-interfaces root-object complete; do
    cp artifacts/*.hi Mixed.hs LateRule.hs "$mode/"
    if compgen -G 'artifacts/*.dyn_hi' > /dev/null; then
        cp artifacts/*.dyn_hi "$mode/"
    fi
    if [[ $mode != just-interfaces ]]; then
        cp artifacts/PairWorker.o "$mode/"
        if [[ -f artifacts/PairWorker.dyn_o ]]; then cp artifacts/PairWorker.dyn_o "$mode/"; fi
    fi
    if [[ $mode == complete ]]; then
        cp artifacts/Auxiliary.o "$mode/"
        if [[ -f artifacts/Auxiliary.dyn_o ]]; then cp artifacts/Auxiliary.dyn_o "$mode/"; fi
    fi
    (
        cd "$mode"
        "${ghc[@]}" -i. -c Mixed.hs
        if [[ $mode == complete ]]; then
            "${ghc[@]}" -i. -fenable-rewrite-rules -fno-omit-interface-pragmas -c LateRule.hs
            "${ghc[@]}" --show-iface LateRule.hi >rule.iface
            grep -Fq 'choose/late' rule.iface
        elif "${ghc[@]}" -i. -fenable-rewrite-rules -c LateRule.hs >rule.log 2>&1 ||
             ! grep -Eq "^LateRule[.]hs:$line:[0-9]+(-[0-9]+)?: error: [[]GHC-83605[]]" rule.log ||
             ! grep -Fq 'choose' rule.log; then
            cat rule.log >&2
            exit 1
        fi
        printf '%s passed\n' "$mode"
    )
done
