#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
g=("$1" "-B$2" -v0 -O0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
g+=("$@")
cp "$src"/{PackedVecLibrary,PackedLength}.hs .
"${g[@]}" -c PackedVecLibrary.hs PackedLength.hs
"${g[@]}" --show-iface PackedLength.hi >worker.iface
grep -Fq 'T:packageLength' worker.iface

cat >Unsupported.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module Unsupported where
import PackedVecLibrary
data Same a where
    Same :: foreach (n :: Nat) -> Vec a 'Z -> Same a
packageLength :: Same a -> Nat
packageLength (Same n _) = n
HS
"${g[@]}" -c Unsupported.hs
"${g[@]}" --show-iface Unsupported.hi >unsupported.iface
if grep -Fq 'T:packageLength' unsupported.iface; then cat unsupported.iface; exit 1; fi

cat >ProjectionRule.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module ProjectionRule where
import PackedLength
import PackedVecLibrary
{-# RULES "packageLength/zero" forall p. packageLength p = Z #-}
HS
"${g[@]}" -fenable-rewrite-rules -fno-omit-interface-pragmas -c ProjectionRule.hs
"${g[@]}" --show-iface ProjectionRule.hi >rule.iface
grep -Fq 'packageLength/zero' rule.iface
cat >Use.hs <<'HS'
{-# LANGUAGE CPP, ExperimentalDependentTypes, GADTs #-}
module Use where
import PackedVecLibrary
import qualified PackedLength as Worker
#ifdef IMPORT_RULE
import ProjectionRule ()
#endif
payload :: foreach (p :: PackedVec a) -> Vec a (Worker.packageLength p)
payload (PackVec _ xs) = xs
HS
reject() {
    local message=$1; shift
    if "${g[@]}" -fenable-rewrite-rules -fno-ignore-interface-pragmas -fforce-recomp -fno-code "$@" -c Use.hs >bad.log 2>&1 ||
       ! grep -Fq 'Use.hs:' bad.log || ! grep -Fq "$message" bad.log; then
        cat bad.log >&2
        exit 1
    fi
}
reject 'A rewrite rule can change the result' -DIMPORT_RULE

write_worker() {
    cat >PackedLength.hs <<HS
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module PackedLength (packageLength) where
import PackedVecLibrary
packageLength :: PackedVec a -> Nat
$1
HS
}
write_worker 'packageLength (PackVec n xs) = case xs of { VNil -> n; VCons _ _ -> n }'
"${g[@]}" -fforce-recomp -c PackedLength.hs
reject 'No checked equations are available'
cp "$src/PackedLength.hs" .
cat >>PackedLength.hs <<'HS'
{-# NOINLINE packageLength #-}
{-# RULES "packageLength/zero" forall p. packageLength p = Z #-}
HS
"${g[@]}" -fforce-recomp -c PackedLength.hs
reject 'No checked equations are available'
write_worker 'packageLength (PackVec n _) = S n'
"${g[@]}" -fforce-recomp -c PackedLength.hs
reject 'Could not deduce'
