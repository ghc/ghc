#!/usr/bin/env bash
# Find out what the peak memory of compiling a package consists of.
#
#   bench/eps-memory/run-peak.sh LABEL
#
# Needs work/LABEL/ghc.rsp from run-bench.sh. Compiles the package once with
# -ddump-timings (phase markers in the eventlog) and +RTS -l -hT (heap census
# by closure type every INTERVAL seconds, default 0.25), then runs eps-peak.
# Results: results/LABEL/peak.{md,hsdat}; compare with
#   cabal run -v0 eps-peak -- compare results/A/peak.hsdat results/B/peak.hsdat
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
label=${1:?usage: run-peak.sh LABEL}
interval=${INTERVAL:-0.25}
ghc_opts=${GHC_OPTS:--j1}
ghc="$here/compilers/$label/bin/ghc"
work="$here/work/$label"
out="$here/results/$label"
[ -f "$work/ghc.rsp" ] || { echo "no $work/ghc.rsp; run run-bench.sh $label first" >&2; exit 1; }
mkdir -p "$out"

(cd "$here" && cabal build -v0 exe:eps-peak)
eps_peak=$(cd "$here" && cabal list-bin -v0 eps-peak)

odir="$work/out-peak"; rm -rf "$odir"; mkdir -p "$odir"
(cd "$(cat "$work/ghc.rsp.cwd")" && \
  "$ghc" --make @"$work/ghc.rsp" -fforce-recomp -outputdir "$odir" $ghc_opts \
    -ddump-timings -ddump-to-file -dumpdir "$odir/dump" \
    +RTS -l -ol"$out/peak.eventlog" -hT -i"$interval" -RTS) > "$out/ghc-peak.log" 2>&1 \
  || { tail -30 "$out/ghc-peak.log"; exit 1; }

"$eps_peak" analyse "$out/peak.eventlog" "$label" "$out/peak"
cat "$out/peak.md"
