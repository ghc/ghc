#!/usr/bin/env bash
# Compile a package (default: Cabal-syntax) with ghc-eps-store, once per EPS
# store, and compare memory and time.
#
#   bench/eps-store/run-stores.sh LABEL [STORE...]     (default: memory hi sqlite)
#
# Environment:
#   BUILD_ROOT  GHC build root to use (default: _default+ipe)
#   RSP         GHC response file with the package's flags (default: the one
#               bench/eps-memory computed for $RSP_FROM)
#   RSP_FROM    bench/eps-memory/work/<label> to take ghc.rsp(.cwd) from
#               (default: slim-eps)
#   GHC_OPTS    extra GHC options (default: -j1)
#
# Results: results/LABEL/<store>.{log,rts}, results/LABEL/summary.md
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
label=${1:?usage: run-stores.sh LABEL [STORE...]}; shift
stores=("$@"); [ ${#stores[@]} -gt 0 ] || stores=(memory hi sqlite)
export BUILD_ROOT=${BUILD_ROOT:-_default+ipe}
rsp_from=${RSP_FROM:-slim-eps}
rsp=${RSP:-$top/bench/eps-memory/work/$rsp_from/ghc.rsp}
comp_dir=$(cat "${rsp}.cwd")
ghc_opts=${GHC_OPTS:--j1}

libdir=$("$top/$BUILD_ROOT/stage1/bin/ghc" --print-libdir)
echo "== building ghc-eps-store against $BUILD_ROOT"
# cabal does not notice a rebuilt ghc library (same inplace unit id)
rm -rf "$here/dist-newstyle/$BUILD_ROOT/build"
(cd "$here" && ./cabal.sh build -v0 all)
exe=$(cd "$here" && ./cabal.sh list-bin -v0 ghc-eps-store)

out="$here/results/$label"
work="$here/results/$label/work"
mkdir -p "$out" "$work"

for s in "${stores[@]}"; do
  echo "== $s"
  # The same output path for every store: it is part of the flags hash.
  odir="$work/out"
  rm -rf "$odir" "$work/out-$s"; mkdir -p "$odir"
  start=$(date +%s.%N)
  (cd "$comp_dir" && "$exe" -B"$libdir" --eps-store="$s" --eps-stats \
      @"$rsp" -fforce-recomp -outputdir "$odir" $ghc_opts \
      +RTS -s"$out/$s.rts" -RTS) > "$out/$s.log" 2>&1 \
    || { tail -30 "$out/$s.log"; exit 1; }
  end=$(date +%s.%N)
  echo "wall seconds: $(awk -v a="$start" -v b="$end" 'BEGIN { printf "%.1f", b - a }')" >> "$out/$s.log"
  mv "$odir" "$work/out-$s"
done

# Outputs must not depend on the store.
for s in "${stores[@]}"; do
  [ "$s" = "${stores[0]}" ] && continue
  if diff -rq "$work/out-${stores[0]}" "$work/out-$s" > "$out/diff-$s.txt"; then
    echo "outputs of $s identical to ${stores[0]}" | tee -a "$out/diff-$s.txt"
  else
    echo "WARNING: outputs of $s differ from ${stores[0]}, see $out/diff-$s.txt"
  fi
done

field() { grep -E "$2" "$out/$1.log" | head -1 | awk '{print $NF}'; }
mib() { awk -v b="$1" 'BEGIN { printf "%.1f", b / 1048576 }'; }
{
  echo "| store | live at end (MiB) | max live (MiB) | allocated (MiB) | rehydrations | wall (s) |"
  echo "|---|---:|---:|---:|---:|---:|"
  for s in "${stores[@]}"; do
    printf '| %s | %s | %s | %s | %s | %s |\n' "$s" \
      "$(mib "$(field "$s" 'live bytes at end')")" \
      "$(mib "$(field "$s" 'max live bytes')")" \
      "$(mib "$(field "$s" 'allocated bytes')")" \
      "$(field "$s" 'rehydrations' || true)" \
      "$(field "$s" 'wall seconds')"
  done
} | tee "$out/summary.md"
