#!/usr/bin/env bash
# Coordination only: the work is done by tool/ (eps-hackage, system GHC) and
# runner/ (eps-hackage-run, the GHC API, built with the compiler under test).
#
#   bench.sh setup LABEL BUILD_ROOT [abstract-eps]
#       build pandoc's dependencies with BUILD_ROOT's GHC, record its flags,
#       build the runner (with EPS store support if abstract-eps is given)
#   bench.sh run LABEL MODE...
#       compile pandoc's library; MODE is ghc-api (GHC's default EPS) or
#       store:memory, store:hi, store:sqlite (needs abstract-eps)
#   bench.sh summary RESULT-FILE...
#
# Environment for run: GHC_OPTS (default -j1), RUNS (default 1), HASHES=1
# (record hashes of the outputs; add -fobject-determinism to GHC_OPTS).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)

tool() {
  (cd "$here/tool" && cabal build -v0 exe:eps-hackage)
  "$(cd "$here/tool" && cabal list-bin -v0 exe:eps-hackage)" "$@"
}

case "${1:-}" in
  setup)
    label=${2:?label}; root=${3:?build root}; flag=${4:-}
    case "$root" in /*) ;; *) root="$top/$root" ;; esac
    ghc="$root/stage1/bin/ghc"
    work="$here/work/$label"
    mkdir -p "$work"
    "$ghc" --version > "$work/ghc-version"
    tool deps  --ghc "$ghc" --project "$here/project" --work "$work"
    tool flags --ghc "$ghc" --project "$here/project" --work "$work"
    echo "== building the runner"
    flags=(); [ "$flag" = abstract-eps ] && flags=(--flags=+abstract-eps)
    # cabal does not notice a rebuilt ghc library (same inplace unit id)
    rm -rf "$work/runner-dist/build"
    (cd "$here/runner" && cabal --store-dir="$work/store" build -v1 \
       --builddir="$work/runner-dist" -w "$ghc" --with-hc-pkg="$root/stage1/bin/ghc-pkg" \
       "${flags[@]}" exe:eps-hackage-run > "$work/runner.log" 2>&1) \
      || { tail -30 "$work/runner.log"; exit 1; }
    (cd "$here/runner" && cabal --store-dir="$work/store" list-bin -v0 \
       --builddir="$work/runner-dist" -w "$ghc" "${flags[@]}" exe:eps-hackage-run) > "$work/runner"
    echo "$root" > "$work/build-root"
    echo "== $label ready"
    ;;
  run)
    label=${2:?label}; shift 2
    work="$here/work/$label"
    out="$here/results/$label"; mkdir -p "$out"
    libdir=$("$(cat "$work/build-root")/stage1/bin/ghc" --print-libdir)
    odir="$here/work/out"   # the same for every run: it is part of the flags hash
    for mode in "$@"; do
      store=(); [ "$mode" != ghc-api ] && store=(--store "${mode#store:}")
      hashes=(); [ "${HASHES:-0}" = 1 ] && hashes=(--hashes)
      for i in $(seq 1 "${RUNS:-1}"); do
        name=${mode/:/-}.$i
        echo "== $label $name"
        rm -rf "$odir"
        (cd "$(cat "$work/ghc.rsp.cwd")" && "$(cat "$work/runner")" \
           --libdir "$libdir" --label "$label" --out "$out/$name.result" \
           "${store[@]}" "${hashes[@]}" -- \
           @"$work/ghc.rsp" -fforce-recomp -outputdir "$odir" ${GHC_OPTS:--j1}) \
          > "$out/$name.log" 2>&1 || { tail -30 "$out/$name.log"; exit 1; }
      done
    done
    rm -rf "$odir"
    ;;
  summary)
    shift; tool summary "$@"
    ;;
  *)
    sed -n '2,15p' "$0"; exit 1 ;;
esac
