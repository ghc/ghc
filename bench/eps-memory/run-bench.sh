#!/usr/bin/env bash
# Compile Cabal-syntax with a frozen compiler and measure the memory reachable
# from GHC's ExternalPackageState.
#
#   bench/eps-memory/run-bench.sh LABEL
#
# LABEL     a compiler frozen with freeze-compiler.sh (compilers/LABEL); the
#           frozen copy of GHC/Unit/External.hs gives the EPS field names
#
# Environment:
#   CABAL_DIR  cabal checkout (default: ../../cabal relative to the worktree)
#   PACKAGE    package to compile (default: Cabal-syntax)
#   OPT        optimisation level (default: -O1)
#   GHC_OPTS   extra GHC options (default: -j1)
#   SKIP_RTS=1 skip the unpaused run that collects +RTS -t statistics
#
# Results go to results/LABEL/: eps.md (tables), eps.hsdat (eps-compare input),
# rts-stats, logs.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
label=${1:?usage: run-bench.sh LABEL}
cabal_dir=${CABAL_DIR:-$(cd "$top/../../cabal" && pwd)}
package=${PACKAGE:-Cabal-syntax}
opt=${OPT:--O1}
ghc_opts=${GHC_OPTS:--j1}

ghc="$here/compilers/$label/bin/ghc"
[ -x "$ghc" ] || { echo "no frozen compiler $ghc; run freeze-compiler.sh $label" >&2; exit 1; }

out="$here/results/$label"
work="$here/work/$label"
mkdir -p "$out" "$work"

echo "== building benchmark tools"
(cd "$here" && cabal build -v0 all)
bin() { (cd "$here" && cabal list-bin -v0 "$1"); }
eps_flags=$(bin eps-flags)
eps_analyse=$(bin eps-analyse)

echo "== computing flags with hie-bios"
"$eps_flags" --ghc "$ghc" --package-dir "$cabal_dir/$package" --work "$work" \
  --out "$work/ghc.rsp" --opt "$opt"
comp_dir=$(cat "$work/ghc.rsp.cwd")

run_ghc() {
  local odir=$1; shift
  rm -rf "$odir"; mkdir -p "$odir"
  (cd "$comp_dir" && "$ghc" --make @"$work/ghc.rsp" -fforce-recomp -outputdir "$odir" $ghc_opts "$@")
}

if [ "${SKIP_RTS:-0}" != 1 ]; then
  echo "== run 1: unpaused, collecting +RTS -t statistics"
  run_ghc "$work/out-rts" +RTS -t"$out/rts-stats" --machine-readable -RTS \
    > "$out/ghc-rts.log" 2>&1 || { tail -30 "$out/ghc-rts.log"; exit 1; }
fi

echo "== run 2: pause at end, analyse EPS with ghc-debug"
sock="$work/ghc-debug.sock"
rm -f "$sock"
odir="$work/out-debug"
rm -rf "$odir"; mkdir -p "$odir"
# exec, so that $! is the pid of ghc itself
(cd "$comp_dir" && GHC_DEBUG_SOCKET="$sock" GHC_DEBUG_PAUSE_AT_END=1 \
   exec "$ghc" --make @"$work/ghc.rsp" -fforce-recomp -outputdir "$odir" $ghc_opts) \
  > "$out/ghc-debug.log" 2>&1 &
ghc_pid=$!
trap 'kill -9 $ghc_pid 2>/dev/null || true' EXIT

for _ in $(seq 1 600); do
  [ -S "$sock" ] && break
  kill -0 $ghc_pid 2>/dev/null || { echo "ghc exited early" >&2; tail -30 "$out/ghc-debug.log"; exit 1; }
  sleep 0.1
done
[ -S "$sock" ] || { echo "ghc-debug socket never appeared" >&2; exit 1; }

rts_arg=()
[ -f "$out/rts-stats" ] && rts_arg=(--rts-stats "$out/rts-stats")
"$eps_analyse" --socket "$sock" --external-hs "$here/compilers/$label/External.hs" \
  --label "$label" --out "$out/eps" --ghc-log "$out/ghc-debug.log" "${rts_arg[@]}" 2> >(tee "$out/analyse.log" >&2)

kill -9 $ghc_pid 2>/dev/null || true
wait $ghc_pid 2>/dev/null || true
trap - EXIT
grep -q "ghc-debug: paused" "$out/ghc-debug.log" || echo "warning: ghc did not report pausing" >&2
echo "== done: $out/eps.md"
cat "$out/eps.md"
