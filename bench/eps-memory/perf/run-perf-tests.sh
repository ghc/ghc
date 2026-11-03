#!/usr/bin/env bash
# Run the compiler perf tests listed in tests.txt with the compiler of a hadrian
# build root and keep the RTS stats files (allocation, residency, CPU time).
#
#   bench/eps-memory/perf/run-perf-tests.sh LABEL [REPS]
#
# Environment: TOP (GHC checkout, default: this one), OUT_DIR (default
# results/perf), BUILD_ROOT (default _default+ipe), FLAVOUR (default default+ipe),
# THREADS (testsuite parallelism, default 1 so that timings are not disturbed),
# TESTS (space separated, default: tests.txt), TEST_COMPILER (a ghc to test
# instead of the one built in BUILD_ROOT, e.g. compilers/perf-LABEL/bin/ghc).
# Without TEST_COMPILER, the compiler is the one currently built in BUILD_ROOT
# (hadrian --freeze1), so check out and build the commit to measure first.
# Results: results/perf/LABEL/repN/<test>.<way>.stats (+ metrics.tsv, test.log),
# results/perf/LABEL/commit. Compare with
#   cabal run -v0 perf-compare -- results/perf/BASE results/perf/NEW
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
top=${TOP:-$(cd "$here/../../.." && pwd)}
label=${1:?usage: run-perf-tests.sh LABEL [REPS]}
reps=${2:-3}
root=${BUILD_ROOT:-_default+ipe}
flavour=${FLAVOUR:-default+ipe}
threads=${THREADS:-1}
tests=${TESTS:-$(grep -v '^#' "$here/tests.txt" | tr '\n' ' ')}
out="${OUT_DIR:-$here/../results/perf}/$label"

mkdir -p "$out"
if [ -n "${TEST_COMPILER:-}" ]; then
  tc_arg=(--test-compiler="$TEST_COMPILER")
  cat "$(dirname "$(dirname "$TEST_COMPILER")")/commit" > "$out/commit" 2>/dev/null || echo "$TEST_COMPILER" > "$out/commit"
else
  tc_arg=()
  git -C "$top" rev-parse HEAD > "$out/commit"
  git -C "$top" log -1 --format=%s HEAD >> "$out/commit"
fi
first_rep=${FIRST_REP:-1}

for rep in $(seq "$first_rep" $((first_rep + reps - 1))); do
  d="$out/rep$rep"
  rm -rf "$d"; mkdir -p "$d/tmp"
  echo "== $label rep $rep"
  # The testsuite creates its temporary test directories with mkdtemp, which
  # honours TMPDIR; -k keeps them, so we can pick up the stats files.
  (cd "$top" && TMPDIR="$d/tmp" ./hadrian/build -o"$root" --freeze1 --flavour="$flavour" \
      -j"$threads" test -k "${tc_arg[@]}" --only="$tests" --summary-metrics="$d/metrics.tsv" \
      --test-verbose=2) > "$d/test.log" 2>&1 || echo "  (testsuite exit code $?, see $d/test.log)"
  # Keep only the RTS stats of the compiler/program runs.
  find "$d/tmp" -name '*.stats' -print0 | while IFS= read -r -d '' f; do
    cp "$f" "$d/$(basename "$f")"
  done
  rm -rf "$d/tmp"
  grep -E 'unexpected|Unexpected' "$d/test.log" | tail -5 || true
  echo "  $(ls "$d"/*.stats 2>/dev/null | wc -l) stats files"
done
