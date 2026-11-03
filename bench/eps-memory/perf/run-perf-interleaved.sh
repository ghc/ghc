#!/usr/bin/env bash
# Time the perf tests with several frozen compilers, interleaved repetition by
# repetition, so that changes in machine load affect all compilers alike.
#
#   bench/eps-memory/perf/run-perf-interleaved.sh REPS LABEL...
#
# Uses compilers/perf-LABEL/bin/ghc (frozen copies of _default+ipe/stage1).
# Results: results/perf-timing/LABEL/repN (OUT_DIR overrides results/perf-timing).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
reps=${1:?usage: run-perf-interleaved.sh REPS LABEL...}; shift
export OUT_DIR=${OUT_DIR:-$here/../results/perf-timing}
for rep in $(seq 1 "$reps"); do
  for label in "$@"; do
    TEST_COMPILER="$here/../compilers/perf-$label/bin/ghc" FIRST_REP=$rep \
      "$here/run-perf-tests.sh" "$label" 1
  done
done
