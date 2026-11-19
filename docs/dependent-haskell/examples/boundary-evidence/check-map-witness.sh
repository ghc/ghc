#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -lt 2 || $1 != /* || $2 != /* ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR [WAY_FLAG ...]\n' "$0" >&2
    exit 2
fi
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -Wno-inline-rule-shadowing)
shift 2
ghc+=("$@")
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp -- "$src"/{MapWitness,MapRuleReplacement,UseMapWitness}.hs .

"${ghc[@]}" -O0 -fno-enable-rewrite-rules -c MapWitness.hs MapRuleReplacement.hs
line=$(awk '/^fresh = Refl$/ {print NR}' UseMapWitness.hs)
if "${ghc[@]}" -O1 -fenable-rewrite-rules -DFRESH -c UseMapWitness.hs >fresh.log 2>&1 ||
   ! grep -Eq "^UseMapWitness[.]hs:$line:.*error: \[GHC-83605\]" fresh.log ||
   ! grep -Fq 'only when rewrite rules are disabled' fresh.log; then
    cat fresh.log >&2
    exit 1
fi
"${ghc[@]}" -O1 -fenable-rewrite-rules -ddump-rule-firings -c UseMapWitness.hs >firings
grep -F 'Rule fired: replace introduced Bool builder' firings >/dev/null
"${ghc[@]}" UseMapWitness.o MapWitness.o MapRuleReplacement.o -o use-map-witness
result=$(./use-map-witness)
printf '%s\n' "$result"
[[ $result == '(False,True)' ]]

# The equality is required to move the head to the public list index.
sed 's/checkedFirst (Checked Refl h)/checkedFirst (Checked _ h)/' UseMapWitness.hs >discarded.hs
mv discarded.hs UseMapWitness.hs
line=$(awk '/^checkedFirst \(Checked _ h\) = h$/ {print NR}' UseMapWitness.hs)
if "${ghc[@]}" -O1 -fforce-recomp -c UseMapWitness.hs >discarded.log 2>&1 ||
   ! grep -Eq "^UseMapWitness[.]hs:$line:.*error: " discarded.log ||
   ! grep -Fq "Couldn't match" discarded.log; then
    cat discarded.log >&2
    exit 1
fi
