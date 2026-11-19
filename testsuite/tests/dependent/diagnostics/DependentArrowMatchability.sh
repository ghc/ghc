#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
ghc=("$1" "-B$2" -O -v0 -package-env - -dcore-lint -fdiagnostics-color=never)

# Compile the consumer with the provider's interface alone.
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider
cp "$src/DependentArrowProvider.hs" provider/
cp "$src/DependentArrowMatchability.hs" .
(cd provider && "${ghc[@]}" -c DependentArrowProvider.hs)
cp provider/DependentArrowProvider.hi .
"${ghc[@]}" -c DependentArrowMatchability.hs
"${ghc[@]}" -fforce-recomp -XExperimentalDependentTypes -c DependentArrowMatchability.hs

reject() {
    local line=$1 message=$2
    if "${ghc[@]}" -c Rejected.hs > reject.log 2>&1 ||
       ! grep -Eq "^Rejected\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

# Keep the declaration and use identical except for the head matchability.
sed -e 's/^module DependentArrowMatchability/module Rejected/' \
    -e 's/(f :: FUN @M/(f :: FUN @U/' DependentArrowMatchability.hs > Rejected.hs
line=$(grep -n '^inverseM _ Refl = Refl$' Rejected.hs | cut -d: -f1)
reject "$line" 'In the expression: Refl'

for bad in "P.D (FUN @U 'Many Type Type) Maybe" \
           "P.D (FUN @M 'Many Type Type) P.Abstract"; do
    sed 's/^module DependentArrowMatchability/module Rejected/' DependentArrowMatchability.hs > Rejected.hs
    printf '\ntype Bad = %s\n' "$bad" >> Rejected.hs
    line=$(wc -l < Rejected.hs | tr -d ' ')
    reject "$line" '[GHC-83865]'
    grep -Fq 'Matchable' reject.log
    grep -Fq 'Unmatchable' reject.log
done
