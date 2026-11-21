#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider client
cp "$src/NativeClosedMatchabilityProvider.hs" provider/
(cd provider && "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeClosedMatchabilityProvider.hs)
cp provider/NativeClosedMatchabilityProvider.hi "$src"/NativeClosedMatchability{Client,Conflict}.hs client/
cd client
"${ghc[@]}" -c NativeClosedMatchabilityClient.hs
"${ghc[@]}" -dppr-cols=200 -fprint-explicit-kinds --show-iface NativeClosedMatchabilityClient.hi | sed "s/NativeClosedMatchabilityProvider\.K/K/g" > client.iface
grep -Eq 'type PairOf @{([^}]+)} @{([^}]+)} b c = \(K @{\1} b, K @{\2} c\)' client.iface
if grep -Eq 'type PairOf @{([^}]+)} @{\1} b c' client.iface; then exit 1; fi
for mode in CONFLICT FIXED INVERSE; do
    if "${ghc[@]}" "-D$mode" -c NativeClosedMatchabilityConflict.hs >conflict.log 2>&1; then
        cat conflict.log >&2
        exit 1
    fi
    if [[ $mode == INVERSE ]]; then
        grep -Fq '[GHC-25897]' conflict.log || { cat conflict.log >&2; exit 1; }
    else
        grep -Fq '[GHC-83865]' conflict.log &&
        grep -Fq 'Matchable' conflict.log &&
        grep -Fq 'Unmatchable' conflict.log || { cat conflict.log >&2; exit 1; }
    fi
done
