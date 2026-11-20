#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fhpc -dcore-lint -dstg-lint)
hpc=$3
provider=NativeRetainedIndexHpcProvider
client=NativeRetainedIndexHpcClient

check_tick() {
    local module=$1 span=$2 count=$3
    awk -v module="$module" -v span="$span" -v count="$count" '
        $3 == module && $4 == span && $5 == "ExpBox" {
            seen++
            if ($2 != count) incorrect = 1
        }
        END { exit (seen != 1 || incorrect) }
    ' ticks || { echo "Expected $count hits for $module $span" >&2; cat ticks >&2; exit 1; }
}

for opt in -O0 -O2; do
    work="$out/$opt"
    mkdir -p "$work"/{provider,client,hidden}
    cd "$work/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "$opt" -c "$provider.hs"
    mv "$provider.hs" ../hidden/
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "$opt" -c "$client.hs"
    "${ghc[@]}" "$opt" "$client.o" "../provider/$provider.o" -o run
    ./run
    "$hpc" show --hpcdir=.hpc --hpcdir=../provider/.hpc run.tix >ticks

    # skippedIndex leaves both constructors unevaluated.
    check_tick "$provider" 20:21-20:25 0
    check_tick "$provider" 20:24-20:24 0
    # selectedIndex evaluates S and leaves its Z field unevaluated.
    check_tick "$provider" 24:25-24:29 1
    check_tick "$provider" 24:28-24:28 0
    # The false guard leaves guardedIndex unevaluated.
    check_tick "$provider" 28:21-28:25 0
    check_tick "$provider" 28:24-28:24 0
    # The true guard evaluates both constructors in demandedIndex.
    check_tick Main 17:22-17:26 1
    check_tick Main 17:25-17:25 1
done
