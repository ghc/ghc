#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")/nested-retained-case" && pwd)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never -outputdir .)

for opt in -O0 -O2 -fno-code; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    "${ghc[@]}" "$opt" -fwrite-interface -c "$src/Provider.hs"
    cd ../client
    cp ../provider/Provider.hi .
    "${ghc[@]}" "$opt" -c "$src/Main.hs"

    if [[ "$opt" != -fno-code ]]; then
        "${ghc[@]}" Main.o ../provider/Provider.o -o run
        ./run
    fi

    if "${ghc[@]}" "$opt" -fno-code -c "$src/Reject.hs" >reject.log 2>&1; then
        echo "Expected retained case errors" >&2
        exit 1
    fi
    if ! grep -Eq 'Reject\.hs:12:[0-9]+: error: \[GHC-05617\]' reject.log ||
       ! grep -Eq 'Reject\.hs:19:[0-9]+: error: \[GHC-83865\]' reject.log ||
       ! grep -Eq 'Reject\.hs:25:[0-9]+: error: \[GHC-18872\]' reject.log ||
       ! grep -Eq 'Reject\.hs:32:[0-9]+: error: \[GHC-83865\]' reject.log ||
       ! grep -Eq 'Reject\.hs:36:[0-9]+: error: \[GHC-25897\]' reject.log; then
        cat reject.log >&2
        exit 1
    fi
done
