#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd "$(dirname "$0")/app-under-retained-equality" && pwd)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never -outputdir .)

for ql in -XNoImpredicativeTypes -XImpredicativeTypes; do
    for opt in -O0 -O2 -fno-code; do
        mkdir -p "$out/$ql/$opt"/{provider,client}
        cd "$out/$ql/$opt/provider"
        "${ghc[@]}" "$ql" "$opt" -fwrite-interface -c "$src/Provider.hs"
        cd ../client
        cp ../provider/Provider.hi .
        "${ghc[@]}" "$ql" "$opt" -c "$src/Main.hs"
        if [[ "$opt" != -fno-code ]]; then
            "${ghc[@]}" Main.o ../provider/Provider.o -o run
            test "$(./run)" = '(True,True)'
        fi
        if "${ghc[@]}" "$ql" "$opt" -fno-code -c "$src/Reject.hs" >reject.log 2>&1; then
            echo "Expected application argument errors" >&2
            exit 1
        fi
        if ! grep -Eq 'Reject\.hs:10:[0-9]+: error: \[GHC-05617\]' reject.log ||
           ! grep -Eq 'Reject\.hs:15:[0-9]+: error: \[GHC-05617\]' reject.log; then
            cat reject.log >&2
            exit 1
        fi
    done
done
