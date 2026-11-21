#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/promoted-class" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{target,provider,client}
    cd "$out/$opt/target"
    cp "$src/ChooseTarget.hs" .
    if "${ghc[@]}" "$opt" -c ChooseTarget.hs > failure 2>&1; then
        echo "Accepted the abstract promoted method at $opt"
        exit 1
    fi
    grep -Eq '^ChooseTarget.hs:[0-9]+:[0-9]+: error: \[GHC-70566\]' failure
    grep -Fq 'The local dependency choose is not an ordinary function binding.' failure

    cd ../provider
    cp "$src/ChooseControl.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -c ChooseControl.hs
    cd ../client
    cp "$src/Main.hs" ../provider/ChooseControl.hi .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o ../provider/ChooseControl.o -o probe
    test "$(./probe yes)" = True
    test "$(./probe no)" = False
done
