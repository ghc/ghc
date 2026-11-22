#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -package template-haskell -fexternal-interpreter -dcore-lint -i -i.)
warnings=(-Werror=pun-uses -Werror=pun-bindings)

mkdir provider client
(
    cd provider
    cp ../{Provider,Forward}.hs .
    "${ghc[@]}" -c Provider.hs
    "${ghc[@]}" "${warnings[@]}" -c Forward.hs
    cp Provider.{hi,o} Forward.{hi,o} ../client/
)
cd client
cp ../{Main,Ambiguous}.hs .
"${ghc[@]}" "${warnings[@]}" -c Main.hs
"${ghc[@]}" Main.o Forward.o Provider.o -o run
./run
if "${ghc[@]}" "${warnings[@]}" -fdiagnostics-color=never -fno-code -c Ambiguous.hs > diagnostic 2>&1; then
    exit 1
fi
grep -Fq 'Ambiguous.hs:5:7: error: [GHC-41293]' diagnostic
grep -Fq 'Ambiguous.hs:3:27-34' diagnostic
grep -Fq 'Ambiguous.hs:3:17-24' diagnostic
test "$(grep -c 'error:' diagnostic)" -eq 1
