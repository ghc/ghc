#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
client=NativeNamedOpaqueCharacterClient.hs

for opt in 0 2; do
    mkdir -p "$work/$opt"/{provider,reader}
    cd "$work/$opt/provider"
    cp "$src/CharacterDecision.hs" .
    "${ghc[@]}" "-O$opt" -c CharacterDecision.hs
    cp CharacterDecision.hi ../reader/

    cd ../reader
    cp "$src/$client" .
    "${ghc[@]}" "-O$opt" -c "$client"
    for control in WRONG_CHARACTER_RESULT MIXED_CHARACTER_CALL; do
        if "${ghc[@]}" "-O$opt" -D"$control" -fforce-recomp -c "$client" > reject.log 2>&1 ||
           ! grep -Fq '[GHC-83865]' reject.log; then
            cat reject.log >&2
            exit 1
        fi
    done
done
