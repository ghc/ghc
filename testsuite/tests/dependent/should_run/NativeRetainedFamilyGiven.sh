#!/usr/bin/env bash
set -euo pipefail
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
compile=("$1" "-B$2" -v0 -package-env - -fforce-recomp -dcore-lint -dstg-lint
         -fdiagnostics-color=never -outputdir "$out" -c NativeRetainedFamilyGiven.hs)
"${compile[@]}"
"${compile[@]}" -O2
for mode in OUTSIDE DISTINCT ROLE CYCLE; do
    if "${compile[@]}" "-D$mode" >"$out/$mode.log" 2>&1; then
        echo "Unexpected proof in $mode" >&2
        exit 1
    fi
    code=GHC-05617
    if [[ $mode == OUTSIDE ]]; then code=GHC-83865; fi
    grep -Fq "error: [$code]" "$out/$mode.log" || { cat "$out/$mode.log" >&2; exit 1; }
    if [[ $mode == OUTSIDE ]]; then grep -Fq 'Nothing -> Refl' "$out/$mode.log"; fi
done
