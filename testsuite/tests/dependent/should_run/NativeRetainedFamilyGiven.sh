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

# The pattern-match checker can try closed-family improvement on this Given.
for opt in -O0 -O2; do
    "${compile[@]}" "$opt" -DCLOSED_GIVEN
    if "${compile[@]}" "$opt" -DCLOSED_WRONG >"$out/closed-wrong.log" 2>&1; then
        echo 'Unexpected closed-family proof with a different result' >&2
        exit 1
    fi
    grep -Fq 'error: [GHC-05617]' "$out/closed-wrong.log" ||
        { cat "$out/closed-wrong.log" >&2; exit 1; }
done
