#!/usr/bin/env bash
set -euo pipefail
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
compile=("$1" "-B$2" -v0 -package-env - -fforce-recomp -dcore-lint -dstg-lint
         -fdiagnostics-color=never -outputdir "$out" -c NativeRetainedFamilyGiven.hs)
"${compile[@]}"
"${compile[@]}" -O2
for mode in OUTSIDE DISTINCT ROLE CYCLE CALL_HEAD CALL_ARG CALL_ROLE CALL_CYCLE; do
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

for opt in -O0 -O2; do
    if "${compile[@]}" "$opt" -DWRONG_CONTINUATION -ferror-spans >"$out/continuation.log" 2>&1; then
        echo 'Unexpected gcastWith proof for a different list' >&2
        exit 1
    fi
    grep -Fq 'error: [GHC-05617]' "$out/continuation.log"
    grep -Fq 'from the context: F xs ~ G xs' "$out/continuation.log"
    grep -Fq 'In the second argument of ‘gcastWith’, namely ‘Refl’' "$out/continuation.log"
done

# Use the sibling proof to choose the nominal intermediate before the continuation.
for opt in -O0 -O2; do
    for mode in COMPOSITION_WRONG COMPOSITION_AMBIGUOUS COMPOSITION_OUTSIDE; do
        log="$out/$mode.log"
        if "${compile[@]}" "$opt" "-D$mode" >"$log" 2>&1; then
            echo "Unexpected proof in $mode" >&2
            exit 1
        fi
        case "$mode" in
            COMPOSITION_WRONG)
                grep -E 'error: \[GHC-(05617|83865)\]' "$log" &&
                grep -F 'In the second argument of' "$log"
                ;;
            COMPOSITION_AMBIGUOUS)
                grep -F 'error: [GHC-83865]' "$log" &&
                grep -F 'is untouchable' "$log"
                ;;
            COMPOSITION_OUTSIDE)
                grep -F 'error: [GHC-83865]' "$log" &&
                grep -F '() -> Refl' "$log"
                ;;
        esac > /dev/null || { cat "$log" >&2; exit 1; }
    done
done

# Supply the boxed nominal Given when inferring a successor tail.
for opt in -O0 -O2; do
    build="$out/tail-${opt#-}"
    mkdir "$build"
    ghc=("$1" "-B$2" "$opt" -v0 -package-env - -dcore-lint -dstg-lint
         -fdiagnostics-color=never -outputdir "$build")
    "${ghc[@]}" --make -no-link NativeRetainedTailGiven.hs
    if "${ghc[@]}" -i -i"$build" -fforce-recomp -ferror-spans -DWRONG_TAIL \
         -c NativeRetainedTailGiven.hs >"$build/zero.log" 2>&1; then
        echo 'Unexpected successor tail from a zero-length proof' >&2
        exit 1
    fi
    grep -Fq 'error: [GHC-05617]' "$build/zero.log"
    grep -Fq 'from the context: extent ~ Z' "$build/zero.log"
    grep -Fq "In the first argument of ‘first’" "$build/zero.log"
done
