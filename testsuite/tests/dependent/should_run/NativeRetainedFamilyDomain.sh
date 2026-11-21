#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
cd "$out"
mkdir client
cp "$src/ConstrainedElements.hs" .
"${ghc[@]}" -fomit-interface-pragmas -c ConstrainedElements.hs
cd client
cp ../ConstrainedElements.hi "$src/ConstrainedElementsClient.hs" "$src/NativeRetainedFamilyDomain.hs" .
"${ghc[@]}" -c ConstrainedElementsClient.hs
for mode in WRONG_TAG LATE_GIVEN CALLBACK_ESCAPE CALLBACK_MISSING MISSING; do
    if "${ghc[@]}" -fforce-recomp "-D$mode" -fdefer-type-errors -c NativeRetainedFamilyDomain.hs > failure 2>&1; then
        echo "Accepted $mode"
        exit 1
    fi
    grep -Eq '^NativeRetainedFamilyDomain.hs:[0-9]+:[0-9]+: error: \[GHC-39999\]' failure
    case "$mode" in
        WRONG_TAG)
            grep -Fq 'Could not deduce' failure
            grep -Fq 'Collects (FieldKey b)' failure
            grep -Fq 'from the context: Collects (FieldKey a)' failure
            grep -A3 'bound by the type signature for:' failure | grep -Fq 'foreach (a :: Bool)'
            ;;
        LATE_GIVEN) grep -Fq "Collects (FieldKey 'False)" failure ;;
        CALLBACK_ESCAPE) grep -Fq 'Collects a' failure ;;
        CALLBACK_MISSING)
            grep -Fq 'Collects c' failure
            grep -A1 'a type expected by the context:' failure | grep -Fq 'Proxy c -> Elem c -> Elem c'
            ;;
        MISSING) grep -Fq 'Collects (FieldKey tag)' failure ;;
    esac
done
