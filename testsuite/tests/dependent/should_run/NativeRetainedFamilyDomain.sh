#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
cd "$out"
mkdir client
cp "$src/ConstrainedElements.hs" .
"${ghc[@]}" "${stripped[@]}" -c ConstrainedElements.hs
cd client
cp ../ConstrainedElements.hi "$src/ConstrainedElementsClient.hs" "$src/NativeRetainedFamilyDomain.hs" .
"${ghc[@]}" "${stripped[@]}" -c ConstrainedElementsClient.hs
"${ghc[@]}" -DREQUIRED_KINDS -c NativeRetainedFamilyDomain.hs
"${ghc[@]}" -fforce-recomp -DKNOWN_PREFIX -c NativeRetainedFamilyDomain.hs
"${ghc[@]}" -fforce-recomp -DVALID_FORGOTTEN -c NativeRetainedFamilyDomain.hs
"${ghc[@]}" -fforce-recomp -DALIAS_FIELD -c NativeRetainedFamilyDomain.hs
for mode in ALIAS_OPEN ALIAS_ESCAPE FORGOTTEN MISSING_PREFIX MISSING_KIND WRONG_TAG LATE_GIVEN CALLBACK_ESCAPE CALLBACK_MISSING REQUIRED_MISSING MISSING; do
    if "${ghc[@]}" -fforce-recomp "-D$mode" -fdefer-type-errors -c NativeRetainedFamilyDomain.hs > failure 2>&1; then
        echo "Accepted $mode"
        exit 1
    fi
    grep -Eq '^NativeRetainedFamilyDomain.hs:[0-9]+:[0-9]+: error: \[GHC-39999\]' failure
    case "$mode" in
        ALIAS_OPEN) grep -Fq 'Allowed a' failure ;;
        ALIAS_ESCAPE) grep -Fq 'Collects c' failure ;;
        FORGOTTEN|MISSING_PREFIX|MISSING_KIND) grep -Fq 'Allowed Int' failure ;;
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
        REQUIRED_MISSING|MISSING) grep -Fq 'Collects (FieldKey tag)' failure ;;
    esac
done

# The ordinary wrapper and producer are both source-hidden in this reader.
cd "$out"
mkdir format
cp ConstrainedElements.hi client/ConstrainedElementsClient.hi "$src/ConstrainedFormatClient.hs" format/
cd format
cat >Main.hs <<'HS'
module Main where
import ConstrainedFormatClient (renderedRawView)
main :: IO ()
main = putStrLn renderedRawView
HS
for level in -O0 -O2; do
    "${ghc[@]}" "$level" -fforce-recomp -c ConstrainedFormatClient.hs Main.hs
    "${ghc[@]}" Main.o ConstrainedFormatClient.o ../client/ConstrainedElementsClient.o ../ConstrainedElements.o -o run
    [[ $(./run) == '[True,False]' ]]
done

sed -e 's/^    ( Allowed a$/    (/' -e 's/^    , forall x\./      forall x./' ConstrainedFormatClient.hs >MissingViews.hs
line=$(awk '/^renderRaw ::/ { print NR }' MissingViews.hs)
if "${ghc[@]}" -O2 -fforce-recomp -fdefer-type-errors -c MissingViews.hs >failure 2>&1 ||
   ! grep -Eq "^MissingViews.hs:$line:[0-9]+: error: \[GHC-39999\]" failure ||
   ! grep -Fq 'Could not deduce ‘Allowed a’ arising from a use of ‘FieldView’' failure; then
    cat failure
    exit 1
fi
