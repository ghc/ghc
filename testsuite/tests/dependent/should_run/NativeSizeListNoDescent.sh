#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cp PreludeMapLengthSize.hs NativeSizeListNoDescent{,Source}.hs "$out"
cd "$out"
mkdir proof
mv NativeSizeListNoDescent.hs proof
for opt in 0 2; do
    mkdir "opt-$opt"
    "${ghc[@]}" -O"$opt" --make -no-link -outputdir "opt-$opt" NativeSizeListNoDescentSource.hs
    cd "opt-$opt"
    if "${ghc[@]}" -O"$opt" -c ../proof/NativeSizeListNoDescent.hs >error.log 2>&1; then
        echo "The provider exported equations for recursion without descent" >&2
        exit 1
    fi
    grep -Fq '[GHC-70565]' error.log || { cat error.log >&2; exit 1; }
    cd ..
done
