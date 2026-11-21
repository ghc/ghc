#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/promoted-class" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{target,provider,client,evidence}
    cd "$out/$opt/target"
    cp "$src/ChooseTarget.hs" .
    "${ghc[@]}" "$opt" -c ChooseTarget.hs

    cd ../provider
    cp "$src/ChooseControl.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -c ChooseControl.hs
    cd ../client
    cp "$src/Main.hs" ../provider/ChooseControl.hi .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o ../provider/ChooseControl.o -o probe
    test "$(./probe yes)" = True
    test "$(./probe no)" = False

    cd ../evidence
    cp "$src/ChooseEvidence.hs" "$src/EvidenceMain.hs" .
    if "${ghc[@]}" "$opt" -DMISSING_GIVEN -c ChooseEvidence.hs > missing 2>&1; then
        echo "Accepted the call without Choose b at $opt"
        exit 1
    fi
    grep -Eq '^ChooseEvidence.hs:[0-9]+:[0-9]+: error: \[GHC-39999\]' missing
    grep -Fq 'add (Choose b) to the context' missing

    "${ghc[@]}" "$opt" -DPROMOTED -c ChooseEvidence.hs

    "${ghc[@]}" "$opt" -c ChooseEvidence.hs
    "${ghc[@]}" "$opt" -c EvidenceMain.hs
    "${ghc[@]}" "$opt" EvidenceMain.o ChooseEvidence.o -o evidence
    test "$(./evidence yes)" = '(True,False)'
    test "$(./evidence no)" = '(False,True)'
done

mkdir -p "$out/specialise"/{provider,client}
cd "$out/specialise/provider"
cp "$src/../"{PackedVecLibrary,ImplicitVector,RetainedNaturals,ScopedClassMethod}.hs .
"${ghc[@]}" -O2 -fomit-interface-pragmas -fno-write-if-simplified-core \
    -c PackedVecLibrary.hs ImplicitVector.hs RetainedNaturals.hs ScopedClassMethod.hs
cd ../client
cp ../provider/*.hi "$src/ScopedSpecialise.hs" .
for opt in -O1 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -ddump-rule-firings -c ScopedSpecialise.hs > firings
    grep -Fq 'SPEC countKey @Int' firings
    "${ghc[@]}" "$opt" ScopedSpecialise.o ../provider/*.o -o select
    test "$(./select)" = '([7],[],[],2)'
done
