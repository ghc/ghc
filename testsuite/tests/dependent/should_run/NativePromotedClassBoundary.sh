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

    cp "$src/../MappingWorker.hs" "$src/MappingTags.hs" ../provider/
    (cd ../provider && "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core \
        -c MappingWorker.hs MappingTags.hs)
    cp ../provider/{MappingWorker,MappingTags}.hi "$src/MappingScope.hs" .
    "${ghc[@]}" "$opt" -fignore-interface-pragmas -c MappingScope.hs
    for setting in WRONG_TAG GIVEN; do
        if "${ghc[@]}" "$opt" -fignore-interface-pragmas -fforce-recomp -D"$setting" -c MappingScope.hs > failure 2>&1; then
            echo "Accepted $setting at $opt"
            exit 1
        fi
        case "$setting" in
            WRONG_TAG) grep -Fq 'GHC-83865' failure && grep -Fq 'New (f x)' failure || exit 1 ;;
            GIVEN) grep -Fq 'GHC-83605' failure && grep -Fq 'MapLike Maybe' failure || exit 1 ;;
        esac
    done

    cp "$src"/{DirectMappingProvider,DirectMappingControl}.hs \
        "$src/../PreludeMappingProof.hs" .
    "${ghc[@]}" "$opt" -fignore-interface-pragmas -c PreludeMappingProof.hs
    cat > DirectMappingMain.hs <<'EOF'
module Main where
import PreludeMappingProof (example, presentExample)
main :: IO ()
main = mapM_ putStrLn [example, presentExample]
EOF
    "${ghc[@]}" "$opt" -c DirectMappingMain.hs
    "${ghc[@]}" "$opt" DirectMappingMain.o PreludeMappingProof.o -o direct
    printf '%s\n' '(Refl,Nothing)' '(Refl,Refl,Just False,Just True,True)' > expected
    ./direct > actual
    cmp expected actual
    for setting in GIVEN OTHER_HEAD OTHER_INPUT CANCEL CHANGED_RULE; do
        if "${ghc[@]}" "$opt" -fforce-recomp -D"$setting" -c DirectMappingControl.hs > failure 2>&1; then
            echo "Accepted a direct method with $setting at $opt"
            exit 1
        fi
        case "$setting" in
            GIVEN) grep -Fq 'GHC-83605' failure && grep -Fq 'Functor Maybe' failure || exit 1 ;;
            OTHER_HEAD|OTHER_INPUT) grep -Fq "Couldn't match" failure || exit 1 ;;
            CANCEL) grep -Fq 'GHC-25897' failure && grep -Fq 'Could not deduce' failure || exit 1 ;;
            CHANGED_RULE) grep -Fq 'rewrite rule' failure || exit 1 ;;
        esac
    done

    mv DirectMappingProvider.hs ../provider/
    (cd ../provider && "${ghc[@]}" -O2 -c DirectMappingProvider.hs)
    cp ../provider/DirectMappingProvider.hi .
    "${ghc[@]}" "$opt" -fignore-interface-pragmas -DALTERNATE -c DirectMappingControl.hs
    if "${ghc[@]}" "$opt" -fforce-recomp -fignore-interface-pragmas -DSTRICT_FIELD -c DirectMappingControl.hs > failure 2>&1; then
        echo "Accepted a strict callback field at $opt"
        exit 1
    fi
    grep -Fq 'GHC-70566' failure
    (cd ../provider && "${ghc[@]}" -O2 -DHIDE_WORKER -fforce-recomp -c DirectMappingProvider.hs)
    cp ../provider/DirectMappingProvider.hi .
    if "${ghc[@]}" "$opt" -fignore-interface-pragmas -DALTERNATE -c DirectMappingControl.hs > failure 2>&1; then
        echo "Accepted a direct method without an available unfolding at $opt"
        exit 1
    fi
    grep -Fq 'contains no usable Core unfolding' failure
    grep -Fq '$fAlternateMaybe_$calternate' failure

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
