#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/data-family-promotion" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
experimental=-XExperimentalDependentTypes
ordinary=-XNoExperimentalDependentTypes
expected=$(printf "(True,False)\n'Tup2 Bool Bool 'True 'False")

refuse() {
    local code=$1
    shift
    if "$@" > diagnostic 2>&1; then
        echo "Expected GHC-$code for $*"
        exit 1
    fi
    grep -Fq "[GHC-$code]" diagnostic || { cat diagnostic; exit 1; }
}

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,promotion,client}
    cd "$out/$opt/provider"
    cp "$src/TupleProvider.hs" .
    "${ghc[@]}" "$opt" "$ordinary" -c TupleProvider.hs
    cp TupleProvider.hi ../promotion/
    cd ../promotion
    cp "$src/TuplePromotion.hs" .
    "${ghc[@]}" "$opt" "$experimental" -c TuplePromotion.hs
    cp {TupleProvider,TuplePromotion}.hi ../client/
    refuse 64578 "${ghc[@]}" "$opt" "$ordinary" -fno-code -c TuplePromotion.hs
    refuse 83865 "${ghc[@]}" "$opt" "$experimental" -DWRONG_KIND -fno-code -c TuplePromotion.hs
    refuse 83865 "${ghc[@]}" "$opt" "$experimental" -DWRONG_REFINEMENT -fno-code -c TuplePromotion.hs
    refuse 83865 "${ghc[@]}" "$opt" "$experimental" -DWRONG_PROOF -fno-code -c TuplePromotion.hs
    refuse 18872 "${ghc[@]}" "$opt" "$experimental" -DWRONG_COERCION -fno-code -c TuplePromotion.hs
    refuse 64578 "${ghc[@]}" "$opt" "$experimental" -DUNSUPPORTED_CONSTRUCTORS -fno-code -c TuplePromotion.hs
    test "$(grep -Fc '[GHC-64578]' diagnostic)" = 2

    cd ../client
    cp "$src/Main.hs" .
    "${ghc[@]}" "$opt" "$ordinary" -c Main.hs
    "${ghc[@]}" Main.o ../promotion/TuplePromotion.o ../provider/TupleProvider.o -o run
    test "$(./run)" = "$expected"

    if [[ -n ${DATA_FAMILY_PROMOTION_PREVIOUS_GHC:-} ]]; then
        previous=("$DATA_FAMILY_PROMOTION_PREVIOUS_GHC" "-B${DATA_FAMILY_PROMOTION_PREVIOUS_LIBDIR:-$2}"
                  -v0 -package-env - -i -i.)
        "${previous[@]}" "$opt" -fforce-recomp -c Main.hs
        "${previous[@]}" Main.o ../promotion/TuplePromotion.o ../provider/TupleProvider.o -o previous-run
        test "$(./previous-run)" = "$expected"
    fi
done
