#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -Werror=inaccessible-code -Werror=incomplete-patterns -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
src=$PWD

# Compile the provider and read its unfolding without its source in scope.
for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{PackedVecLibrary,NativeIndexedGadtFieldsProvider,NativeNonuniformGadtFieldsProvider}.hs .
    "${ghc[@]}" "-$opt" -c PackedVecLibrary.hs
    if [[ $opt = O2 ]]; then
        "${ghc[@]}" "-$opt" -ddump-stg-from-core -dsuppress-uniques -dsuppress-idinfo \
            -dppr-cols=160 -c NativeIndexedGadtFieldsProvider.hs > provider.stg
        # Keep the original equality token and the two fields only.
        awk '/PackedVecLibrary[.]VCons[[:space:]].*->/ {
                 sub(/^.*PackedVecLibrary[.]VCons[[:space:]]+/, "")
                 sub(/[[:space:]]+->.*$/, "")
                 seen = 1
                 if (split($0, binders, /[[:space:]]+/) != 3) {
                     print
                     bad = 1
                 }
             }
             END { exit (bad || !seen) }' provider.stg
        "${ghc[@]}" --show-iface NativeIndexedGadtFieldsProvider.hi > provider.iface
        grep -Fq 'indexed-constructor-case+indexed-constructor-fields' provider.iface
    else
        "${ghc[@]}" "-$opt" -c NativeIndexedGadtFieldsProvider.hs
    fi
    "${ghc[@]}" "-$opt" -c NativeNonuniformGadtFieldsProvider.hs
    if [[ $opt = O2 ]]; then
        "${ghc[@]}" --show-iface NativeNonuniformGadtFieldsProvider.hi >nonuniform.iface
        grep -Fq 'indexed-diagonal-constructor-case' nonuniform.iface
        grep -Fq 'opaque-ordinary-constructor-case' nonuniform.iface
    fi
    cp ./*.hi "$src/NativeIndexedGadtFields.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c NativeIndexedGadtFields.hs
    "${ghc[@]}" "-$opt" NativeIndexedGadtFields.o \
        ../provider/{PackedVecLibrary,NativeIndexedGadtFieldsProvider,NativeNonuniformGadtFieldsProvider}.o -o run
    test "$(./run)" = $'(True,True,False,True)\n(True,True,True,True,True,True)'
done

cd "$out/O2/provider"
mkdir legacy
"${ghc[@]}" -O2 -DLEGACY_ONLY -fforce-recomp -outputdir legacy -c NativeNonuniformGadtFieldsProvider.hs
"${ghc[@]}" --show-iface legacy/NativeNonuniformGadtFieldsProvider.hi >legacy.iface
grep -Fq 'retained-constructor-case-v1' legacy.iface
if grep -Fq 'opaque-ordinary-constructor-case' legacy.iface; then
    exit 1
fi

# Force readback of the legacy structural tag before and after mutation.
cd "$out/O2/provider/legacy"
mkdir reader
cp "$src/NativeLegacyGadtFieldsMutate.hs" .
"${ghc[@]}" -O0 -package ghc NativeLegacyGadtFieldsMutate.hs -o mutate
cd reader
cp ../NativeNonuniformGadtFieldsProvider.hi .
cat >LegacyReader.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module LegacyReader where
import NativeNonuniformGadtFieldsProvider

observesNext :: Swap Bool Bool -> Bool
observesNext term = case inspectSwap term of
    Stopped -> False
    Advanced -> True
HS
"${ghc[@]}" -O2 -ddump-inlinings -c LegacyReader.hs >valid.log 2>&1
grep -Fq 'Inlining done: NativeNonuniformGadtFieldsProvider.inspectSwap' valid.log
../mutate "$2" ../NativeNonuniformGadtFieldsProvider.hi NativeNonuniformGadtFieldsProvider.hi
if "${ghc[@]}" -O2 -fforce-recomp -c LegacyReader.hs >duplicate.log 2>&1; then
    exit 1
fi
grep -Fq 'Missing or unsupported constructor proof in interface alternative: Next' duplicate.log

cd "$out/O0/provider"
if "${ghc[@]}" -O0 -fforce-recomp -DWRONG_NONUNIFORM -c NativeNonuniformGadtFieldsProvider.hs >negative.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' negative.log ||
   ! grep -Fq 'PApp (parReflCase r)' negative.log; then
    cat negative.log >&2
    exit 1
fi
for wrong in WRONG_HEAD WRONG_SCRUTINEE; do
    if "${ghc[@]}" -O0 -fforce-recomp "-D$wrong" -c NativeIndexedGadtFieldsProvider.hs > negative.log 2>&1 ||
       ! grep -Eq '\[GHC-(05617|83865)\]' negative.log ||
       ! grep -Fq 'ConsSeen @' negative.log; then
        cat negative.log >&2
        exit 1
    fi
done
