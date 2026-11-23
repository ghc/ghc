#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans -fomit-interface-pragmas -fno-write-if-simplified-core)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{owner,projection,client}
    cd "$out/$opt/owner"
    cp "$src/"{PackedVecLibrary,RetainedCharVector,RetainedCharLength}.hs .
    "${ghc[@]}" "$opt" --make -no-link RetainedCharLength.hs
    "${ghc[@]}" --show-iface RetainedCharLength.hi > first.iface
    grep -Fq 'SourceTable T:storedLength' first.iface
    cp ./*.hi "$src/"{RetainedCharProjection,RetainedCharLocal}.hs ../projection/

    cd ../projection
    test ! -e RetainedCharLength.hs
    "${ghc[@]}" "$opt" -c RetainedCharProjection.hs
    "${ghc[@]}" "$opt" -c RetainedCharLocal.hs
    cat > KnownLength.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module KnownLength where
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..))
import RetainedCharLength (storedLength)
import RetainedCharVector (CharVector(..))

knownLength :: storedLength ('CharVector ('S 'Z) ('VCons 'r' 'VNil)) :~: 'S 'Z
knownLength = Refl
HS
    "${ghc[@]}" "$opt" -c KnownLength.hs
    cp ./*.hi "$src/RetainedCharClient.hs" ../client/

    cd ../client
    test ! -e RetainedCharProjection.hs
    cat > Main.hs <<'HS'
module Main where
import RetainedCharClient (observeProjection, observeSeparateProjection)
main :: IO ()
main = do
    observeProjection >>= print
    observeSeparateProjection >>= print
HS
    "${ghc[@]}" "$opt" -c RetainedCharClient.hs
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o RetainedCharClient.o ../projection/{RetainedCharProjection,RetainedCharLocal}.o ../owner/*.o -o observe
    timeout 15 ./observe > actual
    printf '%s\n' '(1,"r",True)' '(1,"r",True)' | cmp - actual

    if [[ "$opt" = -O0 ]]; then
        cd ../projection
        sed -e '1i{-# LANGUAGE DataKinds #-}' -e 's/Vec)/Nat(..), Vec)/' \
            -e "s/(storedLength r)/('S (storedLength r))/" RetainedCharProjection.hs > WrongIndex.hs
        if "${ghc[@]}" "$opt" -fforce-recomp -c WrongIndex.hs > wrong.log 2>&1 ||
           ! grep -Fq 'GHC-05617' wrong.log || ! grep -Fq 'Expected: Vec Char (S ' wrong.log; then
            cat wrong.log >&2
            exit 1
        fi
    fi
done
