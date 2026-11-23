#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{PackedVecLibrary,SiblingColumns}.hs .
    "${ghc[@]}" "$opt" --make -no-link SiblingColumns.hs
    cp ./*.hi "$src/SiblingColumnsClient.hs" ../client/

    cd ../client
    test ! -e SiblingColumns.hs
    test ! -e PackedVecLibrary.hs
    cat > Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
import PackedVecLibrary (Nat(..), Vec(..))
import SiblingColumns (Columns(..))
import SiblingColumnsClient (readPairs)

main :: IO ()
main = print (readPairs columns)
  where
    columns = Columns (S (S Z))
        (VCons 'r' (VCons 's' VNil))
        (VCons True (VCons False VNil))
HS
    "${ghc[@]}" "$opt" -c SiblingColumnsClient.hs
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o SiblingColumnsClient.o ../provider/{PackedVecLibrary,SiblingColumns}.o -o observe
    timeout 15 ./observe > actual
    printf '%s\n' "[('r',True),('s',False)]" | cmp - actual

    cat > WrongPairs.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module WrongPairs where
import PackedVecLibrary (toList, zipVec)
import SiblingColumns (Columns, flags, letters)

readPairs :: Columns -> Columns -> [(Char, Bool)]
readPairs p q = toList (zipVec (letters p) (flags q))
HS
    status=0
    "${ghc[@]}" "$opt" -c WrongPairs.hs > wrong.log 2>&1 || status=$?
    if [[ "$status" != 1 ]] ||
       ! grep -Eq 'GHC-(05617|83865)' wrong.log ||
       ! grep -Fq 'size p' wrong.log ||
       ! grep -Fq 'size q' wrong.log ||
       ! grep -Eiq "second argument of .*zipVec.*flags q" wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
done
