#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -no-user-package-db -hide-all-packages
     -package base -dcore-lint -dstg-lint -fdiagnostics-color=never)
pkg=("$3" --global-package-db "$2/package.conf.d" --no-user-package-db)
db="$out/package-db"
package="$out/lib"
unit=retained-vector-facade-0.1
base=$("${pkg[@]}" field base id --simple-output)
experimental=$("${pkg[@]}" field ghc-experimental id --simple-output)
archive=$("${ghc[@]}" --info | sed -n 's/^ ,("ar command","\(.*\)")$/\1/p')

mkdir "$out/provider" "$package" "$out/client"
cd "$out/provider"
cp "$src/"{PackedVecLibrary,RetainedCharVector,RetainedCharLength,RetainedCharProjection}.hs .
cat > PublicVector.hs <<'HS'
module PublicVector
    (CharVector, fromChar, storedLength, storedCells, Nat(..), Vec, natToInt, toList) where

import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)
import RetainedCharVector (CharVector(..))
import RetainedCharLength (storedLength)
import RetainedCharProjection (storedCells)

fromChar :: Char -> CharVector
fromChar c = CharVector (S Z) (VCons c VNil)
HS
"${ghc[@]}" -O2 -fwrite-source-equations -this-unit-id "$unit" -package-id "$experimental" \
    --make -no-link PublicVector.hs
cp ./*.hi "$package/"
"$archive" rcs "$package/libHS$unit.a" ./*.o
cat > "$out/package.conf" <<CONFIG
name: retained-vector-facade
version: 0.1
id: $unit
key: $unit
exposed: True
exposed-modules: PublicVector
hidden-modules: PackedVecLibrary RetainedCharVector RetainedCharLength RetainedCharProjection
import-dirs: "$package"
library-dirs: "$package"
hs-libraries: HS$unit
depends: $base $experimental
CONFIG
"${pkg[@]}" init "$db"
if ! "${pkg[@]}" --package-db "$db" register "$out/package.conf" > "$out/register.log" 2>&1; then
    cat "$out/register.log" >&2
    exit 1
fi

# The installed package holds only the archive and interface files.
cd "$out/client"
cat > Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
import PublicVector (CharVector, fromChar, storedLength, storedCells, natToInt, toList)

readCells :: CharVector -> String
readCells p = toList (storedCells p)

main :: IO ()
main =
    let p = fromChar 'r'
    in print (natToInt (storedLength p), readCells p)
HS
cat > Forward.hs <<'HS'
{-# LANGUAGE CPP, DataKinds #-}
#ifdef WRONG
{-# LANGUAGE NoExperimentalDependentTypes #-}
#else
{-# LANGUAGE ExperimentalDependentTypes #-}
#endif
module Forward where

import PublicVector (CharVector, Nat(..), Vec, storedLength, storedCells)

#ifdef WRONG
forward :: CharVector -> Vec Char 'Z
#else
forward :: foreach (p :: CharVector) -> Vec Char (storedLength p)
#endif
forward p = storedCells p
HS
cat > Forge.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Forge where

import PublicVector (CharVector, Nat(..))

forge :: CharVector
forge = CharVector Z undefined
HS
cat > Hidden.hs <<'HS'
module Hidden where
import RetainedCharVector (CharVector(..))
HS

client=("${ghc[@]}" -static -package-db "$db" -package-id "$unit")
for opt in -O0 -O2; do
    "${client[@]}" "$opt" -fforce-recomp Main.hs -o observe
    timeout 15 ./observe > actual
    grep -Fxq '(1,"r")' actual
    "${client[@]}" "$opt" -fforce-recomp -c Forward.hs
done

reject() {
    local log=$1 code=$2
    shift 2
    if "${client[@]}" -fforce-recomp -c "$@" > "$log" 2>&1 ||
       ! grep -Eq "$code" "$log"; then
        cat "$log" >&2
        exit 1
    fi
}
reject wrong.log GHC-83865 -DWRONG Forward.hs
grep -Fq 'Expected: Vec Char Z' wrong.log
reject forge.log GHC-01928 Forge.hs
reject hidden.log GHC-87110 Hidden.hs
grep -Fq 'hidden module' hidden.log
