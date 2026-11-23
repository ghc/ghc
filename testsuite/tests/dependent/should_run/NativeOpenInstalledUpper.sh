#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -dcore-lint -dstg-lint)
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{db,interfaces,exporter,client}

source "$src/NativeOpenInstalledInterfaces.sh" "$2" "$3"
cd "$out/exporter"
cp "$src/UnicodeExport.hs" .
"${ghc[@]}" "${db[@]}" -fomit-interface-pragmas -c UnicodeExport.hs
"${ghc[@]}" --show-iface UnicodeExport.hi >exported.iface
for mapping in toUpper toTitle toLower; do
    grep -Fq "GHC.Internal.Unicode.T:$mapping" exported.iface
done
cp UnicodeExport.hi "$out/interfaces/"

cd "$out/client"
cp "$src/NativeOpenInstalledUpper.hs" .
"${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c NativeOpenInstalledUpper.hs
cat >Specialize.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Specialize where
import qualified Data.Char
import Data.Type.Equality ((:~:) (Refl))
import UnicodeExport (Title)

same :: Title 'x' :~: Data.Char.toTitle 'x'
same = Refl
HS
"${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c Specialize.hs

# GHC must leave both the generated family and the enclosing family stuck
# until the caller supplies the character.
cat >WrongTitle.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module WrongTitle where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import UnicodeExport (Title)

type family Classify (c :: Char) :: Bool where
    Classify '\x1c5' = 'True
    Classify _ = 'False

wrong :: Title '\x1c6' :~: '\x1c4'
wrong = Refl

unknown :: Proxy c -> Classify (Title c) :~: 'False
unknown _ = Refl
HS
if "${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c WrongTitle.hs >wrong.log 2>&1 ||
   [[ $(grep -Fc '[GHC-83865]' wrong.log) != 2 ]]; then
    cat wrong.log >&2
    exit 1
fi
# Only the link step uses the original library registration and exporter object.
"${ghc[@]}" NativeOpenInstalledUpper.o ../exporter/UnicodeExport.o -o client
./client
