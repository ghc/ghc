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
grep -Fq 'GHC.Internal.Unicode.T:toUpper' exported.iface
cp UnicodeExport.hi "$out/interfaces/"

cd "$out/client"
cp "$src/NativeOpenInstalledUpper.hs" .
"${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c NativeOpenInstalledUpper.hs
# Only the link step uses the original library registration and exporter object.
"${ghc[@]}" NativeOpenInstalledUpper.o ../exporter/UnicodeExport.o -o client
./client
