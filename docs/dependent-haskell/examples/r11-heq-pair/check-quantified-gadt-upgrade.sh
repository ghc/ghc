#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 4 ]]; then
    echo "Usage: $0 OLD_GHC OLD_LIBDIR NEW_GHC NEW_LIBDIR" >&2
    exit 2
fi
old=("$(realpath "$1")" "-B$(realpath "$2")")
new=("$(realpath "$3")" "-B$(realpath "$4")")
src=$(cd "$(dirname "$0")" && pwd)
client="$src/../../../../testsuite/tests/dependent/should_run/QuantifiedGadtIndexClient.hs"
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
flags=(-v0 -package-env - -i -i. -fdiagnostics-color=never)
mkdir -p "$out"/{warm,reverse}
cd "$out/warm"
cat >Ordinary.hs <<'HS'
module Ordinary where
answer :: Int
answer = 41
HS
"${old[@]}" "${flags[@]}" -O0 -c Ordinary.hs
"${old[@]}" --show-iface Ordinary.hi >old.iface
old_version=$(awk '/^interface Ordinary / {print $NF}' old.iface)
[[ $old_version =~ ^[0-9]+$ ]]

for opt in -O0 -O2; do
    mkdir -p "$out/${opt#-}"/{provider,old-reader,new-reader}
    cd "$out/${opt#-}/provider"
    "${new[@]}" "${flags[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core \
        -dcore-lint -dstg-lint -c "$src/QuantifiedGadtIndex.hs" -outputdir .
    "${new[@]}" --show-iface QuantifiedGadtIndex.hi >new.iface
    new_version=$(awk '/^interface QuantifiedGadtIndex / {print $NF}' new.iface)
    [[ $new_version =~ ^[0-9]+$ && $new_version != "$old_version" ]]
    cd ../old-reader
    cp ../provider/QuantifiedGadtIndex.hi .
    if "${old[@]}" "${flags[@]}" "$opt" -fno-code -c "$client" >rejected 2>&1; then exit 1; fi
    grep -F 'GHC-47808' rejected
    grep -F "mismatched interface file versions (wanted \"$old_version\", got \"$new_version\")" rejected
    cd ../new-reader
    cp ../provider/QuantifiedGadtIndex.hi .
    "${new[@]}" "${flags[@]}" "$opt" -dcore-lint -dstg-lint -c "$client" -outputdir . -o Main.o
    "${new[@]}" "${flags[@]}" Main.o ../provider/QuantifiedGadtIndex.o -o check
    ./check >actual
    grep -Fx '(41,43)' actual
done

cd "$out/reverse"
cp ../warm/Ordinary.hi .
cat >Use.hs <<'HS'
module Use where
import Ordinary
result :: Int
result = answer
HS
if "${new[@]}" "${flags[@]}" -fno-code -c Use.hs >rejected 2>&1; then exit 1; fi
grep -F 'GHC-47808' rejected
grep -F "mismatched interface file versions (wanted \"$new_version\", got \"$old_version\")" rejected
cd "$out/warm"
"${new[@]}" "${flags[@]}" -O0 -v1 --make -no-link Ordinary.hs >upgrade.log 2>&1
grep -F 'Compiling Ordinary' upgrade.log
cp Ordinary.hi upgraded.hi
"${new[@]}" "${flags[@]}" -O0 -v1 --make -no-link Ordinary.hs >unchanged.log 2>&1
if grep -Fq 'Compiling Ordinary' unchanged.log; then exit 1; fi
cmp Ordinary.hi upgraded.hi
