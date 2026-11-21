#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -O1 -i -i. -package-env - -dcore-lint -dstg-lint)
cd "$out"
mkdir client
provider=NativeConstrainedFamilyRulesProvider
cp "$src"/{PackedVecLibrary,ParameterizedIndexLibrary,FinitePosition}.hs "$src/$provider.hs" .
"${ghc[@]}" -fwrite-source-equations -c PackedVecLibrary.hs
"${ghc[@]}" -c ParameterizedIndexLibrary.hs FinitePosition.hs "$provider.hs"
"${ghc[@]}" --show-iface "$provider.hi" > provider.iface
grep -Eq 'constrained-families-v1.*retained-user-unfolding.*linear-unpack' provider.iface

if [[ $# -ge 3 ]]; then
    old=("$3" "-B${4:-$2}")
    printf 'module Plain where\nvalue :: Bool\nvalue = True\n' > Plain.hs
    "${ghc[@]}" -c Plain.hs
    "${old[@]}" --show-iface Plain.hi > old-plain.log
    if "${old[@]}" --show-iface "$provider.hi" > old-provider.log 2>&1; then
        echo 'Previous reader accepted the constrained family header'
        exit 1
    fi
    grep -Fq 'mismatched interface file versions' old-provider.log
    grep -Fq 'constrained-families-v1' old-provider.log
fi

cp ./*.hi client/
cd client
cp "$src/NativeConstrainedFamilyRulesClient.hs" Main.hs
"${ghc[@]}" -ddump-rule-firings -ddump-inlinings -c Main.hs > optimisations.log
grep -Fq "Inlining done: $provider.zero" optimisations.log
test "$(grep -Fc "Rule fired: linear/unpack ($provider)" optimisations.log)" = 2
"${ghc[@]}" Main.o ../*.o -o main
test "$(./main)" = '(101,102)'

sed -e 's/module Main where/module Bad where/' -e 's/Elem \[Bool\] :~: Bool/Elem [Int] :~: Int/' Main.hs > Bad.hs
if "${ghc[@]}" -fforce-recomp -fdiagnostics-color=never -fdefer-type-errors -c Bad.hs > bad.log 2>&1; then
    echo 'Accepted a family application outside its domain'
    exit 1
fi
grep -Fq '[GHC-39999]' bad.log
