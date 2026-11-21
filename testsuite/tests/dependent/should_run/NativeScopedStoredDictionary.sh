#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/promoted-class/stored-dictionary" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
expected='(True,False)'

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{StoredEvidence,Selector,FoldCell}.hs .
    for name in StoredEvidence Selector FoldCell; do
        "${ghc[@]}" "$opt" "${stripped[@]}" -c "$name.hs"
        cp "$name.hi" ../client/
    done
    for defer in -fno-defer-type-errors -fdefer-type-errors; do
        if "${ghc[@]}" "$opt" "$defer" -DCOMPETING_DICTIONARY -fno-code -c FoldCell.hs > competing 2>&1; then
            echo "Accepted competing dictionaries from two constructor patterns"
            exit 1
        fi
        grep -Fq '[GHC-83605]' competing
        grep -Fq 'Constraint solving for this retained function type is not supported yet' competing
    done
    if "${ghc[@]}" "$opt" -DLINEAR_FIELD -fno-code -c FoldCell.hs > linear 2>&1; then
        echo "Accepted a linear field as an unrestricted scoped selector operand"
        exit 1
    fi
    grep -Fq 'This argument has multiplicity: One' linear

    cd ../client
    cp "$src/Main.hs" .
    "${ghc[@]}" "$opt" "${stripped[@]}" -c Main.hs
    "${ghc[@]}" Main.o ../provider/{StoredEvidence,Selector,FoldCell}.o -o run
    test "$(./run)" = "$expected"
done

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/"{StoredEvidence,Selector,FoldCell,Main}.hs .
test "$("${ghc[@]}" --interactive -ignore-dot-ghci -e main Main.hs)" = "$expected"
