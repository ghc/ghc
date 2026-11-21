#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/data-family-promotion" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
expected='((True,False),(False,True),True)'

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cd "$out/$opt/provider"
    cp "$src/"{TupleProvider,RetainedTuple}.hs .
    for unit in TupleProvider RetainedTuple; do
        "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$unit.hs"
        cp "$unit.hi" ../client/
    done
    if "${ghc[@]}" "$opt" -DWRONG_IDENTITY -fno-code -c RetainedTuple.hs > wrong 2>&1; then
        echo "Accepted equality between distinct retained operands"
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong
    if "${ghc[@]}" "$opt" -DSTUCK_TYPE_FAMILY -fno-code -c RetainedTuple.hs > stuck 2>&1; then
        echo "Accepted a stuck ordinary type family as a retained domain"
        exit 1
    fi
    grep -Fq 'Its declared domain is: Reduce Bool' stuck

    cd ../client
    cp "$src/RetainedClient.hs" Main.hs
    "${ghc[@]}" "$opt" -XNoExperimentalDependentTypes -c Main.hs
    "${ghc[@]}" Main.o ../provider/{TupleProvider,RetainedTuple}.o -o run
    test "$(./run)" = "$expected"

    cp "$src/RetainedStrictClient.hs" StrictMain.hs
    "${ghc[@]}" "$opt" -c StrictMain.hs -o StrictMain.o -ohi StrictMain.hi
    "${ghc[@]}" StrictMain.o ../provider/{TupleProvider,RetainedTuple}.o -o run-strict
    test "$(./run-strict)" = '(True,True,True,True)'
done

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/"{TupleProvider,RetainedTuple}.hs .
cp "$src/RetainedClient.hs" Main.hs
test "$("${ghc[@]}" --interactive -ignore-dot-ghci -e main Main.hs)" = "$expected"
