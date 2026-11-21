#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/data-family-promotion" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
expected='((True,False),(0,True),(False,True),True)'
feature=retained-data-family-constructor-operand

for opt in -O0 -O2; do
    for compression in 0 1 2; do
        mkdir -p "$out/$opt/$compression/"{provider,client}
        cd "$out/$opt/$compression/provider"
        cp "$src/"{TupleProvider,RetainedTuple,RetainedConstructor}.hs .
        for unit in TupleProvider RetainedTuple RetainedConstructor; do
            "${ghc[@]}" "$opt" "-fwrite-if-compression=$compression" -c "$unit.hs"
            cp "$unit.hi" ../client/
        done
        if [[ $opt == -O2 ]]; then
            "${ghc[@]}" --show-iface RetainedConstructor.hi > iface
            grep -Fq "retained-natural-operand+$feature" iface
            if [[ -n ${DATA_FAMILY_OPERAND_PREVIOUS_GHC:-} ]]; then
                # The version mismatch must be detected before decoding the payload.
                head -c 192 RetainedConstructor.hi > header.hi
                if "$DATA_FAMILY_OPERAND_PREVIOUS_GHC" \
                    "-B${DATA_FAMILY_OPERAND_PREVIOUS_LIBDIR:?}" --show-iface header.hi > old 2>&1; then
                    echo "Previous reader accepted a data family constructor operand"
                    exit 1
                fi
                grep -Fq 'mismatched interface file versions' old
                grep -Fq "$feature" old
            fi
        fi
        if [[ $compression == 0 ]]; then
            if "${ghc[@]}" "$opt" -DWRONG_INDEX -fno-code -c RetainedConstructor.hs > wrong 2>&1; then
                echo "Accepted the wrong data family constructor index"
                exit 1
            fi
            grep -Fq '[GHC-83865]' wrong
            if "${ghc[@]}" "$opt" -DSTRICT_FIELD -fno-code -c RetainedConstructor.hs > strict 2>&1; then
                echo "Accepted a strict constructor as a retained data family operand"
                exit 1
            fi
            grep -Fq '[GHC-83605]' strict
        fi

        cd ../client
        cp "$src/RetainedConstructorClient.hs" Main.hs
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/{TupleProvider,RetainedTuple,RetainedConstructor}.o -o run
        test "$(./run)" = "$expected"
    done
done

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/"{TupleProvider,RetainedTuple,RetainedConstructor}.hs .
cp "$src/RetainedConstructorClient.hs" Main.hs
test "$("${ghc[@]}" --interactive -ignore-dot-ghci -e main Main.hs)" = "$expected"
