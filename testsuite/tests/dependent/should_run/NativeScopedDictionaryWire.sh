#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/promoted-class" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
export_flag=-DSCOPED_DICTIONARY_EXPORT

expected() {
    if [[ $1 == true ]]; then
        printf '(True,False)\nTrue\nTrue\n()\n'
    else
        printf '(False,True)\nFalse\nTrue\n()\n'
    fi
}

for opt in -O0 -O2; do
    for compression in 0 1 2; do
        mkdir -p "$out/$opt/$compression/"{provider,client,ordinary}
        cd "$out/$opt/$compression/provider"
        cp "$src/ScopedDictionaryProvider.hs" Provider.hs
        flags=("$opt" "-fwrite-if-compression=$compression" -fforce-recomp)
        "${ghc[@]}" "${flags[@]}" "$export_flag" -c Provider.hs
        mv Provider.hi first.hi
        "${ghc[@]}" "${flags[@]}" "$export_flag" -c Provider.hs
        cmp first.hi Provider.hi
        "${ghc[@]}" --show-iface Provider.hi > iface
        grep -Fq 'retained-features-v1-experimental-scoped-dictionary' iface
        grep -Fq 'chooseTaggedBoth ::' iface

        if [[ -n ${SCOPED_DICTIONARY_PREVIOUS_GHC:-} ]]; then
            # Retain the header and truncate the payload before the name tables.
            head -c 100 Provider.hi > header.hi
            for iface in Provider.hi header.hi; do
                if "$SCOPED_DICTIONARY_PREVIOUS_GHC" "-B${SCOPED_DICTIONARY_PREVIOUS_LIBDIR:-$2}" \
                    --show-iface "$iface" > previous.log 2>&1; then
                    echo "Previous reader accepted scoped evidence at $opt, compression $compression"
                    exit 1
                fi
                grep -Fq 'mismatched interface file versions' previous.log
                grep -Fq 'experimental-scoped-dictionary' previous.log
            done
        fi

        cd ../client
        cp "$src/ScopedDictionaryImport.hs" Main.hs
        cp ../provider/Provider.hi .
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" "$opt" Main.o ../provider/Provider.o -o run
        diff -u <(expected true) <(./run true)
        diff -u <(expected false) <(./run false)
        if ./run force > demand 2>&1; then
            echo "Expected the result to demand the class method"
            exit 1
        fi
        grep -Fq 'the method was forced' demand

        cat > Missing.hs <<'MISSING'
{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}
module Missing where
import Provider
missing :: a -> Bool
missing x = case chooseTagged x of
    Tagged value -> value
MISSING
        if "${ghc[@]}" "$opt" -c Missing.hs > missing 2>&1; then
            echo "Accepted the call without Choose a"
            exit 1
        fi
        grep -Fq 'error: [GHC-83605]' missing
        sed 's/forwardBoth x y = chooseTaggedBoth x y/forwardBoth x y = chooseTaggedBoth y x/' Main.hs > Wrong.hs
        if "${ghc[@]}" "$opt" -c Wrong.hs > wrong 2>&1; then
            echo "Accepted evidence for the wrong operand"
            exit 1
        fi
        grep -Fq 'error: [GHC-83605]' wrong

        cp "$src/ScopedDictionaryClient.hs" Main.hs
        "${ghc[@]}" "$opt" -c Main.hs
        "${ghc[@]}" "$opt" Main.o ../provider/Provider.o -o ordinary-client
        diff -u <(printf 'True\n'; expected true) <(./ordinary-client true)

        cd ../ordinary
        cp ../provider/Provider.hs .
        "${ghc[@]}" "${flags[@]}" -c Provider.hs
        "${ghc[@]}" --show-iface Provider.hi > iface
        if grep -Fq 'experimental-scoped-dictionary' iface; then
            echo "An interface with ordinary exports requested scoped evidence"
            exit 1
        fi
        if [[ -n ${SCOPED_DICTIONARY_PREVIOUS_GHC:-} ]]; then
            "$SCOPED_DICTIONARY_PREVIOUS_GHC" "-B${SCOPED_DICTIONARY_PREVIOUS_LIBDIR:-$2}" \
                --show-iface Provider.hi > previous.iface
            cmp iface previous.iface
        fi
    done
done

mkdir "$out/combined"
cd "$out/combined"
# A second wire feature must remain visible alongside scoped dictionaries.
sed 's/, Tagged(..), chooseTagged, chooseTaggedBoth/, Tagged(..), chooseTagged, chooseTaggedBoth, Package/'     "$src/ScopedDictionaryProvider.hs" > Provider.hs
printf '\ntype Package = exists a | [a]\n' >> Provider.hs
"${ghc[@]}" "$export_flag" -c Provider.hs
"${ghc[@]}" --show-iface Provider.hi > iface
grep -Fq 'retained-features-v1-erased+experimental-scoped-dictionary' iface
grep -Fq 'chooseTaggedBoth ::' iface
grep -Fq 'type Package =' iface

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/ScopedDictionaryProvider.hs" Provider.hs
cp "$src/ScopedDictionaryImport.hs" Main.hs
printf ':main true\n:main false\n:main force\n:quit\n' |
    "${ghc[@]}" --interactive -ignore-dot-ghci -fbyte-code "$export_flag" Main.hs > interactive.out 2> interactive.err
diff -u <(expected true; expected false) interactive.out
grep -Fq 'the method was forced' interactive.err
