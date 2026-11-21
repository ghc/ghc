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
        flags=("$opt" "-fwrite-if-compression=$compression" -fforce-recomp -fwrite-ide-info)
        "${ghc[@]}" "${flags[@]}" "$export_flag" -c Provider.hs
        mv Provider.hi first.hi
        "${ghc[@]}" "${flags[@]}" "$export_flag" -c Provider.hs
        cmp first.hi Provider.hi
        test -s Provider.hie
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
        "${ghc[@]}" "$opt" -fwrite-ide-info -c Main.hs
        test -s Main.hie
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
        "${ghc[@]}" "$opt" -DDIRECT_IMPORT -c Main.hs
        "${ghc[@]}" "$opt" Main.o ../provider/Provider.o -o ordinary-client
        diff -u <(printf '(True,False)\nTrue\n'; expected true) <(./ordinary-client true)

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
            if [[ $compression == 0 ]]; then
                mkdir client
                cp Provider.hi "$src/ScopedDictionaryClient.hs" client/
                (
                    cd client
                    previous=("$SCOPED_DICTIONARY_PREVIOUS_GHC" "-B${SCOPED_DICTIONARY_PREVIOUS_LIBDIR:-$2}"
                              -v0 -package-env - -i -i.)
                    "${previous[@]}" "$opt" -c ScopedDictionaryClient.hs
                    "${previous[@]}" "$opt" ScopedDictionaryClient.o ../Provider.o -o previous-client
                    diff -u <(printf 'True\n'; expected true) <(./previous-client true)
                )
            fi
        fi
    done
done

mkdir "$out/combined"
cd "$out/combined"
# Check the R24 base version together with both R22 package markers.
sed -e '1s/LANGUAGE /LANGUAGE TypeFamilies, /' \
    -e 's/, Tagged(..), chooseTagged, chooseTaggedBoth/, Tagged(..), chooseTagged, chooseTaggedBoth, Package, Hidden, Collects(..)/' \
    "$src/ScopedDictionaryProvider.hs" > Provider.hs
cat >> Provider.hs <<'COMBINED'

type Package = exists a | [a]
type Hidden = exists a. Maybe a

class Collects c where
    type Element c

instance Collects Bool where
    type Element Bool = Bool
COMBINED
"${ghc[@]}" "$export_flag" -c Provider.hs
"${ghc[@]}" --show-iface Provider.hi > iface
grep -Fq -- '-constrained-families-v1-retained-features-v1-erased+implicit+experimental-scoped-dictionary' iface
grep -Fq 'chooseTaggedBoth ::' iface
grep -Fq 'type Package =' iface
grep -Fq 'type Hidden =' iface
mkdir client
cp Provider.hi client/
(
    cd client
    cat > Good.hs <<'GOOD'
{-# LANGUAGE NoExperimentalDependentTypes, TypeFamilies #-}
module Good where
import Data.Proxy
import Provider

good :: Proxy (Element Bool)
good = Proxy
GOOD
    "${ghc[@]}" -c Good.hs
    sed -e 's/module Good/module Bad/' -e 's/Element Bool/Element Int/' Good.hs > Bad.hs
    if "${ghc[@]}" -c Bad.hs > bad 2>&1; then
        echo "Accepted an associated family application without Collects Int"
        exit 1
    fi
    grep -Fq 'error: [GHC-39999]' bad
    grep -Fq 'Collects Int' bad
)

mkdir "$out/interactive"
cd "$out/interactive"
cp "$src/ScopedDictionaryProvider.hs" Provider.hs
cp "$src/ScopedDictionaryImport.hs" Main.hs
printf ':main true\n:main false\n:main force\n:quit\n' |
    "${ghc[@]}" --interactive -ignore-dot-ghci -fbyte-code "$export_flag" Main.hs > interactive.out 2> interactive.err
diff -u <(expected true; expected false) interactive.out
grep -Fq 'the method was forced' interactive.err

# An exported method and a marked family share the same direct Given.
for opt in -O0 -O2; do
    mkdir -p "$out/scoped-family/$opt/"{provider,client}
    cd "$out/scoped-family/$opt/provider"
    cp "$src/ScopedFamilyMethodProvider.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c ScopedFamilyMethodProvider.hs
    "${ghc[@]}" --show-iface ScopedFamilyMethodProvider.hi > iface
    grep -Fq 'constrained-families-v1-retained-features-v1-experimental-scoped-dictionary' iface
    cp ScopedFamilyMethodProvider.hi "$src/ScopedFamilyMethodClient.hs" ../client/
    cd ../client
    "${ghc[@]}" "$opt" -c ScopedFamilyMethodClient.hs
    "${ghc[@]}" ScopedFamilyMethodClient.o ../provider/ScopedFamilyMethodProvider.o -o run
    ./run > actual
    printf '%s\n' "((GT,True),(LT,'x'))" "((GT,'q'),LT)" '("True","False")' "(GT,'q')" LT '((GT,5),(LT,5))' 5 | cmp - actual

    cat > Length.hs <<'HS'
{-# LANGUAGE DataKinds #-}
module Length where
import Data.Proxy
import ScopedFamilyMethodProvider
wrong :: Collects c => c -> Proxy 6
wrong x = case measuredEntry x 5 of (_, lengthProxy) -> lengthProxy
wrongProject :: Collects c => c -> Proxy 6
wrongProject x = snd (id (measuredEntry x 5))
HS
    sed 's/module Length/module Five/; s/Proxy 6/Proxy 5/' Length.hs > Five.hs
    "${ghc[@]}" "$opt" -c Five.hs
    if "${ghc[@]}" "$opt" -c Length.hs > length.log 2>&1; then exit 1; fi
    grep -Fq 'Length.hs:6:57: error: [GHC-83865]' length.log
    grep -Fq 'Length.hs:8:18: error: [GHC-83865]' length.log
    grep -Fq 'Expected: Proxy 6' length.log
    grep -Fq 'Actual: Proxy 5' length.log
    cat > Receiver.hs <<'HS'
module Receiver where
import ScopedFamilyMethodProvider
direct = measuredEntry True 5
HS
    if "${ghc[@]}" "$opt" -fno-code -fdiagnostics-show-caret -c Receiver.hs > receiver.log 2>&1; then exit 1; fi
    grep -Fq 'Receiver.hs:3:24: error: [GHC-83605]' receiver.log
    grep -Fq 'Bind the argument to a local variable' receiver.log

    cat > Ambiguous.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
module Ambiguous where
import ScopedFamilyMethodProvider
missingPayload :: forall c a. (Collects c, Show a) =>
                  foreach (x :: c) -> TaggedEntry Int (compareOrigin x)
missingPayload x = inspectPayload x (5 :: Int)
HS
    if "${ghc[@]}" "$opt" -c Ambiguous.hs > ambiguous.log 2>&1; then exit 1; fi
    grep -Fq 'is ambiguous' ambiguous.log
    "${ghc[@]}" "$opt" -XAllowAmbiguousTypes -c Ambiguous.hs

    cat > MissingOwner.hs <<'HS'
{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes, TypeFamilies #-}
module MissingOwner where
import Data.Proxy
import ScopedFamilyMethodProvider
missing :: Proxy (Element Double)
missing = Proxy
HS
    if "${ghc[@]}" "$opt" -c MissingOwner.hs > missing-owner.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-39999]' missing-owner.log

    cat > WrongOwner.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module WrongOwner where
import ScopedFamilyMethodProvider
class OtherClass c where
    otherCompare :: c -> Ordering
wrongOwner :: forall c. OtherClass c
           => foreach (x :: c) -> Element c -> TaggedEntry (Element c) (otherCompare x)
wrongOwner _ _ = error "missing owner"
HS
    if "${ghc[@]}" "$opt" -c WrongOwner.hs > wrong-owner.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-39999]' wrong-owner.log
    grep -Fq 'Collects c' wrong-owner.log

    cat > CrossedIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeFamilies #-}
module CrossedIndex where
import ScopedFamilyMethodProvider
crossed :: forall c. Collects c
        => foreach (x :: c) -> foreach (y :: c) -> Element c -> TaggedEntry (Element c) (compareOrigin x)
crossed x y payload = inspectEntry y payload
HS
    if "${ghc[@]}" "$opt" -c CrossedIndex.hs > crossed-index.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83605]' crossed-index.log
done
