#!/usr/bin/env bash
set -euo pipefail
new=("$1" "-B$2")
old=()
if [[ -n ${ORDERED_GADT_PREVIOUS_GHC:-} ]]; then
    old=("$ORDERED_GADT_PREVIOUS_GHC" "-B${ORDERED_GADT_PREVIOUS_LIBDIR:-$2}")
fi
flags=(-v0 -package-env - -i -i. -O0 -dcore-lint -dstg-lint -fomit-interface-pragmas
       -fno-write-if-simplified-core -outputdir .)
marker=extended-ordered-gadt-telescope
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

# CPP records the installation path of ghcversion.h in the interface.
# Use plain Haskell for the byte comparison between separate installations.
cat >"$out/header" <<'SOURCE'
{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module Provider where

SOURCE

cat >"$out/LEADING" <<'SOURCE'
data T where
    C :: forall a -> Eq a => forall {b} c. Show c => a -> !(b, c) -> T
SOURCE

cat >"$out/EXISTENTIAL" <<'SOURCE'
data T where
    C :: forall a. Int -> Eq a => a -> T
SOURCE

cat >"$out/FIELDLESS" <<'SOURCE'
data T a b where
    C :: forall a. Eq a => forall b. T a b
SOURCE

cat >"$out/CONTROL" <<'SOURCE'
data Tag a where
    Tag :: forall a -> Tag a

data Interior a where
    Interior :: forall a. Tag a -> (a ~ Int) => Tag a -> Interior a

data T where
    C :: Int -> forall {b} c. Show c => (b, c) -> T
SOURCE

cat >"$out/Read.hs" <<'SOURCE'
{-# LANGUAGE TemplateHaskell #-}
module Read where

import Provider
import qualified Language.Haskell.TH as TH

$(TH.reify ''T >> pure [])
SOURCE

for compression in 0 1 2; do
    for shape in LEADING EXISTENTIAL FIELDLESS CONTROL; do
        work="$out/$compression/$shape"
        mkdir -p "$work/"{new,read}
        cat "$out/header" "$out/$shape" > "$work/Provider.hs"
        (
            cd "$work/new"
            "${new[@]}" "${flags[@]}" "-fwrite-if-compression=$compression" -c "$work/Provider.hs"
            "${new[@]}" --show-iface Provider.hi > iface
            if [[ "$shape" == CONTROL ]]; then
                if grep -Fq "$marker" iface; then
                    echo "Unexpected extended-order version in the control interface" >&2
                    exit 1
                fi
            else
                grep -Fq "$marker" iface
            fi
            if [[ "$shape" == FIELDLESS ]]; then
                grep -Fq 'fieldless-ordered-gadt-telescope' iface
            elif grep -Fq 'fieldless-ordered-gadt-telescope' iface; then
                echo "Unexpected fieldless-order version in $shape interface" >&2
                exit 1
            fi
        )
        cp "$work/new/Provider.hi" "$work/read/"
        (
            cd "$work/read"
            "${new[@]}" "${flags[@]}" -fexternal-interpreter -c "$out/Read.hs"
        )

        if (( ${#old[@]} )); then
            mkdir "$work/old"
            (
                cd "$work/old"
                if [[ "$shape" == CONTROL ]]; then
                    "${old[@]}" "${flags[@]}" "-fwrite-if-compression=$compression" -c "$work/Provider.hs"
                    cmp Provider.hi "$work/new/Provider.hi"
                    cp "$work/new/Provider.hi" Provider.hi
                    "${old[@]}" "${flags[@]}" -fexternal-interpreter -c "$out/Read.hs"
                else
                    cp "$work/new/Provider.hi" Provider.hi
                    # The earlier reader must reject the version before it evaluates reify.
                    if "${old[@]}" "${flags[@]}" -v3 -fexternal-interpreter -c "$out/Read.hs" > old.log 2>&1; then
                        echo "Earlier reader accepted $shape at compression $compression" >&2
                        exit 1
                    fi
                    if ! grep -Fq 'mismatched interface file versions' old.log ||
                       ! grep -Fq "$marker" old.log ||
                       grep -Eiq 'panic!|internal error|blocked indefinitely|^Starting .*iserv' old.log; then
                        cat old.log >&2
                        exit 1
                    fi
                fi
            )
        fi
    done
done
